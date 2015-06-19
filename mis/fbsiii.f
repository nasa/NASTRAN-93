      SUBROUTINE FBSIII (SCR,Y,X,NWDS)        
C        
C     COMPLEX SINGLE-PRECISION VERSION        
C        
C     FBSIII EXECUTES THE FORWARD/BACKWARD PASS FOR FBS MODULE. THIS    
C     ROUTINE IS USED ONLY WHEN PASSES TO FBS3 ROUTINE IS EXCESSIVE.    
C        
C     THIS ROUTINE DOES NOT USE THE STRING SUBROUTINE CALLS.        
C     THE INNER LOOPS HERE ARE ALREADY GOOD FOR VECTORIZATION        
C        
C     BEFORE CALLING THIS SUBROUTINE, THE CALLER MUST FIRST CALL UNPSCR 
C     (WITH FLAG=2) TO MOVE THE DATA OF THE LOWER TRIANGULAR FACTOR TO  
C     A SPECIAL SCRATCH FILE SCR, AND OPEN THIS SCRATCH FILE FOR READ.  
C     THIS ROUTINE DOES NOT OPEN NOR CLOSE ANY FILE, NOR REWIND, NOR    
C     FORWARD SKIP OF HEADER RECORD.        
C        
C     WRITTEN BY G.CHAN/UNISYS, 11/1991.   LAST REVISED 3/1993        
C        
      LOGICAL          DEBUG        
      INTEGER          SUBNAM(5),SCR,SYSBUF,BASE        
      REAL             Y(1),X(1)        
      CHARACTER        UFM*23,UWM*25,UIM*29,SFM*25        
      COMMON /XMSSG /  UFM,UWM,UIM,SFM        
      COMMON /SYSTEM/  SYSBUF,NOUT        
      COMMON /FBSX  /  DUMMY(28),LCORE        
      EQUIVALENCE      (II,XI),(JJ,XJ), (SUMR,YKR),(SUMI,YKI),        
     1                 (N,DUMMY(2))        
      DATA    SUBNAM/  4HFBSI, 2HII, 2*4HBEGN, 4HEND /, EPSI /1.0E-35 / 
      DATA    DEBUG /  .FALSE. /        
C        
C     Y ARRAY IS HOLDING A NUMBER OF LOAD VECTORS        
C     X ARRAY HOLDS ALSO A NUMBER OF ROWS OF LOWER TRIANGULR MATRIX     
C       COMING FROM THE SCRATCH FILE SCR        
C     NWDS = LENGTH OF A COLUMN LOAD VECTOR (COMPLEX S.P.)        
C        
      SUBNAM(3) = SUBNAM(4)        
      CALL CONMSG (SUBNAM,3,0)        
      NBRITM = NWDS        
      J    = (LOCFX(X)-LOCFX(Y))/NWDS        
      LAST = MAX0(J,1)*NBRITM        
      NET  = LCORE - SYSBUF - J*NWDS        
      IF (DEBUG) WRITE (NOUT,10) NBRITM,J,LAST,NWDS,NET        
   10 FORMAT ('0...FBSIII DEBUG, NBRITM,J,LAST,NWDS,NET =',4I6,I10)     
C        
C     FORWARD SUBSTITUTION PASS        
C        
      IFB  = +1        
      NREC = 0        
      LL   = 0        
      BASE = 2        
      DO 110 J = 1,N        
      IF (BASE .LT. LL) GO TO 40        
      NREC = NREC + 1        
      CALL READ (*200,*20,SCR,X,NET,1,LL)        
      CALL MESAGE (-8,0,SUBNAM)        
   20 IF (DEBUG) WRITE (NOUT,30) LL,NREC,LL,J,IFB        
   30 FORMAT ('  FBSIII DEBUG /@20',I10,' WORDS READ FROM RECORD NO.',  
     1       I5,',  LL,J,IFB = ',I9,2I6)        
      BASE = 2        
      XI   = X(BASE-1)        
      IF (II-J) 200,50,200        
   40 XI   = X(BASE-1)        
   50 XJ   = X(BASE  )        
      NTMS = (JJ-II+1)*2        
      IB   = BASE + 3        
      IE   = BASE + NTMS        
C     IF (DEBUG) WRITE (NOUT,60) BASE,II,JJ,NTMS,IB,IE,J        
C  60 FORMAT ('  @60   BASE,II,JJ,NTMS,IB,IE,J =',I10,6I5)        
      BASE = IE + 2        
      DIAR = X(IB-2)        
      DIAI = X(IB-1)        
      SSQR = 1.0/(DIAR**2 + DIAI**2)        
      J2   = J*2 - 1        
      IF (NTMS .LE. 2) GO TO 90        
C        
C     PROCESS CURRENT NON-ZERO TERMS IN TRIANGULAR FACTOR AGAINST EACH  
C     LOAD VECTOR IN CORE. AT END OF EACH COLUMN, DIVIDE BY DIAGONAL    
C        
      DO 80 K = J2,LAST,NBRITM        
      YKR = Y(K  )        
      YKI = Y(K+1)        
      IF (ABS(YKR).LT.EPSI .AND. ABS(YKI).LT.EPSI) GO TO 80        
      IK  = K        
      DO 70 I = IB,IE,2        
      IK  = IK + 2        
      Y(IK  ) = Y(IK  ) + X(I)*YKR - X(I+1)*YKI        
      Y(IK+1) = Y(IK+1) + X(I)*YKI + X(I+1)*YKR        
   70 CONTINUE        
      Y(K  )  = (YKR*DIAR + YKI*DIAI)*SSQR        
      Y(K+1)  =-(YKR*DIAI - YKI*DIAR)*SSQR        
   80 CONTINUE        
      GO TO 110        
C        
   90 DO 100 K = J2,LAST,NBRITM        
      Y(K  )  = (YKR*DIAR + YKI*DIAI)*SSQR        
      Y(K+1)  =-(YKR*DIAI - YKI*DIAR)*SSQR        
  100 CONTINUE        
C        
  110 CONTINUE        
C        
C     END OF FORWARD SUBSTITUTION        
C        
C        
C     BACKWARD SUBSTITUTION PASS        
C        
C     SKIP LAST COLUMN COMPUTATION IN BACKWARD PASS        
C     FIRST DIAGONAL TERMS WILL NOT BE INCLUDED IN COMPUTATION        
C        
      IF (N .EQ. 1) GO TO 220        
      IFB  = -1        
      LL   = 0        
      BASE = 2        
      J    = N        
      DO 180 JX = 1,N        
      IF (BASE .LT. LL) GO TO 130        
      NREC = NREC + 1        
      CALL READ (*200,*120,SCR,X,NET,1,LL)        
      CALL MESAGE (-8,0,SUBNAM)        
  120 IF (DEBUG) WRITE (NOUT,30) LL,NREC,LL,J,IFB        
      BASE = 2        
      XI   = X(BASE-1)        
      IF (II .NE. J) GO TO 200        
      IF (J  .NE. N) GO TO 140        
      XJ   = X(BASE)        
      BASE = BASE + (JJ-II+1)*2 + 2        
      GO TO 180        
  130 XI   = X(BASE-1)        
  140 XJ   = X(BASE  )        
      NTMS = (JJ-II+1)*2        
      IB   = BASE + 3        
      IE   = BASE + NTMS        
C     IF (DEBUG) WRITE (NOUT,150) BASE,II,JJ,NTMS,IB,IE,J        
C 150 FORMAT ('  '@150  BASE,II,JJ,NTMS,IB,IE,J =',I10,6I5)        
      BASE = IE + 2        
      IF (NTMS .LE. 2) GO TO 180        
C        
C     PROCESS NON-ZERO TERMS OF TRIANGULAR FACTOR AGAINST EACH LOAD     
C     VECTOR IN CORE        
C        
      J2   = J*2 - 1        
      DO 170 K = J2,LAST,NBRITM        
      SUMR = 0.0        
      SUMI = 0.0        
      IK   = K        
      DO 160 I = IB,IE,2        
      IK   = IK + 2        
      SUMR = SUMR + X(I)*Y(IK  ) - X(I+1)*Y(IK+1)        
      SUMI = SUMI + X(I)*Y(IK+1) + X(I+1)*Y(IK  )        
  160 CONTINUE        
      Y(K  ) = Y(K  ) + SUMR        
      Y(K+1) = Y(K+1) + SUMI        
  170 CONTINUE        
C        
  180 J = J - 1        
      GO TO 220        
C        
C     END BACKWARD SUBSTITUTION        
C        
C        
C     FATAL ERROR MESSAGES        
C        
  200 WRITE  (NOUT,210) SFM,J,II,NREC,N,BASE,LL,IFB        
  210 FORMAT (A25,' 2149, SUBROUTINE FBSIII', /5X,'FIRST ELEMENT OF A ',
     1       'COLUMN OF LOWER TRIANGULAR MATRIX IS NOT THE DIAGONAL ',  
     2       'ELEMENT.', /5X,'J,II,NREC,N,BASE,LL,IFB =',4I6,2I10,I4)   
      CALL MESAGE (-61,SCR,SUBNAM)        
C        
  220 SUBNAM(3) = SUBNAM(5)        
      CALL CONMSG (SUBNAM,3,0)        
      RETURN        
      END        
