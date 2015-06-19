      SUBROUTINE FBS1 (BLOCK,Y,YN,NWDS)        
C        
C     FBS1 EXECUTES THE FORWARD/BACKWARD PASS FOR FBS IN RSP        
C        
      INTEGER         BLOCK(8), DBL, BUF(2), SUBNAM, BEGN, END        
      REAL            Y(1), YN(1), LJJ, L, SUM        
      CHARACTER       UFM*23, UWM*25, UIM*29, SFM*25        
      COMMON /XMSSG / UFM, UWM, UIM, SFM        
      COMMON /MACHIN/ MACH        
      COMMON /SYSTEM/ SYSBUF, NOUT        
CZZ   COMMON /XNSTRN/ L(1)        
      COMMON /ZZZZZZ/ L(1)        
      COMMON /FBSX  / DBL   , N        
      DATA    MONE  / -1 /        
      DATA    SUBNAM, BEGN, END / 4HFBS1, 4HBEGN, 4HEND /        
C        
      BUF(1) = SUBNAM        
      BUF(2) = BEGN        
      CALL CONMSG (BUF,2,0)        
      NBRITM = NWDS        
      J    = (LOCFX(YN)-LOCFX(Y)+1)/NWDS        
      LAST = MAX0(J,1)*NBRITM        
      DO 35 J = 1,N        
      J1 = J -1        
      DO 5 K = J,LAST,NBRITM        
      IF (Y(K) .NE. 0.0) GO TO 7        
    5 CONTINUE        
      CALL SKPREC (BLOCK(1),1)        
      GO TO 35        
C        
C     MAKE 1ST STRING CALL FOR COLUMN AND SAVE DIAGONAL ELEMENT        
C        
    7 BLOCK(8) = -1        
      CALL GETSTR (*80,BLOCK)        
      IF (BLOCK(4) .NE. J) GO TO 80        
      JSTR = BLOCK(5)        
      LJJ  = 1.0/L(JSTR)        
      IF (BLOCK(6) .EQ. 1) GO TO 20        
      NSTR = JSTR + BLOCK(6) - 1        
      JSTR = JSTR + 1        
      BLOCK(4) = BLOCK(4) + 1        
C        
C     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH        
C     LOAD VECTOR IN CORE -- Y(I,K) = Y(I,K) + L(I,J)*Y(J,K)        
C        
   10 DO 15 K = 1,LAST,NBRITM        
      YJK = Y(J1+K)        
      IF (YJK .EQ. 0.0) GO TO 15        
      IK  = BLOCK(4) + K - 1        
      DO 12 IJ = JSTR,NSTR        
      Y(IK) = Y(IK) + L(IJ)*YJK        
   12 IK = IK + 1        
   15 CONTINUE        
C        
C     GET NEXT STRING IN TRIANGULAR FACTOR        
C        
   20 CALL ENDGET (BLOCK)        
      CALL GETSTR (*30,BLOCK)        
      JSTR = BLOCK(5)        
      NSTR = JSTR + BLOCK(6) - 1        
      GO TO 10        
C        
C     END-OF-COLUMN ON TRIANGULAR FACTOR -- DIVIDE BY DIAGONAL        
C        
   30 DO 32 K = J,LAST,NBRITM        
   32 Y(K) = Y(K)*LJJ        
C        
   35 CONTINUE        
C        
C     INITIALIZE FOR BACKWARD PASS BY SKIPPING THE NTH COLUMN        
C        
      IF (N .EQ. 1) GO TO 65        
      CALL BCKREC (BLOCK)        
      J = N - 1        
C        
C     GET A STRING IN CURRENT COLUMN. IF STRING INCLUDES DIAGONAL,      
C     ADJUST STRING TO SKIP IT.        
C        
   40 J1 = J - 1        
      BLOCK(8) = -1        
   42 CALL GETSTB (*60,BLOCK)        
      IF (BLOCK(4)-BLOCK(6) .EQ. J1) BLOCK(6) = BLOCK(6) - 1        
      IF (BLOCK(6) .EQ. 0) GO TO 58        
      NTERMS = BLOCK(6)        
C        
C     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH        
C     LOAD VECTOR IN CORE -- Y(J,K) = Y(J,K) + L(J,I)*Y(I,K)        
C        
C     CODE 50 IS GOOD FOR ALL MACHINES.  G.CHAN/UNISYS   9/1991        
C        
C     THE FOLLOWING CODE IS USED ON UNIVAC ONLY BECAUSE OF A RESTRICTION
C     ON A NEGATIVE INCREMENT OF A DO LOOP ON CDC AND IBM COMPUTERS.    
C        
C     IF (MACH .NE. 3) GO TO 50        
C     LST = LAST + J1        
C     DO 47 JK = J,LST,NBRITM        
C     IK  = BLOCK(4) + JK - J        
C     JI  = BLOCK(5) - IK        
C     LIM = IK - NTERMS + 1        
C     SUM = 0.0        
C     DO 45 II = IK,LIM,MONE        
C     SUM = SUM + L(JI+II)*Y(II)        
C  45 CONTINUE        
C     Y(JK) = Y(JK) + SUM        
C  47 CONTINUE        
C     GO TO 58        
C        
C  50 CONTINUE        
      DO 55 K = 1,LAST,NBRITM        
      JI  = BLOCK(5) + 1        
      IK  = BLOCK(4) + K        
      SUM = 0.0        
      DO 53 II = 1,NTERMS        
      JI  = JI - 1        
      IK  = IK - 1        
      SUM = SUM + L(JI)*Y(IK)        
   53 CONTINUE        
      Y(J1+K) = Y(J1+K) + SUM        
   55 CONTINUE        
C        
C     TERMINATE CURRENT STRING AND GET NEXT STRING        
C        
   58 CALL ENDGTB (BLOCK)        
      GO TO 42        
C        
C     END-OF-COLUMN -- TEST FOR COMPLETION        
C        
   60 IF (J .NE. 1) GO TO 70        
   65 BUF(2) = END        
      CALL CONMSG (BUF,2,0)        
      RETURN        
C        
   70 J = J - 1        
      GO TO 40        
C        
C     FATAL ERROR MESSAGE        
C        
   80 WRITE  (NOUT,82) SFM,SUBNAM        
   82 FORMAT (A25,' 2149, SUBROUTINE ',A4,/5X,'FIRST ELEMENT OF A COLU',
     1      'MN OF LOWER TRIANGULAR MATRIX IS NOT THE DIAGONAL ELEMENT')
      CALL MESAGE (-61,0,0)        
      RETURN        
      END        
