      SUBROUTINE TRD1C(IC,PD,NGROUP,NLFTP,UDV,ILOOP,SCR1,DIT,NLFT,NOUE, 
     1                 MODAL,PNL)        
C        
C     THIS ROUTINE STEPS INTEGRATION PROCEDURE        
C        
C     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION        
C        
      LOGICAL NOPD        
C        
      INTEGER DIT1,PNL1,PNL        
      INTEGER PD,UDV,SCR1,DIT,SYSBUF,FILE,IZ(1),MCB(7),IPNL(7)        
      INTEGER SUBNAM(2)        
C        
      COMMON /BLANK /DUMMY(4), NCOL        
      COMMON /SYSTEM/SYSBUF        
CZZ   COMMON /ZZTRDC/Z(1)        
      COMMON /ZZZZZZ/Z(1)        
      COMMON /PACKX /IT1,IT2,II,JJ,INCR        
      COMMON /TRDXX /IK(7),IDUM(14),ISCR1,ISCR2,ISCR3,ISCR4,ISCR5,ISCR6,
     1               IOPEN,ISYM,TO,NOPD,ISPNL        
      COMMON /UNPAKX/IT3,III,JJJ,INCR1        
      COMMON /TRDD1 /NLFT1,DIT1,NLFTP1,NOUT,ICOUNT,ILOOP1,MODAL1,NZ,    
     1               ICORE,IU2,IP4,IPNL,NMODES,NSTEP,PNL1,IST,IU1,      
     2               DELTAT,IFRST        
C        
      EQUIVALENCE    (Z(1),IZ(1))        
C        
      DATA   SUBNAM /4HTRD1,1HC/        
C        
C ----------------------------------------------------------------------
C        
C     INITIALIZE        
C        
      NROW  = IK(3)        
      IT1   = 1        
      IT2   = 1        
      II    = 1        
      JJ    = NROW        
      INCR  = 1        
      IT3   = 1        
      III   = 1        
      JJJ   = NROW        
      INCR1 = 1        
      NZ    = KORSZ(Z)        
      IGROUP= NZ -3*NGROUP +1        
      IBUF1 = IGROUP -SYSBUF        
      IBUF2 = IBUF1 -SYSBUF        
      IBUF3 = IBUF2 -SYSBUF        
      IBUF4 = IBUF3-SYSBUF        
      IBUF5 = IBUF4-SYSBUF        
      IBUF6 = IBUF5-SYSBUF        
      IBUF7 = IBUF6-SYSBUF        
      IBUF8 = IBUF7 -SYSBUF        
      NZ    = IBUF7-1        
      IF(NLFTP .NE. 0) NZ = IBUF8-1        
      IOPEN = 0        
      ICRQ  = 7*NROW - NZ        
      IF(ICRQ.GT.0) GO TO 430        
      IU1   = 0        
      IU2   = IU1+NROW        
      IU3   = IU2+ NROW        
      IP1   = IU3+ NROW        
      IP2   = IP1+ NROW        
      IP3   = IP2+ NROW        
      IP4   = IP3+ NROW        
      NLFT1 = NLFT        
      DIT1  = DIT        
      NLFTP1= NLFTP        
      ILOOP1= ILOOP        
      MODAL1= MODAL        
      IST   = 0        
      NZ    = NZ - 7*NROW        
      ICORE = IP4 +NROW        
      NMODES= NROW- NOUE        
      PNL1  = PNL        
      ASSIGN 60 TO IRET1        
      NSTEP = IZ(IGROUP) + 1        
      DELTAT= Z(IGROUP+1)        
      NOUT  = IZ(IGROUP+2)        
      IF( ILOOP .NE. 1) GO TO 210        
C        
C     FIRST ENTRY INITIALIZE STUFF        
C        
      IST  =-1        
      FILE = PD        
C        
C     PUT P0 IN IP2        
C        
      IPNT = IP2        
      NOPD = .TRUE.        
      ASSIGN 5 TO IRETN        
      CALL OPEN(*310,PD,IZ(IBUF2),0)        
      CALL SKPREC(PD,1)        
      NOPD = .FALSE.        
      GO TO 290        
    5 FILE = UDV        
      IAPEND = 0        
      IF (NCOL.LE.0) GO TO 8        
      MCB(1) = UDV        
      CALL RDTRL(MCB)        
      IF(MCB(2) .NE. 0) GO TO 330        
    8 CALL GOPEN (UDV, IZ(IBUF3), 1)        
      CALL MAKMCB(MCB,UDV,NROW,2,1)        
   10 IF (NLFTP.EQ.0) GO TO 20        
C        
C     CHECK TO SEE IF PNL HAS BEEN PRE-PURGED.        
C        
      IPNL(1)= PNL1        
      CALL RDTRL(IPNL)        
      ISPNL= 0        
      IF(IPNL(1) .LE. 0) GO TO 20        
      ISPNL= 1        
      CALL GOPEN(PNL1,IZ(IBUF8),1)        
      CALL MAKMCB(IPNL,PNL1,NROW,2,1)        
   20 CONTINUE        
      IF(IAPEND .EQ. 1) GO TO 50        
      FILE = IC        
      CALL GOPEN(IC,IZ(IBUF1),0)        
      ASSIGN 30 TO IRETN        
      IPNT = IU2        
      GO TO 290        
   30 ASSIGN 40 TO IRETN        
      IPNT = IU3        
      GO TO 290        
   40 CALL CLOSE(IC,1)        
      NSTEP = IZ(IGROUP)+1        
      DELTAT= Z(IGROUP+1)        
      NOUT  = IZ(IGROUP+2)        
C        
C     FORM  U=1, PO, P-1        
C        
      CALL FORM1( Z(IU2+1),Z(IU3+1),Z(IU1+1),Z(IP2+1),Z(IP1+1),DELTAT,  
     1  Z(IBUF1))        
C        
C     START TIME STEP COUNT        
C        
   50 CONTINUE        
      ICOUNT = 1        
   60 CONTINUE        
      IF (NLFTP .EQ. 0) GO TO 62        
      IFRST=0        
      CALL TRD1D        
      IFRST=1        
   62 CONTINUE        
C        
C     OPEN FBS FILES        
C        
      FILE = ISCR1        
      CALL OPEN(*390,ISCR1,IZ(IBUF4),0)        
      FILE = ISCR2        
      CALL OPEN(*390,ISCR2,IZ(IBUF5),0)        
      FILE = ISCR3        
      CALL OPEN(*390,ISCR3,IZ(IBUF6),0)        
      FILE = ISCR4        
      CALL OPEN(*390,ISCR4,IZ(IBUF7),0)        
C        
C     ZERO P*        
C        
   70 CALL TMTOGO(ITLEFT)        
      IF(ITLEFT .LE. 0) GO TO 170        
      DO 80 I = 1,NROW        
      K = IP4 +I        
      Z(K) =0.0        
   80 CONTINUE        
      IF(NLFTP .EQ. 0) GO TO 90        
C        
C     FORM NON-LINEAR LOADS        
C        
      CALL TRD1D        
      IF(ICOUNT.EQ. 1 .OR. ICOUNT .EQ. NSTEP .OR. MOD(ICOUNT+IST,NOUT)  
     1   .EQ. 0) GO TO 85        
      GO TO 90        
   85 IF (ISPNL.GT.0) CALL PACK (Z(IP4+1), PNL, IPNL)        
C        
C     BRING IN NEXT P        
C        
   90 IPNT = IP3        
      FILE = PD        
      ASSIGN 100 TO IRETN        
      IF ( NOPD ) GO TO 310        
      GO TO 290        
C        
C     ADD P-S TO FORM P*        
C        
  100 DO 110 I=1,NROW        
      K = IP4 + I        
      L = IP1 + I        
      M = IP2 + I        
      J = IP3 + I        
      Z(K) = Z(K) +(Z(L) + Z(M) + Z(J))/3.0        
  110 CONTINUE        
      IF (ILOOP.NE.1.OR.ICOUNT.NE.1) GO TO 115        
      IF (IAPEND.EQ.1) GO TO 115        
C        
C     OUTPUT INITIAL DISPLACEMENT        
C        
      CALL PACK (Z(IU2 + 1), UDV, MCB(1))        
C        
C     OUTPUT INITIAL VELOCITY        
C        
      CALL PACK (Z(IU3 + 1), UDV, MCB(1))        
C        
C     SOLVE FOR NEXT SOLUTION        
C        
  115 CALL STEP  (Z(IU3 + 1), Z(IU2 + 1), Z(IU1 + 1), Z(IP4 + 1),       
     1            IZ(IBUF1))        
      IF (ILOOP.EQ.1.AND.ICOUNT.EQ.1) GO TO 145        
      IF (ICOUNT.EQ.NSTEP.OR.MOD(ICOUNT+IST, NOUT).EQ.0) GO TO 130      
      IF (ICOUNT.EQ.1) GO TO 130        
C        
C     ROTATE P POINTERS        
C        
  120 J  = IP1        
      IP1= IP2        
      IP2= IP3        
      IP3= J        
C        
C     ROTATE U POINTERS        
C        
      J  = IU1        
      IU1= IU2        
      IU2= IU3        
      IU3= J        
      ICOUNT = ICOUNT +1        
      IF(ICOUNT-NSTEP) 70,160,170        
C        
C     IT-S OUTPUT TIME -- LUCKY FELLOW        
C        
  130 CALL PACK( Z(IU2+1), UDV, MCB(1) )        
C        
C     COMPUTE U DOT        
C        
      H = 1.0/(2.0*DELTAT)        
      DO 140 I=1,NROW        
      K = IP4 +I        
      L = IU3+I        
      M = IU1 + I        
      Z(K) = (Z(L)-Z(M))*H        
  140 CONTINUE        
      CALL PACK( Z(IP4+1), UDV, MCB(1) )        
C        
C     COMPUTE U DOT DOT        
C        
  145 H = 1.0/(DELTAT*DELTAT)        
      DO 150 I=1,NROW        
      K = IP4+I        
      L = IU3+I        
      M = IU1+I        
      J = IU2 +I        
      Z(K) = (Z(L)+Z(M)- 2.0*Z(J))*H        
  150 CONTINUE        
      CALL PACK( Z(IP4+1), UDV, MCB(1) )        
      GO TO 120        
C        
C     END OF 1 GROUP        
C        
  160 IF(ILOOP .NE. NGROUP) GO TO 200        
      GO TO 70        
  170 J = 1        
  180 CALL CLOSE(UDV,J)        
      CALL CLOSE(PD, J)        
      CALL CLOSE(ISCR1,1)        
      CALL CLOSE(ISCR2,1)        
      CALL CLOSE(ISCR3,1)        
      CALL CLOSE(ISCR4,1)        
      CALL WRTTRL(MCB)        
      IF( NLFTP .EQ. 0) GO TO 190        
      IF (ISPNL.EQ.0) GO TO 190        
      CALL CLOSE(PNL,J)        
      CALL WRTTRL(IPNL)        
  190 RETURN        
C        
C     MORE GROUPS TO COME SAVE STUFF        
C        
  200 J = 2        
      FILE = SCR1        
      CALL OPEN(*390,SCR1,IZ(IBUF1),1)        
      CALL WRITE(SCR1,Z(IU3+1),NROW,1)        
      CALL WRITE(SCR1,Z(IU1+1),NROW,1)        
      CALL WRITE(SCR1,Z(IU2+1),NROW,1)        
      CALL WRITE(SCR1,Z(IP1+1),NROW,1)        
      CALL CLOSE(SCR1,1)        
      GO TO 180        
C        
C     CHANGE OF TIME STEP--RESTORE POINTERS ETC        
C        
  210 IGROUP = IGROUP +(ILOOP-1)*3        
      DELTA1 = Z(IGROUP-2)        
      NSTEP  = IZ(IGROUP)        
      DELTAT = Z(IGROUP+1)        
      NOUT   = IZ(IGROUP+2)        
      IF (.NOT.NOPD) CALL GOPEN (PD, IZ(IBUF2), 2)        
      CALL GOPEN(UDV,IZ(IBUF3),3)        
      MCB(1)= UDV        
      CALL RDTRL(MCB)        
      IF(NLFTP .EQ. 0) GO TO 220        
      IF (ISPNL.GT.0) CALL GOPEN (PNL1, IZ(IBUF8), 3)        
  220 CONTINUE        
C        
C     RESTORE STUFF SAVED        
C        
      FILE = SCR1        
      CALL OPEN(*390,SCR1,IZ(IBUF1),0)        
      CALL FREAD(SCR1,Z(IU1+1),NROW,1)        
      CALL FREAD(SCR1,Z(IU3+1),NROW,1)        
      CALL FREAD(SCR1,Z(IU2+1),NROW,1)        
      CALL FREAD(SCR1,Z(IP2+1),NROW,1)        
      CALL CLOSE(SCR1,1)        
C        
C     COMPUTE U DOT        
C        
      H = 1.0/DELTA1        
      DO 230 I=1,NROW        
      K =  IP1 +I        
      L = IU2 +I        
      M = IU3 +I        
      Z(K) = (Z(L)-Z(M))*H        
  230 CONTINUE        
C        
C     COMPUTE U DOT DOT        
C        
      H = 1.0/(DELTA1*DELTA1)        
      DO 240 I=1,NROW        
      K = IP4+ I        
      L = IU2+ I        
      M = IU3+ I        
      J = IU1+ I        
      Z(K) = (Z(L)- 2.0*Z(M) +Z(J))*H        
  240 CONTINUE        
  250 CONTINUE        
C        
C     COMPUTE UI PRIME        
C        
      H = DELTAT*DELTAT/2.0        
      DO 260 I=1,NROW        
      K =IU1 +I        
      L = IU2 +I        
      M = IP1+I        
      J = IP4 +I        
      Z(K) = Z(L) -DELTAT*Z(M)+ H*Z(J)        
  260 CONTINUE        
C        
C     COMPUTE U DOT PRIME        
C        
      DO 270 I=1,NROW        
      K = IU3 + I        
      L = IP1+I        
      M = IP4 + I        
      Z(K) = Z(L) -DELTAT*Z(M)        
  270 CONTINUE        
C        
C     COMPUTE PI PRIME        
C        
      DO 280 I=1,NROW        
      K = IP1+I        
      Z(K) = 0.0        
  280 CONTINUE        
      CALL FORM2(Z(IP4+1),Z(IU3+1),Z(IU1+1),Z(IP1+1),Z(IBUF1))        
      ICOUNT = 0        
      GO TO IRET1,(60,10)        
C        
C     INTERNAL ROUTINE TO UNPACK VECTORS        
C        
  290 CALL UNPACK(*310,FILE,Z(IPNT+1))        
  300 GO TO IRETN, (5, 30, 40, 100, 350, 360, 370 )        
  310 DO 320 INL=1,NROW        
      K = IPNT +INL        
      Z(K) = 0.0        
  320 CONTINUE        
      GO TO 300        
C        
C     RETRIEVE LAST VECTOR        
C        
  330 CALL GOPEN(UDV,IZ(IBUF3),0)        
      K = 3*(NCOL - 1)        
      IAPEND = 1        
      CALL SKPREC(UDV,K)        
C        
C     GET U SUB I+1        
C        
      IPNT = IU2        
      ASSIGN 350 TO IRETN        
      GO TO 290        
CP        
C     GET U SUB I+1 DOT        
C        
  350 IPNT = IP1        
      ASSIGN 360 TO IRETN        
      GO TO 290        
C        
C     GET U SUB I+1 DOT DOT        
C        
  360 IPNT = IP4        
      ASSIGN 370 TO IRETN        
      GO TO 290        
  370 CONTINUE        
      CALL CLOSE(UDV,1)        
      CALL GOPEN (UDV, IZ(IBUF3), 1)        
      CALL MAKMCB (MCB, UDV, NROW, 2, 1)        
C        
C     OUTPUT INITIAL DISPLACEMENT        
C        
      CALL PACK (Z(IU2+1), UDV, MCB(1))        
C        
C     OUTPUT INITIAL VELOCITY        
C        
      CALL PACK (Z(IP1+1), UDV, MCB(1))        
C        
C     FORM P SUB I+1        
C        
      DO 380 I =1,NROW        
      K = IP2+I        
      Z(K) = 0.0        
  380 CONTINUE        
      CALL FORM2(Z(IP4+1),Z(IP1+1),Z(IU2+1),Z(IP2+1),Z(IBUF1))        
      ASSIGN 10 TO IRET1        
      GO TO 250        
C        
C     ERROR MESAGES        
C        
  390 IP1 = -1        
  400 CALL MESAGE(IP1,FILE,SUBNAM)        
      RETURN        
  430 IP1 = -8        
      FILE= ICRQ        
      GO TO 400        
      END        
