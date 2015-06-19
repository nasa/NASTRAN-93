      SUBROUTINE GPSTGN        
C        
C     THIS MODULE GENERATES THE GRID POINT SINGULARITY TABLE        
C     BY EXAMINING THE SUBMATRICES ALONG THE LEADING DIAGONAL        
C     OF THE INPUT STIFFNESS MATRIX        
C        
C     MODULE DMAP SEQUENCE        
C        
C     GPSTGEN  KGG,SIL/GPST $        
C        
      DIMENSION        K(3)  , MCB(7),ISUBNM(2)        
C        
      INTEGER          SIL   , GPST        
C        
      DOUBLE PRECISION B        
C        
      COMMON /GPSTGX/  GPST  , IGPST , ISIL  , NSING , IBUF2        
      COMMON /GPSTGY/  B(18)        
      COMMON /SYSTEM/  ISYSBF        
      COMMON /UNPAKX/  ITYPOT, II    , JJ    , INCR        
CZZ   COMMON /ZZGPST/  IZ(1)        
      COMMON /ZZZZZZ/  IZ(1)        
C        
      DATA KGG, SIL /101   , 102   /        
      DATA ISUBNM   /4HGPST, 4HGN  /        
C        
      GPST  = 201        
      IGPST = 0        
      NSING = 0        
      ITYPOT= 2        
      INCR  = 1        
      K(1)  = 1        
      K(2)  = 1        
      IBUF1 = KORSZ (IZ) - ISYSBF - 2        
      IBUF2 = IBUF1 - ISYSBF        
      IFILE = SIL        
      CALL OPEN (*120,SIL,IZ(IBUF1),0)        
      CALL SKPREC (SIL,1)        
      MCB(1) = SIL        
      CALL RDTRL (MCB)        
      LUSET = MCB(3)        
      ICORE = LUSET + 1 - IBUF1        
      IF (ICORE.GE.0) GO TO 160        
      CALL READ (*140,*10,SIL,IZ,IBUF1,0,NPTS)        
      GO TO 160        
   10 CALL CLOSE (SIL,1)        
      LOGIC = 110        
      IF (NPTS.NE.MCB(2)) GO TO 150        
      IZ(NPTS+1) = LUSET + 1        
C        
      IFILE = KGG        
      CALL OPEN (*120,KGG,IZ(IBUF1),0)        
      CALL SKPREC (KGG,1)        
      MCB(1) = KGG        
      CALL RDTRL (MCB)        
      LOGIC = 120        
      IF (MCB(2).NE.LUSET .OR. MCB(3).NE.LUSET) GO TO 150        
C        
      DO 100 I = 1, NPTS        
      ITYP = 1        
      ISIL = IZ(I)        
      ISILNX = IZ(I+1)        
      IF (ISILNX-ISIL.EQ.1) ITYP = 2        
      ILOOP = 1        
      IST   = 1        
      II = ISIL        
   20 JJ = II + 2*(2 - ITYP)        
      DO 60 J = II, JJ        
      CALL UNPACK (*30,KGG,B(IST))        
      GO TO 50        
   30 ISTX = IST + 2        
      DO 40 III = IST, ISTX        
      B(III) = 0.0D0        
   40 CONTINUE        
   50 IST = IST + 3        
   60 CONTINUE        
      IF (ITYP .EQ.2) GO TO 70        
      IF (ILOOP.EQ.2) GO TO 90        
      ILOOP = 2        
      II = II + 3        
      GO TO 20        
   70 IF (B(1).GT.0.0D0) GO TO 100        
      K(3) = ISIL        
      IF (IGPST.EQ.1) GO TO 80        
      IGPST = 1        
      CALL GOPEN (GPST,IZ(IBUF2),1)        
   80 NSING = NSING + 1        
      CALL WRITE (GPST,K,3,0)        
      GO TO 100        
   90 CALL GPSTG        
  100 CONTINUE        
      IF (IGPST.EQ.0) GO TO 110        
      CALL WRITE (GPST,0,0,1)        
      CALL CLOSE (GPST,1)        
      CALL MAKMCB (MCB,GPST,NPTS,LUSET,0)        
      MCB(2) = NSING        
      CALL WRTTRL (MCB)        
  110 CALL CLOSE (KGG,1)        
      GO TO 170        
C        
C     ERROR MESSAGES        
C        
  120 N = -1        
  130 CALL MESAGE (N,IFILE,ISUBNM)        
  140 N = -2        
      GO TO 130        
  150 N = -7        
      GO TO 130        
  160 N = -8        
      IFILE = ICORE        
      GO TO 130        
C        
  170 RETURN        
      END        
