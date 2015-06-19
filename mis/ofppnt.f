      SUBROUTINE OFPPNT (OUT,XUT,NWDS,FMT,IFMT,METHD)        
C        
C     TO PRINT A LINE. CURRENTLY CALLED ONLY BY OFP        
C        
C  $MIXED_FORMATS        
C        
C     METHOD 1 (METHD= 0) RELIES ON MIXED-FORMAT FORTRAN OUTPUT WRITE.  
C     IF METHOD 1 DOES NOT WORK, USE METHOD 2 (METHD = 2)        
C        
C     METHOD 2 REQUIRES ALL REAL NUMBERS GREATER THAN 2**27 AND ALL     
C     INTEGERS LESS THAN 2**27 (9 DIGITS)        
C     CALLER MUST SET THE VARIABLE 'METHD' TO 2 EACH TIME A NEW FORMAT  
C     IS USED. OFPPNT ALWAYS SET 'METHD' TO -2 ANTICIPATING THE PREVIOUS
C     FORMAT IS TO BE USED AGAIN.        
C        
C     METHOD 2 WAS WRITTEN BY G.CHAN/UNISYS   6/92        
C        
      INTEGER         OUT(NWDS),FMT(IFMT),NAM(2),M(60)        
C     Hp ONLY:                  FMT(300 )
      REAL            XUT(NWDS)        
      COMMON /SYSTEM/ SYSBUF, L        
      DATA    INTG  / 134217728 /,  NAM / 4HOFPP, 4HNT   /        
C                     134217728 = 2**27        
C        
      IF (METHD) 90,10,30        
C        
C     METHOD 1 -        
C        
   10 CONTINUE        
C     I = MIN0(IFMT,30)        
C     WRITE  (L,20) (FMT(K),K=1,I)        
C  20 FORMAT (' FMT=',30A4)        
C        
      WRITE  (L,FMT,ERR=200) XUT        
      RETURN        
C        
C        
C     METHOD 2 -        
C        
   30 METHD = -2        
      IFLAG = +1        
      IF (IABS(OUT(1)) .LT. INTG) IFLAG = -1        
      IFLAG1= IFLAG        
      M(1)  = 1        
      KOUNT = 1        
      K     = 2        
      IF (NWDS .EQ. 1) GO TO 80        
      DO 70 I = 2,NWDS        
      IF (IFLAG) 40,180,50        
   40 IF (IABS(OUT(I)) .LT. INTG) GO TO 70        
      IFLAG = +1        
      GO TO 60        
   50 IF (IABS(OUT(I)) .GE. INTG) GO TO 70        
      IFLAG = -1        
   60 M(K)  = I - 1        
      M(K+1)= I        
      K     = K + 2        
      KOUNT = KOUNT + 1        
   70 CONTINUE        
      IF (KOUNT .GT. 30) GO TO 160        
   80 M(K) = NWDS        
   90 CONTINUE        
C        
C     WRITE (L,100) (M(I),I=1,K)        
C 100 FORMAT ('  OFPPNT@70  M = ',28I4, /2X,32I4)        
C        
      IF (IFLAG1 .EQ. +1) GO TO 130        
C        
      IF (KOUNT .GT. 1) IF (MOD(KOUNT,2)) 120,110,120        
      WRITE (L,FMT) (OUT(I),I=M(1),M(2))        
      GO TO 200        
  110 WRITE (L,FMT)        
     1      ((OUT(I),I=M(J),M(J+1)),(XUT(I),I=M(J+2),M(J+3)),J=1,K,4)   
      GO TO 200        
  120 K = K - 2        
      WRITE (L,FMT)        
     1      ((OUT(I),I=M(J),M(J+1)),(XUT(I),I=M(J+2),M(J+3)),J=1,K,4),  
     2      (OUT(I),I=M(K+1),M(K+2))        
      GO TO 200        
C        
  130 IF (KOUNT .GT. 1) IF (MOD(KOUNT,2)) 150,140,150        
      WRITE (L,FMT) (XUT(I),I=M(1),M(2))        
      GO TO 200        
  140 WRITE (L,FMT)        
     1      ((XUT(I),I=M(J),M(J+1)),(OUT(I),I=M(J+2),M(J+3)),J=1,K,4)   
      GO TO 200        
  150 K = K - 2        
      WRITE (L,FMT)        
     1      ((XUT(I),I=M(J),M(J+1)),(OUT(I),I=M(J+2),M(J+3)),J=1,K,4),  
     2      (XUT(I),I=M(K+1),M(K+2))        
      GO TO 200        
C        
C     ERROR M ARRAY TOO SMALL        
C        
  160 WRITE  (L,170) K        
  170 FORMAT (/5X,' FATAL ERROR, M ARRAY IN OPNPNT TOO SMALL.  K=',I3)  
  180 CALL MESAGE (-37,0,NAM)        
C        
  200 RETURN        
      END        
