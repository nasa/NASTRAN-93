      SUBROUTINE GNFIAT        
C        
C     GNFIAT WILL GENERATE THE FIAT AND XFIAT FILES FOR VAX I/O        
C        
      INTEGER         FIAT,PFIST,XFIAT,NTAB(74)        
      COMMON /XFIAT / MFIAT,NFIAT,LFIAT,FIAT(1)        
      COMMON /XPFIST/ PFIST        
      COMMON /XXFIAT/ XFIAT(1)        
      COMMON /SYSTEM/ DUM(23),ICFIAT,SKIP(4),MAXFIL        
C        
C        
C     NTAB TABLE INDICATES STATUS FOR EACH LOGICAL UNIT        
C        
C     STATUS = 1  UNIT AVAILABLE FOR FIAT        
C     STATUS = 2  UNIT AVAILABLE IF NOT SET UP BY USER        
C     STATUS = 3  UNIT NOT AVAILABLE        
C     STATUS = 4  UNIT NOT TO BE DYNAMICALLY ASSIGNED        
C        
      DATA LNTAB  /74/,   NTAB/        
     O     4,   3,   1,   4,   4,   4,   3,   3,   3,   3,        
     1     3,   3,   3,   3,   3,   3,   3,   3,   3,   3,        
     2     3,   3,   3,   3,   3,   1,   1,   1,   1,   1,        
     3     1,   1,   1,   1,   1,   1,   1,   1,   1,   1,        
     4     1,   1,   1,   1,   1,   1,   1,   1,   1,   1,        
     5     1,   1,   1,   1,   1,   1,   1,   1,   1,   1,        
     6     1,   1,   1,   1,   1,   1,   1,   1,   1,   3,        
     7     1,   1,   1,   1/        
C        
C        
C        
C     DYNAMICALLY ASSIGN UNIT -        
C     INSERT STATUS 1 AND STATUS 2 FILES INTO FIAT        
C        
      J = 1        
      DO 10 I = 1,LNTAB        
      IF (MFIAT .EQ. MAXFIL) GO TO 20        
      IF (NTAB(I) .GE.    3) GO TO 10        
      MFIAT = MFIAT + 1        
      FIAT(J) = I        
      J = J + ICFIAT        
  10  CONTINUE        
  20  LFIAT = MFIAT        
C        
C     ENTER UNITS INTO XFIAT        
C     PFIST IS SET TO 24 BY SEMDBD        
C        
      J = 7        
      DO 30 I = 1,PFIST        
      XFIAT(I) = J        
  30  J = J + 1        
      RETURN        
      END        
