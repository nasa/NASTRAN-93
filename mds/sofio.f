      SUBROUTINE SOFIO (IRW,IBLKNM,BUF)        
C        
      LOGICAL         ONCE        
      INTEGER         FILNAM,FILSIZ,HIBLK,BLKSIZ,NUNITS(10),        
     1                BUF(4),SYSBUF,FLSPEC(17,2)        
      COMMON /MACHIN/ MAC(3),LQRO        
      COMMON /SYSTEM/ SYSBUF,NOUT        
      COMMON /SOFCOM/ NFILES,FILNAM(10),FILSIZ(10)        
      COMMON /SYS   / BLKSIZ,SKIP(3),HIBLK        
CWKBNB
      COMMON /SOFDSN/ SDSN(10)
      CHARACTER*60  SDSN
CWKBNE
      DATA    ONCE  /.FALSE./        
      DATA    FLSPEC/        
     1        4HINPT, 4HFT03, 4HFT08, 4HFT09, 4HFT10, 4HFT11,        
     2        4HFT12, 4HFT14, 4HFT15, 4HFT16, 4HFT17, 4HFT18,        
     3        4HFT19, 4HFT20, 4HFT21, 4HFT22, 4HFT23,        
     4        14    , 3     , 8     , 9     , 10    , 11    ,        
     5        12    , 14    , 15    , 16    , 17    , 18    ,        
     6        19    , 20    , 21    , 22    , 23    /        
C        
      IF (ONCE) GO TO 50        
      ONCE = .TRUE.        
C        
C     SET UP ARRAY OF FORTRAN UNIT NUMBERS        
C        
      DO 40 I = 1,NFILES        
      DO 20 J = 1,17        
      IF (FILNAM(I) .NE. FLSPEC(J,1)) GO TO 20        
      NUNITS(I) = FLSPEC(J,2)        
      IBKSZ = BLKSIZ*(MOD(LQRO,100)/10)        
C                     MOD(LQRO,100)/10 IS BYTE OR WORD COUNT        
C        
      OPEN (UNIT   = NUNITS(I),        
CWKBI
     1      FILE   = SDSN(I),
     1      STATUS = 'UNKNOWN',        
     2      FORM   = 'UNFORMATTED',        
     3      ACCESS = 'DIRECT',        
     4      MAXREC = FILSIZ(I),        
     5      RECL   = IBKSZ)        
C    6      RECORDTYPE = 'FIXED',        
C    7      BLOCKSIZE  = IBKSZ,        
C    8      ORGANIZATION = 'SEQUENTIAL')        
      GO TO 40        
   20 CONTINUE        
      WRITE  (NOUT,30) FILNAM(I),(FLSPEC(J,1),J=1,17)        
   30 FORMAT ('0*** USER FATAL MESSAGE, ',A4,' IS ILLEGAL NAME FOR SOF',
     1        ' OPERATION.  AVAILABLE FILE NAMES ARE -',/5X,17(A4,2H, ))
      CALL MESAGE (-61,0,0)        
   40 CONTINUE        
C        
C     FIND BLOCK NUMBER        
C        
   50 IBLK = IBLKNM        
      IF (IBLK .LT. 1) GO TO 90        
      DO 70 I = 1,NFILES        
      IF (IBLK .GT. FILSIZ(I)) GO TO 60        
      IFILE = NUNITS(I)        
      GO TO 80        
   60 IBLK = IBLK - FILSIZ(I)        
   70 CONTINUE        
      GO TO 90        
C        
C      PERFORM INPUT/OUTPUT        
C        
   80 CALL SOFIOF (IRW,IFILE,IBLK,BUF(4),BLKSIZ)        
      IF (IRW .EQ. 1) RETURN        
      IF (IBLK.NE.1 .AND. IBLKNM.GT.HIBLK) HIBLK = IBLKNM        
      RETURN        
C        
C     ERRORS        
C        
   90 WRITE  (NOUT,100) IBLKNM        
  100 FORMAT ('0*** SYSTEM FATAL MESSAGE 6225, BLOCK NUMBER',I10,       
     1        ' OUT OF RANGE OF SOF FILES.')        
      CALL MESAGE (-61,0,0)        
      RETURN        
      END        
