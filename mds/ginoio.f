      SUBROUTINE GINOIO (*,OPCODE,B,IMHERE)        
C        
C     GINO INPUT/OUTPUT OPERATIONS OF THE BUFFER BLOCKS (FORTRAN)       
C     REVISED  1/06/87, BY G.CHAN/UNISYS        
C        
C     OPERATIONS ARE AS FOLLOWS (OPCODE):        
C         1 - REWIND FILE        
C         2 - WRITE ONE BLOCK        
C         3 - READ  ONE BLOCK        
C         4 - BACKSPACE ONE BLOCK        
C         5 - FORWARD   ONE BLOCK        
C         6 - REPOSITION TO REQUESTED BLOCK        
C         7 - CLOSE AND DELETE FILE        
C         8 - RE-READ ONE BLOCK              =4+3        
C         9 - CLOSE (DELETE) AND REWIND FILE =7+1        
C        10 - REWIND AND READ FIRST BLOCK    =1+3        
C        
      CHARACTER*44    DSNAMES
      INTEGER         OPCODE   ,ENTRY ,RW    ,F     ,B(NBUFF3) ,R12345, 
     1                R        ,RF        
      COMMON /DSNAME/ DSNAMES(80)
      COMMON /MACHIN/ MAC(3)   ,LQRO        
      COMMON /SYSTEM/ SYSBUF   ,NOUT        
      COMMON /GINOX / LENGTH   ,F     ,EOR   ,OP    ,ENTRY ,LSTNAM    , 
     1                N,        NAME  ,NTAPE ,NBLOCK,NLR   ,UNITAB(75), 
     2                BUFADD(75)      ,NBUFF3,RW    ,IUNITS(300)      , 
     3                IBLOCK(20)      ,KLOSE ,LOCFX1,R12345,R(75)     , 
     4                IOPEN(75)        
C     DATA            IOPEN,    R  /   75*0,  75*1  /        
C        
      IF (R12345 .EQ. -1234567890) GO TO 5        
      R12345 = -1234567890        
      DO 3 I = 1,75        
      IOPEN(I) = 0        
 3    R(I) = 1        
 5    IF (F .LE. 1) RETURN        
      IF (IOPEN(F) .EQ. 1) GO TO 7        
      LREC = NBUFF3*(MOD(LQRO,100)/10)        
C                    MOD(LQRO,100)/10 IS WORD OR BYTE COUNT        
C        
C     VAX:        
C     NBUFF3 IS  RECORDSIZE, IN WORD COUNT        
C     IF MAXRECORD IS NOT SPECIFIED IN OPEN STATEMENT, DEFAULT IS       
C     UNLIMITED.        
C     DOWN BELOW, THE WORD 'BLOCK' IS SYNONYMOUS TO 'RECORD'        
C     E.G.  'READ ONE BLOCK' IS ACTURALLY 'READ ONE RECORD'        
C        
C     SEE VAX FORTRAN PERFORMANCE GUIDE, MAY 1990, SECTION 6.1.1. FOR   
C     BLOCKSIZE & BUFFERCOUNT        
C        
      OPEN (UNIT=F,ACCESS='DIRECT',FORM='UNFORMATTED',STATUS='UNKNOWN', 
     1      RECL=LREC, FILE=DSNAMES(F) )        
C    2               ,BLOCKSIZE=LREC,BUFFERCOUNT=2)        
C    2               ,RCDS     =LREC)        
      IOPEN(F) = 1        
 7    IF (F.EQ.0 .AND. OPCODE.EQ.0 .AND. R(F).EQ.0) CALL VAXERR (7)     
C        
C     BRANCH TO REQUESTED OPERATION.        
C        
      GO TO (10,20,30,40,50,60,70,80,90,100), OPCODE        
C        
C     REWIND FILE:        
C     ===========        
C        
 10   R(F) = 1        
      GO TO 35        
C        
C     WRITE ONE BLOCK:        
C     ===============        
C        
 20   RF = R(F)        
      WRITE (F,REC=RF) B        
      R(F) = R(F) + 1        
      GO TO 35        
C        
C     READ ONE BLOCK:        
C     ==============        
C        
 30   RF = R(F)        
      READ (F,REC=RF,ERR=110) B        
      R(F) = R(F) + 1        
 35   IF (KLOSE .NE. 1) RETURN        
      IOPEN(F) = 0        
      CLOSE (UNIT=F,STATUS='KEEP')        
      RETURN        
C        
C     BACKSPACE ONE BLOCK:        
C     ===================        
C        
 40   IF (R(F) .GT. 1) R(F) = R(F) - 1        
      RETURN        
C        
C     FORWARD ONE BLOCK:        
C     =================        
C        
 50   R(F) = R(F) + 1        
      RETURN        
C        
C     REPOSITION TO REQUESTED BLOCK:        
C     =============================        
C        
 60   IF (NBLOCK .EQ. R(F)-1) RETURN        
      READ (F,REC=NBLOCK) B        
      R(F) = NBLOCK + 1        
      RETURN        
C        
C     CLOSE AND DELETE FILE:        
C     =====================        
C        
 70   CLOSE (UNIT=F,STATUS='DELETE')        
      IOPEN(F) = 0        
      RETURN        
C        
C     RE-READ CURRENT BLOCK: (4 + 3)        
C     =====================        
C        
 80   IF (R(F) .GT. 1) R(F) = R(F) - 1        
      GO TO 30        
C        
C     CLOSE (& DELETE) FILE AND REWIND: (7 + 1)        
C     ================================        
C        
 90   CLOSE (UNIT=F,STATUS='DELETE')        
      IOPEN(F) = 0        
      GO TO 10        
C        
C     REWIND AND READ FIRST BLOCK: (1 + 3)        
C     ===========================        
C        
 100  CONTINUE        
      READ (F,REC=1,ERR=110) B        
      R(F) = 2        
      GO TO 35        
C        
 110  WRITE  (NOUT,120) IMHERE        
 120  FORMAT (' *** GINOIO ERROR.   IMHERE=',I5)        
      RETURN 1        
      END        
