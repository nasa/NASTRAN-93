      INTEGER FUNCTION NUMTYP (Z)        
C        
C     TO CHECK NUMBER TYPE OF Z (NO D.P. CHECK)        
C        
C     SET NUMTYP TO 1 IF Z IS AN INTEGER        
C                TO 2 IF Z IS A  F.P.NUMBER        
C                TO 3 IF Z IS A  BCD WORD        
C                TO 0 IF Z IS AN INTEGER ZERO OR F.P. ZERO        
C        
C     THIS ROUTINE CHECKS THE DATA TYPE FOR IBM, CDC, VAX, UNIVAC (BOTH 
C     FOR AND FTN) AND OTHER MACHINES        
C        
C     BCD AND NBCD DATA WERE GENEREATED IN NSINFO        
C        
C     WRITTEN BY G.CHAN/UNISYS,  NOV. 1985        
C        
      EXTERNAL        LSHIFT,RSHIFT,ANDF        
      INTEGER         Z(1),TWO1,B0,BB,RSHIFT,ANDF,BIT27,BIT31,BLK,BLK32,
     1                CHR(115),BCD        
      COMMON /MACHIN/ MACHX,DUM3(3),MCHNAM        
      COMMON /SYSTEM/ SKIP(38),NBPC        
      COMMON /TWO   / TWO1(32),MZERO        
      COMMON /NUMTPX/ NBCD,BCD(1)        
      EQUIVALENCE     (RZ,IZ)        
      DATA    BB    / 0 /, BLK / 4H    /, B0  / 4H0000 /, BLK32 / 0 /   
      DATA    CHR   / 5, 0, 7, 8, 0, 8,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     9    1, 2, 3, 0, 0, 6, 7, 0, 0, 0,   0, 0, 0, 0, 0, 0, 7, 8, 0, 0, 
     1    1, 0, 0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 3, 4, 5, 6,        
     9          3, 4, 5, 6, 7, 8, 9, 2,   1, 0, 0, 0, 0, 0, 0, 0, 9, 1, 
     1    1, 2, 3, 4, 5, 6, 7, 0, 0, 0,   0, 0, 0, 0, 0, 6, 7, 8, 9, 3, 
     3    1, 2, 3, 0, 0, 0, 0, 0, 0, 4,   1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 
     5    0, 0, 0, 0, 0  /        
      DATA    KOUNT / 0  /        
C        
C     NASTRAN ALLOWABLE CHARACTER SET FOR IBM        
C     BLANK      . (+ &          $*)  -/         ,%  ?           #@'=   
C       6        7777788888888889999999999111111111111111111111111111   
C       4        5678901234567890123456789000000000011111111112222222   
C                                         012345678901234567890123456   
C     ABCDEFGHI       JKLMNOPQR        STUVWXYZ      0123456789        
C     111111122222222222222222222222222222222222222222222222222222222   
C     999999900000000001111111111222222222233333333334444444444555555   
C     345678901234567890123456789012345678901234567890123456789012345   
C        
      IZ = Z(1)        
      IF (IZ.EQ.0 .OR. IZ.EQ.MZERO) GO TO 200        
      IF (IZ.EQ.BLK  .OR. IZ.EQ.B0) GO TO 230        
      IZZ = IABS(IZ)        
      GO TO (10,  30,  60,  10,  70,   70, 140, 140, 140,  70, 180,  10,
C          DUMMY IBM  UNI  CDC  VAX ULTRIX SUN  AIX   HP  S/G  MAC  CRAY
C                     VAC       VMS   RISC        
     1      140,  120,   120, 180,  180,  180,  180,  180,  70,  180),  
C          CONVX  NEC  FUJTSU  DG  AMDL  PRIME  486  DUMMY ALPHA DUMMY  
C        
     2      MACHX        
C        
C     IBM 7094, UNIVAC 1108(FOR), CDC 6600, CRAY        
C     ========  ================  ========  ====        
C        
C ... CHECK FOR 2 (IBM,UNIVAC) OR 4 (CDC,CRAY) BLANKS ON RIGHT SIDE OF  
C     WORD        
C        
 10   IF (BB .NE. 0) GO TO 15        
      NBPC4 = NBPC*4        
      IF (MACHX.EQ.1 .OR. MACHX.EQ.3) NBPC4 = NBPC*2        
      BB = RSHIFT(LSHIFT(B0,NBPC4),NBPC4)        
 15   IF (RSHIFT(LSHIFT(IZ,NBPC4),NBPC4)-BB) 20,230,20        
C        
C ... CHECK FOR EXPONENT IN LEFT SIDE OF WORD        
C        
C20   IF (MACHX .EQ. 12) GO TO 130        
 20   IF (MACHX .EQ. 12) IF (RSHIFT(IZZ,48)) 210,210,220        
      IF (IZZ-TWO1(1)) 210,210,220        
C        
C     IBM        
C     ===        
C        
C ... IT IS REAL IF IT IS MAXIMUM NEGATIVE, i.e. -0.0        
C        
 30   IF (IZ .EQ. TWO1(1)) GO TO 220        
C        
C ... IT IS INTEGER, 8 DIGITS OR LESS, FIRST 5 AND A HALF BITS ARE ZEROS
C        
      IF (IZZ .LE. 99999999) GO TO 210        
C        
C ... IT IS REAL IF ANY ONE OF THE 4 BYTES NOT A NASTRAN ALLOWABLE BCD  
C     SYMBOL        
C        
      N = 0        
      DO 40 I = 1,4        
      J = ANDF(RSHIFT(IZ,N),255)        
C                           255 = 1111 1111 BINARY        
      IF (J .EQ. 64) GO TO 40        
      IF (J.LE.74 .OR. (J.GE.127 .AND. J.LE.192)) GO TO 220        
      K = 74        
      IF (J .GT. 192) K = K + 66        
      IF (CHR(J-K)) 40,220,40        
 40   N = N + 8        
C        
C ... IF Z IS OUTSIDE 10**+35 TO 10**-75 RANGE, SEND IT TO BCD        
C        
      J = ANDF(J,127)        
C                127 = 0111 1111 BINARY        
      IF (J.GT.98 .OR. J.LT.4) GO TO 230        
C        
C ... AT THIS POINT, IT IS STILL POSSIBLE TO HAVE SOME REAL NUMBERS     
C     THAT LOOK EXACTLY LIKE SOME 4-BYTE BCD WORDS        
C        
C     CHECK AGAINST SOME KNOWN BCD WORDS THAT ARE REALLY REAL NUMBERS   
C        
      IF (NBCD .EQ. 0) GO TO 230        
      DO 50 J = 1,NBCD        
      IF (IZ .EQ. BCD(J)) GO TO 220        
 50   CONTINUE        
      GO TO 230        
C        
C     UNIVAC 1108(FTN)        
C     ================        
C        
C ... IF IZZ .LE. BIT31, IT IS AN INTEGER.        
C     IF THE BINARY POINT, THE 27TH BIT, OF Z IS ON, IT IS A S.P. REAL  
C     NUMBER. OTHERWISE, IT IS A BCD WORD. THE 27TH BIT CAN NEVER BE    
C     OVERWRITTEN BY ANY BCD CHARACTER.        
C     (NOTE - THE BINARY POINT MAY SHIFT TO 26TH BIT IN D.P. NUMBER.    
C     NEEDS FURTHER VERIFICATION)        
C        
 60   BIT27 = TWO1(6)        
      BIT31 = 2*(TWO1(2)-1) + 1        
      IF (IZZ .LE. BIT31 ) GO TO 210        
      IF (ANDF(IZZ,BIT27)) 220,230,220        
C        
C        
C     VAX/VMS        
C     =======        
C        
C     **********************************************************        
C     *                                                        *        
C     * WARNING - INTEGER CHECK VALIDS ONLY FROM 1 THRU 16000  *        
C     *           THEREFORE VAX'S NUMTYP IS NOT RECOMMANDED    *        
C     *           FOR CHECKING GRID ID, TABLE ID ETC.          *        
C     *                                                        *        
C     **********************************************************        
C        
C     IF ALL 4 BYTES ARE NASTRAN ALLOWABLE BCD LETTERS (BCD CODE 32 THRU
C     90), IT COULD BE A BCD WORD        
C     CHECK AGAINST SOME KNOWN SYMBOLS THAT REALLY ARE NOT BCD WORDS    
C        
 70   N = 0        
      DO 80 I = 1,4        
      J = ANDF(RSHIFT(IZ,N),255)        
C               1111 1111 = 255        
C        
      IF (J.LT.32 .OR. J.GT.90) GO TO 100        
 80   N = N + 8        
      IF (NBCD .EQ. 0) GO TO 230        
      DO 90 N = 1,NBCD        
      IF (IZ .EQ. BCD(N)) GO TO 220        
 90   CONTINUE        
      GO TO 230        
C        
C     Z IS EITHER REAL OR INTEGER        
C        
C     (1) IF Z IS OUTSIDE 10**-27 TO 10**+27 RANGE, TREAT IT AS INTEGER.
C     (2) IF ABS.Z IS GREATER THAN 16000 (EXACTLY 1./16.0 F.P.VALUE)    
C         TREAT IT AS REAL        
C     (3) CHECK EXACT F.P. FRACTIONS FOR REAL        
C        
 100  RZ = ABS(RZ)        
      IF (RZ.LT.1.E-27 .OR. RZ.GT.1.E+27) GO TO 210        
      J = 16000        
      IF (IZZ .GT. J) GO TO 220        
      DO 110 N = 1,9        
      IF (IZ  .EQ. J) GO TO 220        
 110  J = J - 128        
      GO TO 210        
C        
C     64-BIT MACHINE        
C     ==============        
C        
C     NASTRAN BCD WORD CONTAINS ONLY 4 CHARACTERS, LEFT ADJUSTED AND    
C     BLANK FILL.  NASTRAN INTEGERS (ABSOLUTE) ARE LESS THEN 2**32.     
C     THEREFORE, IZ IS BCD WORD IF RIGHT HALF OF IZ IS BLANK, OTHERWISE 
C     IT IS INTEGER IF IT IS LESS THAN 2**32, OR REAL IF IT EXCEEDS     
C     2**32.        
C     POINTS OF INTEREST -        
C     1. TWO1(1) HERE IS A POSITIVE NO. IT WUOLD BE A NEGATIVE NUMBER   
C        IF MACHINE IS A 32-BIT WORD COMPUTER.        
C     2. BINARY POINT, BIT47, IS DEPENDABLE ONLY FOR REAL S.P. NUMBER,  
C        NOT FOR D.P.        
C        
 120  IF (BLK32 .EQ. 0) BLK32 = RSHIFT(BLK,32)        
      IF (IZZ .LE. TWO1(1)) GO TO 210        
      IF (ANDF(IZ,BLK32)-BLK32) 220,230,220        
C        
C     CRAY        
C     ====        
C        
C130  IF (IZZ-LSHIFT(1,48)) 210,220,220        
C        
C     UNIX/RISC        
C     =========        
C        
C     IF ALL 4 BYTES ARE NASTRAN ALLOWABLE BCD LETTERS (ASCII CODE 32   
C     THRU 90), IT COULD BE A BCD WORD        
C     CHECK AGAINST SOME KNOWN SYMBOLS THAT REALLY ARE NOT BCD WORDS    
C        
 140  N = 0        
      DO 150 I = 1,4        
      J = ANDF(RSHIFT(IZ,N),255)        
C               1111 1111 = 255        
C        
      IF (J.LT.32 .OR. J.GT.90) GO TO 170        
 150  N = N + 8        
      IF (NBCD .EQ. 0) GO TO 230        
      DO 160 N = 1,NBCD        
      IF (IZ .EQ. BCD(N)) GO TO 220        
 160  CONTINUE        
      GO TO 230        
C        
C     Z IS EITHER REAL OR INTEGER        
C        
C     ASSUME LEFT 8 BITS, SIGN BIT EXCLUDED, FORM THE EXPONENT OF A     
C     REAL NUMBER, AND INTEGERS (ABSOLUTE) ARE LESS THAN 2**24        
C        
 170  IF (IZZ-16777216) 210,220,220        
C        
C     UNKOWN MACHINE        
C     ==============        
C        
 180  KOUNT = KOUNT + 1        
      IF (KOUNT .GT. 10) CALL MESAGE (-61,0,0)        
      WRITE  (6,190) MCHNAM,MACHX        
 190  FORMAT (/,' *** SUBROUTINE NUMTYP HAS NOT BEEN SETUP FOR ',A4,    
     1       '- MACHINE (TYPE',I3,') ***')        
C        
C     Z IS INTEGER OR REAL ZERO. MAKE SURE Z IS RETURNED AS POSITIVE 0  
C        
 200  NUMTYP = 0        
      Z(1)   =+0        
      GO TO 240        
C        
C     TYPE IS INTEGER        
C        
 210  NUMTYP = 1        
      GO TO 240        
C        
C     TYPE IS REAL        
C        
 220  NUMTYP = 2        
      GO TO 240        
C        
C     TYPE IS BCD        
C        
 230  NUMTYP = 3        
 240  RETURN        
      END        
