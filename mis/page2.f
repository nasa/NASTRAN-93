      SUBROUTINE PAGE2 (LINES)        
C        
C     2ND MASTER PAGING ROUTINE FOR NASTRAN        
C        
C     IABS(LINES) = NO. OF LINES TO BE ADDED FOR OUTPUT        
C     IF CURRENT PAGE CAN NOT ACCOMODATE THE INCOMING LINES, A NEW PAGE 
C     IS INITIATED WITH PROPER HEADINGS.        
C        
C     IF LINES IS NEGATIVE, A 6-LINE HEADER IS PRINTED.        
C     IF LINES IS POSITIVE, A 3-LINE HEADER IS PRINTED AND FOLLOWED BY  
C        3 BLANK LINES.        
C        
C     ENTRY POINT PAGE3 -        
C     A 3-LINE HEADER IS PRINTED, NO BLANK LINES FOLLOWED. LINES CAN BE 
C     NEGATIVE OR POSITIVE.        
C        
C     SIMPLIFIED BY G.CHAN/UNISYS, AND PAGE3 ADDED  12/92        
C        
      IMPLICIT INTEGER (A-Z)        
      INTEGER         TTLE(18),NAME(2)        
      CHARACTER*9     MONTH(12)        
      COMMON /MACHIN/ MACH(4),MCHNAM        
      COMMON /SYSTEM/ IBUF,NOUT,DUM6(6),SYM,ST,PAGE,LINE,TLINE,MAXLIN,  
     1                DATE(3),DUM15(15),OFP,DUM8(8),CRDATE(3)        
      COMMON /OUTPUT/ TITLE(32),SUBTIT(32),LABEL(32),HEAD1(32),        
     1                HEAD2(32),HEAD3(32)        
      EQUIVALENCE     (TTLE(1),TITLE(1))        
      DATA    MONTH /'  JANUARY', ' FEBRUARY', '    MARCH', '    APRIL',
     1               '      MAY', '     JUNE', '     JULY', '   AUGUST',
     2               'SEPTEMBER', '  OCTOBER', ' NOVEMBER', ' DECEMBER'/
      DATA    NAME  / 4H PAG, 4HE2  /        
C        
      FLAG  = 2        
C        
   10 IF (LINES .EQ. 0) GO TO 100        
      LL    = IABS(LINES)        
      IF (SYM-LINE.LT.LL .OR. OFP.NE.0) GO TO 30        
   20 LINE  = LINE + LL        
      GO TO 100        
C        
   30 PAGE  = PAGE  + 1        
      TLINE = TLINE + LINE        
      LINE  = 0        
      IF (TLINE .GT. MAXLIN) GO TO 90        
      IN    = DATE(1)        
      WRITE  (NOUT,40) TTLE,MONTH(IN),DATE(2),DATE(3),CRDATE(2),        
     1       CRDATE(3),MCHNAM,PAGE        
   40 FORMAT (1H1,4X,17A4,A2,1X,A9,2X,I2,', 19',I2,4X,'RELEASE ' ,      
     1       2A3,A4,4X,'PAGE',I6)        
      WRITE  (NOUT,50) SUBTIT        
   50 FORMAT ( 5X,31A4,A3)        
      WRITE  (NOUT,60) LABEL        
   60 FORMAT (/5X,31A4,A3)        
      LINE  = LINE + 4        
      IF (FLAG  .LT. 0) GO TO 20        
      IF (LINES .GT. 0) GO TO 70        
C        
      WRITE (NOUT,60) (HEAD1(I),I=1,32)        
      WRITE (NOUT,50) (HEAD2(I),I=1,32)        
      WRITE (NOUT,50) (HEAD3(I),I=1,32)        
      LINE  = LINE + 4        
      GO TO 20        
C        
   70 WRITE  (NOUT,80)        
   80 FORMAT (///)        
      LINE  = LINE + 4        
      GO TO 20        
C        
C     MAX LINES EXCEEDED.  BUMP MAXLINES BY 3000 AND CALL MESAGE        
C        
   90 MAXLIN = MAXLIN + 3000        
      CALL MESAGE (-19,TLINE,NAME)        
C        
  100 OFP  = 0        
      RETURN        
C        
C        
      ENTRY PAGE3 (LINES)        
C     ===================        
C        
      FLAG = -3        
      GO TO 10        
C        
      END        
