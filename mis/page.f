      SUBROUTINE PAGE        
C        
C     MASTER PAGING ROUTINE FOR NASTRAN.        
C        
      INTEGER         OTPE,DATE,CRDATE,SYM,TITLEX(18),NAME(2)        
      CHARACTER*9     MONTH(12)        
      COMMON /MACHIN/ MACH(4),MCHNAM        
      COMMON /SYSTEM/ SYSBUF,OTPE,MPCN(3),SPCN,METHOD,LOADN,SYM,ST,     
     1                IPAGE,LINE,ITLINE,MAXLIN,DATE(3),DUM15(15),IOFP,  
     2                X(8),CRDATE(3)        
      COMMON /OUTPUT/ TITLE(32),SUBTIT(32),LABEL(32),HEAD1(32),        
     1                HEAD2(32),HEAD3(32)        
      EQUIVALENCE     (TITLEX(1),TITLE(1))        
      DATA    MONTH /'  JANUARY', ' FEBRUARY', '    MARCH', '    APRIL',
     1               '      MAY', '     JUNE', '     JULY', '   AUGUST',
     2               'SEPTEMBER', '  OCTOBER', ' NOVEMBER', ' DECEMBER'/
      DATA    NAME  / 4HPAGE, 4H    /        
C        
      IOUT  = 1        
   10 IPAGE = IPAGE  + 1        
      ITLINE= ITLINE + LINE        
      LINE  = 0        
      IF (ITLINE .GT. MAXLIN) GO TO 70        
      IN = DATE(1)        
      WRITE  (OTPE,20) TITLEX,MONTH(IN),        
     1       DATE(2),DATE(3),CRDATE(2),CRDATE(3),MCHNAM,IPAGE        
C    1       DATE(2),DATE(3),CRDATE,IPAGE        
   20 FORMAT (1H1,4X,17A4,A2,2X,A9,2X,I2,4H, 19,I2,'  / RELEASE ',      
     1       2A3,A4,' /  PAGE',I6)        
C    1       A4,A3,A2,'/   PAGE ',I6)        
      WRITE  (OTPE,30) SUBTIT        
   30 FORMAT (5X,31A4,A3)        
      WRITE  (OTPE,40) LABEL        
   40 FORMAT (1H0,4X,31A4,A3)        
      LINE = LINE + 4        
      IF (IOUT .EQ.0) GO TO 60        
      WRITE (OTPE,40) (HEAD1(I),I=1,32)        
      WRITE (OTPE,30) (HEAD2(I),I=1,32)        
      WRITE (OTPE,30) (HEAD3(I),I=1,32)        
      LINE = LINE + 4        
   60 RETURN        
C        
C     MAX LINES EXCEEDED.  BUMP MAXLINES BY 3000 AND CALL MESAGE        
C        
   70 MAXLIN = MAXLIN + 3000        
      CALL MESAGE (-19,ITLINE,NAME)        
      GO TO 60        
C        
C        
      ENTRY PAGE1        
C     ===========        
C        
      IOUT = 0        
      GO TO 10        
      END        
