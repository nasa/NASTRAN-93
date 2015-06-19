      PROGRAM NASTRN        
C        
      CHARACTER*44    VALUE, DSNAMES
      CHARACTER*5     TMP
      INTEGER         SPERLK,DEBUG
      COMMON /SYSTEM/ ISYSTM(94),SPERLK
      COMMON /DSNAME/ DSNAMES(80)
      COMMON /SOFDSN/ SDSN(10)
      CHARACTER*60    SDSN
C        
C     SAVE STARTING CPU TIME AND WALL CLOCK TIME IN /SYSTEM/        
C      
      CALL KLOCK  (ISYSTM(18))        
      CALL WALTIM (ISYSTM(32))        
C        
C     EXECUTE NASTRAN SUPER LINK
C        
      SPERLK = 1        
      ISYSTM(11) = 1        
      VALUE = ' '
      LEN = 44
      CALL GETENV ( 'DIRCTY', VALUE )
      DO 5 I = 44, 1, -1
      IF ( VALUE( I:I ) .eq. ' ' ) GO TO  5
      LEN = I
      GO TO 7
5     CONTINUE
7     CONTINUE
      DO 10 I = 1, 75
      IF ( I .LE. 9 ) WRITE ( TMP, 901 ) I
      IF ( I .GT. 9 ) WRITE ( TMP, 902 ) I
901   FORMAT('scr',I1)
902   FORMAT('scr',I2)
      DSNAMES( I ) = VALUE(1:LEN)//'/'//TMP
10    CONTINUE
      CALL GETENV ( 'LOGNM', VALUE )
      DSNAMES(4) = VALUE
      CALL GETENV ( 'OPTPNM', VALUE )
      DSNAMES(8)  = VALUE
      CALL GETENV ( 'NPTPNM', VALUE )
      DSNAMES(9)  = VALUE
      CALL GETENV ( 'OUT11', VALUE )
      DSNAMES(11) = VALUE
      CALL GETENV ( 'IN12', VALUE )
      DSNAMES(12) = VALUE
      CALL GETENV ( 'PLTNM', VALUE )
      DSNAMES(13) = VALUE
      CALL GETENV ( 'DICTNM', VALUE )
      DSNAMES(76) = VALUE
      CALL GETENV ( 'PUNCHNM', VALUE )
      DSNAMES(77) = VALUE 
      CALL GETENV ( 'SOF1', VALUE )
      SDSN(1) = VALUE
      CALL GETENV ( 'SOF2', VALUE )
      SDSN(2) = VALUE
      CALL GETENV ( 'SOF3', VALUE )
      SDSN(3) = VALUE
      CALL GETENV ( 'SOF4', VALUE )
      SDSN(4) = VALUE
      CALL GETENV ( 'SOF5', VALUE )
      SDSN(5) = VALUE
      CALL GETENV ( 'SOF6', VALUE )
      SDSN(6) = VALUE
      CALL GETENV ( 'SOF7', VALUE )
      SDSN(7) = VALUE
      CALL GETENV ( 'SOF8', VALUE )
      SDSN(8) = VALUE
      CALL GETENV ( 'SOF9', VALUE )
      SDSN(9) = VALUE
      CALL GETENV ( 'SOF10', VALUE )
      SDSN(10) = VALUE
      OPEN (  4, FILE=DSNAMES(4) ,STATUS='UNKNOWN')
      IF ( DSNAMES(11) .NE. 'none' )
     & OPEN ( 11, FILE=DSNAMES(11),STATUS='UNKNOWN')
      IF ( DSNAMES(12) .NE. 'none' )
     & OPEN ( 12, FILE=DSNAMES(12),STATUS='UNKNOWN')
      IF ( DSNAMES(13) .NE. 'none' )
     & OPEN ( 13, FILE=DSNAMES(13),STATUS='UNKNOWN')
      IF ( DSNAMES(76) .NE. 'none' )
     & OPEN ( 76, FILE=DSNAMES(76),STATUS='UNKNOWN')
      IF ( DSNAMES(77) .NE. 'none' )
     & OPEN ( 77, FILE=DSNAMES(77),STATUS='UNKNOWN')
      CALL XSEM00       
      STOP
      END        
