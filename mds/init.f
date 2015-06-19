      SUBROUTINE INIT (*,IRDWRT,JBUFF)        
C        
      INTEGER         IUNITS(300)  ,XYZ   ,UNITAB,BUFADD,PRVOPN,        
     1                RSHIFT,ANDF        
      COMMON /GINOX / LENGTH,IFILEX,IEOR  ,IOP   ,IENTRY,LSTNAM,        
     1                N     ,NAME  ,NTAPE ,XYZ(2),UNITAB(75)   ,        
     2                BUFADD(75)   ,NBUFF3,PRVOPN,IUNITS        
      COMMON /SYSTEM/ IBUF  ,NOUT        
      DATA   ICLOSE / 4            /        
      DATA   MASK6F / '00FFFFFF'X  /        
CUNIX DATA   MASK6F / X'00FFFFFF'  /        
C        
C*****        
      ANDF(I,J)   = IAND(I,J)        
      RSHIFT(I,J) = ISHFT(I,-J)        
C     WHERE         ISHFT(I,-J) IS RIGHT-SHIFT I BY J BITS, ZERO FILL   
C     AND           ISHFT IS SYSTEM ROUTINE        
C        
C UNIX:        
C     REMOVE ABOVE 2 ON-LINE FUNCTIONS IF IAND AND ISHFT SYSTEM        
C     FUNCTIONS ARE NOT AVAILABLE. ANDF AND RSHIFT ARE ALREADY        
C     ENTRY POINTS IN SUBROUTINE MAPFNS.        
C*****        
      NAMEX = NAME        
      IF (NAMEX .LT.    400) GO TO 30        
      IF (NAMEX .NE. LSTNAM) GO TO 20        
      GO TO 40        
C        
C     IFILEX MUST BE PRESET TO ZERO, SEE P.M. P.3.4-18        
C        
   20 IFILEX = 0        
      CALL GETURN (NAMEX)        
      GO TO 40        
   30 NAMEX  = NAMEX - 100        
      IFILEX = IUNITS(NAMEX)        
   40 IF (IFILEX .NE. 0) GO TO 50        
      IF (IENTRY .EQ. ICLOSE) RETURN 1        
      CALL FNAME (NAME,XYZ)        
      WRITE  (NOUT,45) XYZ,NAME        
   45 FORMAT ('0*** RD/WRT/FWDREC/REWIND WITHOUT FIRST OPENNING FILE ', 
     1        2A4,I6)        
      CALL VAXEND        
   50 JBUFF = ANDF(BUFADD(IFILEX),MASK6F)        
      IF (JBUFF  .NE. 0) GO TO 60        
      IF (IENTRY .EQ. ICLOSE) RETURN 1        
      CALL VAXEND        
   60 IRDWRT = RSHIFT(BUFADD(IFILEX),24)        
      RETURN        
      END        
