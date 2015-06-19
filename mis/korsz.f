      INTEGER FUNCTION KORSZ (A)        
C        
      INTEGER         A(1),MSG(5),ZCORSZ
      COMMON /MACHIN/ MACH        
      COMMON /L15 L8/ L15,L8,L13
CZZ   COMMON /NOTZZZ/ LLOC
      COMMON /ZZZZZZ/ LLOC
      DATA    MSG   / 4HOPEN,4H COR,4HE = ,4H    ,4H    /
      DATA    LAST  / -1 /
C
      IF (MACH .EQ.  4) GO TO 40
      IF (LAST .EQ. -1) LAST = LLOC + 1
      KORSZ = LAST - LOCFX(A(1))
      IF (L13 .EQ. 0) GO TO 50
C
      IF (MACH .LE. 3) GO TO (50,10,30), MACH
   10 WRITE  (4,20) KORSZ
   20 FORMAT (14X,'OPEN CORE =',I9,' WORDS')
      GO TO 50
   30 CALL INT2A8 (*50,KORSZ,MSG(4))
      CALL CONMSG (MSG,5,0)
      GO TO 50
   40 KORSZ = ZCORSZ(A)
      IF (L13 .NE. 0) CALL CDCKSZ (KORSZ)
C
   50 RETURN        
      END        
