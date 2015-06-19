      SUBROUTINE DEFCOR        
C        
C     THIS ROUTINE DEFINES THE ACTUAL OPEN CORE FOR THE UNIX MACHINE   
C     THE MAXIMUM OPEN CORE SIZE IS DEFINED BY zzcore.F
C        
      INTEGER         HICORE        
      COMMON /MACHIN/ MACH        
      COMMON /SYSTEM/ IDUM,NOUT,JDUM(28),HICORE        
      COMMON /VAXOPC/ LENOPC        
      COMMON /ZZZZZZ/ IZ(1)        
C        
CWKBR IF (MACH .GT. 5) MAXZ = IZ(1)        
CWKBI
      MAXZ = IZ(1)
      IF (HICORE .GT. MAXZ) GO TO 20        
      LENOPC = HICORE        
      IZ(1)  = LOCFX(IZ(HICORE))        
      RETURN        
C        
 20   WRITE  (NOUT,30) MAXZ        
 30   FORMAT (//,' *** USER FATAL MESSAGE - HICORE REQUEST EXCEEDS',    
     1           ' MAXIMUM SIZE OF',I8,' SET BY SUBROUTINE DEFCOR')     
      CALL PEXIT        
      END        
