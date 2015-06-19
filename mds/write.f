      SUBROUTINE WRITE (NAMEI,IARRAY,NI,IEORI)        
      INTEGER         IARRAY(1),IDUM2(2)        
      COMMON /ZZZZZZ/ ICORE(1)        
      COMMON /GINOX / LENGTH,IFILEX,IEOR,IOP,IENTRY,LSTNAM,N,NAME       
C*****        
      NAME = NAMEI        
      N    = NI        
      IEOR = IEORI        
      IENTRY = 2        
      CALL INIT (*10,IRDWRT,JBUFF)        
      CALL GINO (*10,*10,ICORE(JBUFF),IARRAY,IDUM2,IRDWRT)        
      RETURN        
C        
C     ERROR IN INIT OR GINO        
C        
   10 CALL VAXEND        
      RETURN        
      END        
