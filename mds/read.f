      SUBROUTINE READ (*,*,NAMEI,IARRAY,NI,IEORI,M)        
C        
      INTEGER         MM(2),IARRAY(1)        
      COMMON /GINOX / LENGTH,IFILEX,IEOR,IOP,IENTRY,LSTNAM,N,NAME       
      COMMON /ZZZZZZ/ ICORE(1)        
C*****        
      NAME = NAMEI        
      N    = NI        
      IEOR = IEORI        
      IENTRY = 3        
      CALL INIT (*10,IRDWRT,JBUFF)        
      CALL GINO (*20,*30,ICORE(JBUFF),IARRAY,MM,IRDWRT)        
      RETURN        
C        
   20 RETURN 1        
C        
   30 M = MM(1)        
C         MM(2) = NO. OF UNUSED WORDS IN LAST BUFFER        
      RETURN 2        
C        
C     ERROR IN INIT        
C        
   10 CALL VAXEND        
      RETURN        
      END        
