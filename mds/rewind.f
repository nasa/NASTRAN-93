      SUBROUTINE REWIND (NAMEI)        
      INTEGER         IDUM1(1),IDUM2(2)        
      COMMON /ZZZZZZ/ ICORE(1)        
      COMMON /GINOX / LENGTH,IFILEX,IEOR,IOP,IENTRY,LSTNAM,N,NAME       
C*****        
      NAME = NAMEI        
      IENTRY = 8        
      CALL INIT (*10,IRDWRT,JBUFF)        
      CALL GINO (*10,*10,ICORE(JBUFF),IDUM1,IDUM2,IRDWRT)        
      RETURN        
C        
C     ERROR INIT OR GINO        
C        
   10 CALL VAXEND        
      RETURN        
      END        
