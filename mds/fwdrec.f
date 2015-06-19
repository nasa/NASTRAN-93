      SUBROUTINE FWDREC (*,NAMEI)        
      INTEGER         IDUM1(1),IDUM2(2)        
      COMMON /ZZZZZZ/ ICORE(10)        
      COMMON /GINOX / LENGTH,IFILEX,IEOR,IOP,IENTRY,LSTNAM,N,NAME       
C        
      NAME = NAMEI        
      IENTRY = 6        
      CALL INIT (*30,IRDWRT,JBUFF)        
      CALL GINO (*20,*20,ICORE(JBUFF),IDUM1,IDUM2,IRDWRT)        
      RETURN        
   20 RETURN 1        
C        
C     ERROR IN INIT        
C        
   30 CALL VAXEND        
      RETURN        
      END        
