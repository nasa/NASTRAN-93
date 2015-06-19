      SUBROUTINE RDBLK (*,NAMEI,FIRST,LEFT)        
      INTEGER         IDUM1(1),INCT(2),FIRST        
      COMMON /ZZZZZZ/ ICORE(1)        
      COMMON /GINOX / LENGTH,IFILEX,IEOR,IOP,IENTRY,LSTNAM,N,NAME       
C*****        
      NAME = NAMEI        
      IENTRY = 20        
      CALL INIT (*30,IRDWRT,JBUFF)        
      INCT(1) = FIRST        
      CALL GINO (*20,*30,ICORE(JBUFF),IDUM1,INCT,IRDWRT)        
      RETURN        
C        
   20 LEFT = INCT(2)        
      RETURN 1        
C        
C     ERROR IN INIT OR GINO        
C        
   30 CALL VAXEND        
      RETURN        
      END        
