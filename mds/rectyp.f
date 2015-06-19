      SUBROUTINE RECTYP (NAMEI,ITYPE)        
C        
C     INPUT FILE NAME NAMEI        
C     OUTPUT ITYPE = 0, INPUT FILE IS A NORMAL GINO RECORD FILE        
C                  = 1, INPUT FILE IS WRTTEN IN STRING FORMAT        
C        
      INTEGER         IDUM1(1),INCT(2)        
      COMMON /ZZZZZZ/ ICORE(1)        
      COMMON /GINOX / LENGTH,IFILEX,IEOR,IOP,IENTRY,LSTNAM,N,NAME       
C        
      NAME   = NAMEI        
      IENTRY = 18        
      CALL INIT (*10,IRDWRT,JBUFF)        
      CALL GINO (*10,*10,ICORE(JBUFF),IDUM1,INCT,IRDWRT)        
      ITYPE  = INCT(1)        
      RETURN        
C        
C     ERROR IN INIT OR GINO        
C        
   10 CALL VAXEND        
      RETURN        
      END        
