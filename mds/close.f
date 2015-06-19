      SUBROUTINE CLOSE (NAMEI,IOPI)        
C        
      INTEGER         IDUM1(1),IDUM2(2)        
      COMMON /ZZZZZZ/ ICORE(1)        
      COMMON /GINOX / LENGTH,IFILEX,IEOR,IOP,IENTRY,LSTNAM,N,NAME,      
     1                NTAPE,NBLOCK,NLR,ITABLE(150),NBUFF3,PRVOPN,       
     2                IUNITS(300),IBLOCK(20),KLOSE        
C        
      NAME   = NAMEI        
      IOP    = IOPI        
      IENTRY = 4        
      KLOSE  = 1        
      CALL INIT (*10,IRDWRT,JBUFF)        
C     GO TO 20        
C        
C  10 IENTRY = 22        
   20 CALL GINO (*30,*30,ICORE(JBUFF),IDUM1,IDUM2,IRDWRT)        
      KLOSE  = 0        
   10 RETURN        
C        
   30 CALL ERRTRC (0)        
      END        
