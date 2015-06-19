      SUBROUTINE PUTSTR (IBLK)        
C        
      INTEGER         IDUM1(1)     ,IDUM2(2)     ,IBLK(12)     ,        
     1                BUFADD(75)        
      COMMON /GINOX / LENGTH,IFILEX,IEOR  ,IOP   ,IENTRY,LSTNAM,        
     1                N     ,NAME  ,IXYZ(3)      ,ITABAD(150)  ,        
     2                NBUFF3,IRDWRT,IUNITS(300)  ,IBLOCK(20)        
      COMMON /ZZZZZZ/ ICORE(1)        
      EQUIVALENCE     (ITABAD(76),BUFADD(1))        
      DATA    MASK2F, MASK6F/ 'FF000000'X  ,'00FFFFFF'X  /        
CUNIX DATA    MASK2F, MASK6F/ X'FF000000'  ,X'00FFFFFF'  /        
C        
      NAME   = IBLK(1)        
      IENTRY = 12        
      IBLOCK(3) = IBLK(3)        
      IBLOCK(8) = IBLK(8)        
C        
CUNIX IF ( AND(IBLOCK(8),MASK2F) .NE. MASK2F) GO TO 20        
      IF (IAND(IBLOCK(8),MASK2F) .NE. MASK2F) GO TO 20                  
      IBLOCK( 2) = IBLK( 2)        
      IBLOCK(12) = IBLK(12)        
      CALL INIT (*50,IRDWRT,JBUFF)        
      GO TO 30        
   20 IBLOCK( 9) = IBLK( 9)        
      IBLOCK(10) = IBLK(10)        
      IBLOCK(11) = IBLK(11)        
      IFILEX = IBLOCK(11)        
CUNIX JBUFF  =  AND(BUFADD(IFILEX),MASK6F)        
      JBUFF  = IAND(BUFADD(IFILEX),MASK6F)                              
   30 CALL GINO (*50,*50,ICORE(JBUFF),IDUM1,IDUM2,IRDWRT)        
C     DO 40 I = 2,12     ! FOR SOME REASON, 'I = 2,12' WILL NOT WORK    
      DO 40 I = 5,11        
      IBLK(I) = IBLOCK(I)        
   40 CONTINUE        
      RETURN        
   50 CALL VAXEND        
      RETURN        
      END        
