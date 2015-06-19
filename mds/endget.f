      SUBROUTINE ENDGET (IBLK)        
C        
C     DEPART FROM THE CURRENT STRING IN BUFFER, AND SET POINTER TO THE  
C     BEGINNING OF NEXT STRING        
C        
      INTEGER         IDUM1(1)     ,IDUM2(1)     ,IBLK(11)     ,        
     1                BUFADD(75)        
      COMMON /GINOX / LENGTH,IFILEX,IEOR  ,IOP   ,IENTRY,LSTNAM,        
     1                N     ,NAME  ,IXYZ(3)      ,ITABAD(150)  ,        
     2                NBUFF3,IRDWRT,IUNITS(300)  ,IBLOCK(20)        
      COMMON /ZZZZZZ/ ICORE(1)        
      EQUIVALENCE     (ITABAD(76)  ,BUFADD(1))        
      DATA    MASK6F/ '00FFFFFF'X  /        
CUNIX DATA    MASK6F/ X'00FFFFFF'  /        
C        
      NAME   = IBLK(1)        
      IENTRY = 15        
      IBLOCK( 3) = IBLK( 3)        
      IBLOCK( 8) = IBLK( 8)        
      IBLOCK( 9) = IBLK( 9)        
      IBLOCK(11) = IBLK(11)        
C        
      IFILEX = IBLOCK(11)        
CUNIX JBUFF  =  AND(BUFADD(IFILEX),MASK6F)        
      JBUFF  = IAND(BUFADD(IFILEX),MASK6F)                              
      CALL GINO (*30,*30,ICORE(JBUFF),IDUM1,IDUM2,IRDWRT)        
      IBLK( 3) = IBLOCK( 3)        
      IBLK( 8) = IBLOCK( 8)        
      IBLK( 9) = IBLOCK( 9)        
      IBLK(11) = IBLOCK(11)        
      RETURN        
C        
   30 CALL VAXEND        
      END        
