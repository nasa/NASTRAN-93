      SUBROUTINE QOPEN (*,NAMEI,IBUFER,IOP)        
C        
      INTEGER         FILEX        ,OP    ,ENTRY        ,UNITAB(75),    
     1                BUFADD(75)   ,PRVOPN,IBUFER(1)    ,EOR       ,    
     2                IDUM2(2)        
      CHARACTER*6     FLAG(4)        
      COMMON /SYSTEM/ NBUFF ,NOUT        
      COMMON /ZZZZZZ/ ICORE(1)        
      COMMON /L15 L8/ L15   ,L8        
      COMMON /GINOX / LENGTH,FILEX ,EOR   ,OP    ,ENTRY ,LSTNAM    ,    
     1                N     ,NAME  ,NTAPE ,NBLOCK,NLR   ,UNITAB    ,    
     2                BUFADD,NBUFF3,PRVOPN,IUNITS(300)  ,IBLOCK(20),    
     3                KLOSE ,LOCFX1,I2345        
      DATA    FLAG  / 'RDREW ',    'WRTREW',     'RD    ','WRT   ' /    
      DATA            MASK1,        MASK2,        MASK8            /    
CUNIX1                X'01000000',  X'02000000',  X'00008000'      /    
     1                '01000000'X,  '02000000'X,  '00008000'X      /    
C                                                 '00008000'X = 32768   
C        
      NAME  = NAMEI        
      OP    = IOP        
      ENTRY = 1        
      KLOSE = 0        
      NBUFF3= NBUFF - 4        
      IF (BUFADD(FILEX) .NE. 0) GO TO 70        
      JBUFF = IABS(LOCFX(ICORE(1))-LOCFX(IBUFER(1))) + 1        
      IF (MOD(JBUFF,2) .NE. 0) JBUFF = JBUFF + 1        
      JLO = JBUFF - NBUFF        
      JHI = JBUFF + NBUFF        
      DO 50 NDX = 1,75        
      J = BUFADD(NDX)        
      IF (J.NE.0 .AND. (J.GT.JLO .AND. J.LT.JHI)) CALL VAXEND        
   50 CONTINUE        
      BUFADD(FILEX) = JBUFF        
      IF (OP.EQ.1 .OR. OP.EQ.3)        
     1    BUFADD(FILEX) = IOR(BUFADD(FILEX),MASK1)                      
CUNIX1    BUFADD(FILEX) =  OR(BUFADD(FILEX),MASK1)        
      IF (ANDF(NTAPE,MASK8) .NE. 0)        
     1    BUFADD(FILEX) = IOR(BUFADD(FILEX),MASK2)                      
CUNIX1    BUFADD(FILEX) =  OR(BUFADD(FILEX),MASK2)        
      CALL GINO (*100,*100,ICORE(JBUFF),IBUFER,IDUM2,IRDWRT)        
      RETURN        
C        
   70 CALL FNAME (NAME,IDUM2)        
      WRITE  (NOUT,80) IDUM2,FILEX,NAME,IOP,FLAG(IOP+1)        
   80 FORMAT ('0*** NASTRAN FATAL MESSAGE FROM QOPEN, OPENING ',2A4,    
     1       ' FILE WITHOUT FIRST CLOSING IT.', /5X,'LOGICAL UNIT =',   
     2       I5,',  GINO FILE',I5,',  FLAG =',I3,2H (,A6,1H))        
      WRITE  (NOUT,90) BUFADD        
   90 FORMAT (//,' QOPEN/@90 BUFADD 75 WORDS =',5(/2X,15I8))        
      IF (L15 .NE. 0) CALL ERRTRC (0)        
      I = I2345        
      BUFADD(I) = 0        
  100 RETURN 1        
      END        
