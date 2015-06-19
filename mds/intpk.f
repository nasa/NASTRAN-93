      SUBROUTINE INTPK (*,NAME,IBLOCK,TYPOUT,FLAG)        
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL*1        DEBUG        
      INTEGER          IBLOCK(16),JBLOCK(1) ,BLOCK(20) ,TYPES(4)  ,     
     1                 BUFF(1)   ,SUB(2)        
      REAL             CORE      ,ELEML     ,ELEM(4)   ,XELEM(4)  ,     
     1                 XELEM1    ,XELEM2        
      DOUBLE PRECISION DELEM1    ,DELEM2    ,DZERO        
      COMMON /SYSTEM/  SYSBUF    ,NOUT      ,NOGO        
      COMMON /ZNTPKX/  ELEML(4)  ,LROW      ,IEOL      ,IEOR        
      COMMON /ZZZZZZ/  CORE(1)        
      COMMON /GINOX /  LENGTH    ,FILEX     ,EOR       ,OP        ,     
     1                 ENTRY     ,LSTNAM    ,N         ,NAMEX     ,     
     2                 NTAPE     ,NBLOCK    ,NLR       ,UNITAB(75),     
     3                 BUFADD(75),NBUFF3    ,PRVOPN    ,UNITS(300),     
     4                 LBLOCK(20),KLOSE     ,LOCFX1    ,R12345    ,     
     5                 R(75)     ,IOPEN(75)        
      COMMON /BUFCOM/  BUFOFF    ,BUFBGN    ,BUFEND    ,BUFFLG        
      COMMON /XXFIAT/  EXFIAT(1)        
      COMMON /XFIAT /  FIAT(1)        
      COMMON /XFIST /  MFIST     ,NFIST     ,FIST(1)        
      EQUIVALENCE      (BLOCK(  1),BNAME )  ,(BLOCK(  2),BTYPE )  ,     
     1                 (BLOCK(  3),BFORM )  ,(BLOCK(  4),BROW  )  ,     
     2                 (BLOCK(  5),BPOINT)  ,(BLOCK(  6),BNBRAV)  ,     
     3                 (BLOCK(  7),BNBWRT)  ,(BLOCK(  8),BFLAG )  ,     
     4                 (BLOCK(  9),BSFT  )  ,(BLOCK( 10),BPREC )  ,     
     5                 (BLOCK( 12),BCOL  )  ,(BLOCK( 13),BCOUNT)  ,     
     6                 (BLOCK( 14),BADDR1)  ,(BLOCK( 15),BADDR2)        
C    7,                (BLOCK( 16),BEOL  )        
      EQUIVALENCE      (LBLOCK( 1),GNAME )  ,(LBLOCK( 2),GTYPE )  ,     
     1                 (LBLOCK( 3),GFORM )  ,(LBLOCK( 4),GROW  )  ,     
     2                 (LBLOCK( 5),GPOINT)  ,(LBLOCK( 6),GNBRAV)  ,     
     3                 (LBLOCK( 7),GNBWRT)  ,(LBLOCK( 8),GFLAG )  ,     
     4                 (LBLOCK( 9),GSFT  )  ,(LBLOCK(10),GPREC )  ,     
     5                 (LBLOCK(11),GFILEX)  ,(LBLOCK(12),GCOL  )        
      EQUIVALENCE      (BUFF  (1),CORE(1))  ,(PRVOPN    ,RDWRT )  ,     
     1                 (XELEM (1) ,DELEM1   ,XELEM1)              ,     
     2                 (XELEM (3) ,DELEM2)  ,(XELEM2    ,XELEM(2))      
      DATA             TYPES     / 1,  2,   2,  4                    /  
      DATA             BUFLGX    , TWO121    , CLR0      , BLANK     /  
     1                 0         , 4095      , 9         , 4H        /  
      DATA             DZERO     , DEBUG     , SUB                   /  
     1                 0.0D+0    ,.FALSE.    , 4HINTP    , 4HK       /  
C VAX:        
      DATA             RECHDR    , MTXHDR    , LASTCW    , EQF       /  
     1                'F1111000'X,'F2222000'X,'F5555000'X,'F7777000'X/  
      DATA             SHEAD     , STRAIL    , DUMSTR    , LSTSTR    /  
     1                'F8888000'X,'F9999000'X,'FAAAA000'X,'FBBBB000'X/  
      DATA             CHEAD     , CTRAIL    , MASKD     , MASKE     /  
     1                '40000000'X,'80000000'X,'FF00FFFF'X,'0000FF00'X/  
      DATA             MASK1     , MASK2     , MASK5     , MASK7     /  
     1                '00001111'X,'00002222'X,'00005555'X,'00007777'X/  
      DATA             MASK8     , MASKA     , MASKB     , MASKF     /  
     1                '00008888'X,'0000AAAA'X,'0000BBBB'X,'0000FFFF'X/  
      DATA             MASK3F    , MASK6F    , MASK2F    , MASKC     /  
     1                '00000FFF'X,'00FFFFFF'X,'FF000000'X,'FF0000FF'X/  
C UNIX:        
C     DATA             RECHDR    , MTXHDR    , LASTCW    , EQF       /  
C    1                X'F1111000',X'F2222000',X'F5555000',X'F7777000'/  
C     DATA             SHEAD     , STRAIL    , DUMSTR    , LSTSTR    /  
C    1                X'F8888000',X'F9999000',X'FAAAA000',X'FBBBB000'/  
C     DATA             CHEAD     , CTRAIL    , MASKD     , MASKE     /  
C    1                X'40000000',X'80000000',X'FF00FFFF',X'0000FF00'/  
C     DATA             MASK1     , MASK2     , MASK5     , MASK7     /  
C    1                X'00001111',X'00002222',X'00005555',X'00007777'/  
C     DATA             MASK8     , MASKA     , MASKB     , MASKF     /  
C    1                X'00008888',X'0000AAAA',X'0000BBBB',X'0000FFFF'/  
C     DATA             MASK3F    , MASK6F    , MASK2F    , MASKC     /  
C    1                X'00000FFF',X'00FFFFFF',X'FF000000',X'FF0000FF'/  
C        
C     MASKF = '0000FFFF'X (OR X'0000FFFF') = 65535        
C        
C*****        
      ANDF(I,J)   = IAND(I,J)        
      ORF (I,J)   = IOR (I,J)        
      LSHIFT(I,J) = ISHFT(I, J)        
      RSHIFT(I,J) = ISHFT(I,-J)        
C     WHERE         ISHFT(I,+J) IS  LEFT-SHIFT I BY J BITS, ZERO FILL   
C                   ISHFT(I,-J) IS RIGHT-SHIFT I BY J BITS, ZERO FILL   
C     AND           ISHFT IS SYSTEM ROUTINE        
C        
C UNIX:        
C     REMOVE ABOVE 4 ON-LINE FUNCTIONS IF IAND, IOR AND ISHFT SYSTEM    
C     FUNCTIONS ARE NOT AVAILABLE. ANDF, ORF AND L/RSHIFT ARE ALREADY   
C     ENTRY POINTS IN SUBROUTINE MAPFNS.        
C*****        
      IF (TYPOUT .EQ. 0) CALL MESAGE (-37,0,SUB)        
      IF (FLAG   .NE. 2) GO TO 10         !  FLAG IS SET TO 2 IN UNPACK 
      BFLAG = IBLOCK(8)        
      GO TO 20        
   10 BFLAG = MASKC        
      IF (FLAG .EQ. 0) BFLAG = MASKD        
C        
   20 BNAME = NAME        
      IRET  = 0        
      NWPUP = 0        
C        
C     CALL GETSTR (*50,BLOCK)        
      ASSIGN 30 TO GETSTR        
      ASSIGN 50 TO ALTRTN        
      GO TO 900        
   30 IADD = 0        
      IF (FLAG .NE. 0) GO TO 40        
      IEOL = 0        
      IEOR = 0        
C        
   40 IF (TYPOUT .LT. 0) IADD = 16        
      OUTYPE = IABS(TYPOUT)        
      BADDR2 = LSHIFT(TYPES(OUTYPE),24)        
      INDEX  = 4*(BTYPE-1) + OUTYPE        
      BRNCH  = -99        
      IF (INDEX.LT.1 .OR. INDEX.GT.16) GO TO 100        
      INDEX  = INDEX + IADD        
      BADDR1 = ORF(LSHIFT(TYPES(BTYPE),24),INDEX)        
      GO TO 80        
C        
   50 IRET = 1        
      IF (FLAG) 120,140,120        
C        
   80 IF (FLAG) 120, 90,120        
C        
   90 NWPUP = RSHIFT(BADDR1,24)        
      BRNCH = ANDF(BADDR1,MASK6F)        
      IF (BRNCH.GE.1 .AND. BRNCH.LE.32) GO TO 140        
  100 WRITE (NOUT,110) BRNCH,INDEX,BTYPE,OUTYPE,TYPES(BTYPE),FLAG,IRET, 
     1                 BADDR1,MASK6F        
  110 FORMAT ('0*** SYSTEN FATAL ERROR - INTPK@110', /5X,        
     1     'BRNCH,INDEX,BTYPE,OUTYPE,TYPES(BTYPE),FLAG,IRET',        
     2      14X,'BADDR1',14X,'MASK6F=', /5X,2I5,I6,I7,I12,I6,I5,2O20)   
      CALL VAXERR        
C        
  120 DO 130 I = 1,16        
      IBLOCK(I) = BLOCK(I)        
  130 CONTINUE        
C        
  140 IF (IRET .EQ. 1) RETURN 1        
      RETURN        
C        
C*****        
      ENTRY INTPKI (ELEM,IROW,NAME,JBLOCK,JEOL)        
C     =========================================        
C     MULTI-COLUMN ENTRY POINT        
C*****        
C        
C     READ SUCCESSIVE NON-ZERO ELEMENTS OF THE COLUMN.        
C        
      JFLAG = 1        
      DO 150 I = 1,15        
      BLOCK(I) = JBLOCK(I)        
  150 CONTINUE        
      NWPUP = RSHIFT(BADDR1,24)        
      BRNCH = ANDF(BADDR1,MASK6F)        
      IF (BRNCH.LT.1 .OR. BRNCH.GT.32) GO TO 100        
      GO TO 160        
C        
C*****        
      ENTRY ZNTPKI        
C     ============        
C     SINGLE-COLUMN ENTRY POINT        
C*****        
C        
      JFLAG = 0        
      IF (NWPUP .NE. 0) GO TO 160        
      NWPUP = RSHIFT(BADDR1,24)        
      BRNCH = ANDF(BADDR1,MASK6F)        
      IF (BRNCH.LT.1 .OR. BRNCH.GT.32) GO TO 100        
C        
  160 MROW  = 0        
  170 J = BPOINT        
      DO 180 I = 1,NWPUP        
      XELEM(I) = CORE(J)        
  180 J = J + 1        
      BPOINT = J        
C        
C        
C     NEXT 23 LINES ARE NO LONGER USED SINCE BLDPK DOES NOT MERGE       
C     STRINGS THAT ARE SEPERATED BY A FEW (3 OR LESS) ZERO ELEMENTS     
C        
C     NEXT 5 LINES WERE 4/89 CODE        
C     THE MISTAKE WAS THAT IT RETURNED ZERO ELEMENT TO THE CALLER WITH  
C     WRONG INFORMATION        
C        
C     DO 190 I = 1,NWPUP        
C     IF (XELEM(I) .NE. 0.0) GO TO 200        
C 190 CONTINUE        
C     BNBWRT = BNBWRT + 1        
C     GO TO 170        
C        
C     NEXT FEW LINES WERE FROM 3/90.        
C     IT MAY NOT BE 100 PERCENT FOOL PROOF        
C        
C     A ZERO IS ENCOUNTERED.        
C     MUST GET BY THIS ZERO WITHOUT RETURNING TO CALLER        
C        
C     NEXT 2 LINES MAY NOT BE NEEDED    4/90 (TEST: D01051A.NID)        
C     LROW = BROW + BNBWRT        
C     IF (JFLAG .EQ. 1) IROW = LROW        
C     MROW = -1        
C     GO TO 420        
C        
C        
C        OUT=    SP,  DP, SCX, DCX   !  IN=        
  200 GO TO (   400, 220, 210, 220,  !  +SP        
     1          240, 400, 240, 230,  !  +DP        
     2          210, 220, 400, 250,  ! +SCX        
     3          240, 230, 260, 400,  ! +DCX        
     4          280, 320, 310, 320,  !  -SP        
     5          340, 300, 340, 330,  !  -DP        
     6          310, 320, 270, 350,  ! -SCX        
     7          340, 330, 360, 290   ! -DCX        
     8      ),  BRNCH        
C        
  210 XELEM2 = 0.0                   ! SCX= +SP,  SP=+SCX        
      GO TO 400        
  220 DELEM1 = DBLE(XELEM1)          ! DCX= +SP        
  230 DELEM2 = DZERO                 ! DP = +SP,  DP=+SCX,  DCX= +DP    
      GO TO 400        
  240 XELEM1 = SNGL(DELEM1)          ! SP = +DP,  SP=+DCX,  SCX= +DP    
      XELEM2 = 0.0        
      GO TO 400        
  250 DELEM2 = DBLE(XELEM2)          ! DCX=+SCX        
      DELEM1 = DBLE(XELEM1)        
      GO TO 400        
  260 XELEM1 = SNGL(DELEM1)          ! SCX=+DCX        
      XELEM2 = SNGL(DELEM2)        
      GO TO 400        
C        
  270 XELEM2 = -XELEM2               ! SCX=-SCX        
  280 XELEM1 = -XELEM1               ! SP = -SP        
      GO TO 400        
  290 DELEM2 = -DELEM2               ! DCX=-SCX        
  300 DELEM1 = -DELEM1               ! DP = -DP        
      GO TO 400        
  310 XELEM1 = -XELEM1               ! CSX= -SP,  SP=-SCX        
      XELEM2 =  0.0        
      GO TO 400        
  320 DELEM1 = -DBLE(XELEM1)         ! DCX= -SP,  DP=-SCX,  DP = -SP    
      DELEM2 =  DZERO        
      GO TO 400        
  330 DELEM1 = -DELEM1               ! DCX= -DP,  DP=-DCX        
      DELEM2 =  DZERO        
      GO TO 400        
  340 XELEM1 = -SNGL(DELEM1)         ! SP = -DP,  SP=-DCX,  SCX= -DP    
      XELEM2 =  0.0        
      GO TO 400        
  350 DELEM2 = -DBLE(XELEM2)         ! DCX=-SCX        
      DELEM1 = -DBLE(XELEM1)        
      GO TO 400        
  360 XELEM1 = -SNGL(DELEM1)         ! SCX=-DCX        
      XELEM2 = -SNGL(DELEM2)        
C        
  400 MROW = BROW + BNBWRT        
      IF (JFLAG .EQ. 0) GO TO 410        
      IROW    = MROW        
      ELEM(1) = XELEM 1        
      ELEM(2) = XELEM 2        
      ELEM(3) = XELEM(3)        
      ELEM(4) = XELEM(4)        
      GO TO 420        
C        
  410 LROW     = MROW        
      ELEML(1) = XELEM 1        
      ELEML(2) = XELEM 2        
      ELEML(3) = XELEM(3)        
      ELEML(4) = XELEM(4)        
C        
C     IF LAST TERM IN STRING, GET NEW STRING DEFINITION.        
C        
  420 IF (BNBWRT+1 .LT. BNBRAV) GO TO 440        
C        
C     CALL ENDGET (BLOCK)        
C     CALL GETSTR (*460,BLOCK)        
      ASSIGN 900 TO ENDGET        
      ASSIGN 450 TO GETSTR        
      ASSIGN 460 TO ALTRTN        
      GO TO 700        
C        
C     EXIT HERE WHEN NOT END-OF-COLUMN.        
C        
  440 BNBWRT = BNBWRT + 1        
  450 IF (ANDF(BFLAG,MASKE).EQ.0 .AND. JFLAG.NE.0) JEOL = 0        
C        
C     NEXT 23 LINES FOR LOGIC THAT INVOLVE MERGING STRINGS WHICH WERE   
C     SEPERATED WITH A FEW ZERO ELEMENTS        
C        
C     IF (MROW .EQ. -1) GO TO 160        
C     IF (JFLAG) 1600,1600,480        
C        
C     EXIT HERE ON END-OF-COLUMN.        
C        
C 460 IF (JFLAG .EQ. 1) GO TO 470        
C     IEOL = 1        
C     IEOR = 1        
C     IF (MROW .NE. -1) GO TO 1600        
C     DO 465 I = 1,4        
C 465 ELEML(I) = 0.0        
C     GO TO 1600        
C        
C 470 JEOL = 1        
C 480 DO 490 I = 1,16        
C     JBLOCK(I) = BLOCK(I)        
C 490 CONTINUE        
C     IF (MROW .NE. -1) GO TO 1600        
C     DO 495 I = 1,4        
C 495 ELEM(I) = 0.0        
C     GO TO 1600        
C        
C     NEXT 18 LINES ARE FOR LOGIC THAT DOES NOT INVOLVE STRING MERGING  
C        
      GO TO 480        
C        
C     EXIT HERE ON END-OF-COLUMN.        
C        
  460 IF (JFLAG .NE. 0) GO TO 470        
      IEOL = 1        
      IEOR = 1        
C     GO TO 480        
      GO TO 1600        
C        
  470 JEOL = 1        
C        
C 480 IF (JFLAG .EQ. 0) IF (NROW) 160,1600,1600        
  480 IF (JFLAG .EQ. 0) GO TO 1600        
      DO 490 I = 1,16        
      JBLOCK(I) = BLOCK(I)        
  490 CONTINUE        
C     IF (MROW) 150,1600,1600        
      GO TO 1600        
C        
C        
C        
C     ----- END OF INTPK ------        
C        
C     THE FOLLOWINGS ARE EXCERTS FROM VARIOUS SUPPORTING ROUTINES       
C     ===========================================================       
C        
C*****        
C     SUBROUTINE DCODE (IBUFF,IBUFFE,IBUFFM)        
C*****        
C        
  500 IBUFFE = ANDF(IBUFF,MASK3F)        
      IBUFF  = RSHIFT(IBUFF,12)        
      IBUFFM = ANDF(IBUFF,MASKF)        
      IBUFF  = RSHIFT(IBUFF,16)        
      GO TO DCBACK, (1140,1190,1210)        
C        
  550 WRITE  (NOUT,560) IMHERE        
  560 FORMAT (/,' *** FATAL ERROR IN INTPK @',I5)        
      CALL ERRTRC (0)        
      CALL MESAGE (-61,0,0)        
C        
C*****        
C     SUBROUTINE INIT (*,RDWRT,JBUFF)        
C*****        
C        
  600 NAM = NAMEX        
      IF (NAM .LT.    400) GO TO 620        
      IF (NAM .NE. LSTNAM) GO TO 800        
      IMHERE= 600        
      GO TO 630        
  620 NAM   = NAM - 100        
      FILEX = UNITS(NAM)        
      IMHERE= 620        
  630 IF (FILEX .EQ. 0) GO TO 640        
      JBUFF = ANDF(BUFADD(FILEX),MASK6F)        
      IMHERE= 640        
      IF (JBUFF .EQ. 0) GO TO 640        
      RDWRT = RSHIFT(BUFADD(FILEX),24)        
      GO TO 920        
C        
  640 WRITE  (NOUT,650) NAMEX,NAM,LSTNAM,FILEX,JBUFF        
  650 FORMAT ('0*** GINO ERROR,  FILE NOT FOUND OR NOT ALLOCATED BY ',  
     1        'XSFA, OR USER DMAP ERROR', /5X,        
     1        'NAMEX,NAM,LSTNAM,FILEX,JBUFF =',5I7)        
      IF (NAM.LE.100 .AND. NAM.GE.400) GO TO 550        
      CALL FNAME (NAM,CORE)        
      J = 2        
      WRITE  (NOUT,660) NAM,CORE(1),CORE(J)        
  660 FORMAT (5X,I3,' IS FILE ',2A4)        
      IF (FILEX .NE. 0) GO TO 550        
      WRITE (NOUT,670) (UNITS(J),J=  1,100)        
      WRITE (NOUT,680) (UNITS(J),J=101,200)        
      WRITE (NOUT,680) (UNITS(J),J=201,300)        
  670 FORMAT (/5X,'UNITS =',/,(1X,30I4))        
  680 FORMAT (1X,30I4)        
      GO TO 550        
C        
C*****        
C     SUBROUTINE ENDGET (BLOCK)        
C*****        
C        
  700 NAMEX = BLOCK(1)        
      ENTRY = 15        
      LBLOCK(3) = BLOCK(3)        
      LBLOCK(9) = BLOCK(9)        
      LBLOCK(11)= BLOCK(11)        
C        
      FILEX = LBLOCK(11)        
      JBUFF = ANDF(BUFADD(FILEX),MASK6F)        
C        
C     CALL GINO (*720,*720,CORE(JBUFF),IDUM1,IDUM2,RDWRT)        
      ASSIGN 710 TO GINO        
      ASSIGN 720 TO RET1        
C     ASSIGN 720 TO RET2        
      GO TO 1100        
  710 GO TO ENDGET, (900)        
C        
  720 IMHERE = 720        
      GO TO 550        
C        
C*****        
C     SUBROUTINE GETURN (NAMDUM)        
C*****        
C        
  800 NN = 2*NFIST - 1        
      DO 810 I = 1,NN,2        
      IF (FIST(I) .EQ. NAMEX) GO TO 820        
  810 CONTINUE        
      FILEX  = 0        
      IMHERE = 810        
      GO TO 870        
  820 J = FIST(I+1)        
      IF (J) 830,840,850        
  830 J = -J        
  840 BUFF(1)= EXFIAT(J+1)        
      GO TO 860        
  850 IIII= FIAT(J+1)        
  860 FILEX  = ANDF(IIII,32767)        
      NTAPE  = ANDF(IIII,32768)        
      PRVOPN = RSHIFT(UNITAB(FILEX),28)        
      UNITAB(FILEX) = RSHIFT(LSHIFT(UNITAB(FILEX),4),4)        
      NBLOCK = RSHIFT(UNITAB(FILEX),12)        
      NLR    = ANDF(UNITAB(FILEX),TWO121)        
      IMHERE = 860        
      IF (NAMEX .GE. 400) GO TO 870        
      UNITS(NAMEX-100) = FILEX        
      IMHERE = 870        
  870 GO TO 630        
C        
C*****        
C     SUBROUTINE GETSTR (*,BLOCK)        
C*****        
C        
  900 NAMEX = BLOCK(1)        
      ENTRY = 14        
      LBLOCK(8) = BLOCK(8)        
      IF (ANDF(LBLOCK(8),MASK2F) .NE. MASK2F) GO TO 910        
C        
C     CALL INIT (*960,RDWRT,JBUFF)        
C     GO TO 920        
      GO TO 600        
  910 LBLOCK(10) = BLOCK(10)        
      LBLOCK(11) = BLOCK(11)        
      FILEX = LBLOCK(11)        
      JBUFF = ANDF(BUFADD(FILEX),MASK6F)        
C        
C 920 CALL GINO (*960,*990,CORE(JBUFF),IDUM1,IDUM2,RDWRT)        
  920 ASSIGN 930 TO GINO        
      ASSIGN 960 TO RET1        
C     ASSIGN 990 TO RET2        
      GO TO 1100        
  930 DO 940 I = 2,12        
      BLOCK(I) = LBLOCK(I)        
  940 CONTINUE        
C     IF (DEBUG) WRITE (NOUT,950) BLOCK        
C 950 FORMAT (' INTPK/@950 BLOCK=',4I4,I7,2I4,O20,5I3,2I8,5I2)        
      IF (BPREC .EQ. 1) BPOINT = BPOINT*2 - 1        
      GO TO GETSTR, (30,450)        
C        
  960 DO 970 I = 2,12        
      BLOCK(I) = LBLOCK(I)        
  970 CONTINUE        
C     IF (DEBUG) WRITE (NOUT,980) BLOCK        
C 980 FORMAT (' INTPK/@980 BLOCK=',4I4,I7,2I4,O20,5I3,2I8,5I2)        
      GO TO ALTRTN, (50,460)        
C        
C 990 IMHERE = 990        
C     GO TO 550        
C        
C*****        
C     SUBROUTINE GINOIO (*,OPCODE,B,MHERE)        
C        
C     ENTRY = 3 ONLY - READ ONE BLOCK        
C     B IS BUFF, OPCODE AND MHERE NOT USED HERE        
C*****        
C        
 1000 IF (R12345 .NE. -1234567890) GO TO 1020        
      IF (FILEX .LE. 1) CALL MESAGE (-37,0,SUB)        
      IF (IOPEN(FILEX) .EQ. 0) GO TO 1020        
C        
C ... READ ONE BLOCK:        
C     IF (DEBUG) WRITE (NOUT,1010)        
C1010 FORMAT (/,' ===== INTPK-GINOIO/READ @1010 =====')        
      IMHERE = 1010        
      I = R(FILEX)        
      READ (FILEX,REC=I,ERR=1050) (BUFF(J),J=JBUFF4,JEND)        
      R(FILEX) = R(FILEX) + 1        
      IF (KLOSE .NE. 1) GO TO 1040        
      IOPEN(FILEX) = 0        
      CLOSE (UNIT=FILEX,STATUS='KEEP')        
      GO TO 1040        
 1020 CALL GINOIO (*1050,3,BUFF(JBUFF4),1020)        
C     IF (DEBUG) WRITE (NOUT,1030)        
C1030 FORMAT (/,' ===== INTPK-GINOIO TO READ @1020 =====')        
 1040 GO TO GOBACK, (1160,1260,1410,1430)        
 1050 GO TO ERBACK, (1410,1550,1060)        
 1060 GO TO 550        
C        
C*****        
C     SUBROUTINE GINO (*,*,BUFF,A,INCT,RDWRT)        
C        
C     INTEGER      A(1),INCT(2),BUFF(1)        
C                  A,RDWRT,INCT ARE NOT USED HERE        
C        
C     ENTRY = 14,  CALL GETSTR (*,BLOCK)        
C     ENTRY = 15,  CALL ENDGET (BLOCK)        
C                  BLOCK(1) = NAME OF ALL STRING CALLS        
C*****        
C        
 1100 BUFOFF = JBUFF        
      BUFBGN = 1        
      BUFEND = NBUFF3        
      BUFFLG = BUFLGX        
      LOCFX1 = LOCFX(CORE(1))        
      JBUFF1 = JBUFF  - 1        
      JBUFF2 = JBUFF1 + 2        
      JBUFF3 = JBUFF1 + 3        
      JBUFF4 = JBUFF1 + 4        
      JEND   = JBUFF3 + NBUFF3        
C     IF (DEBUG) WRITE (NOUT,1110) LBLOCK        
C1110 FORMAT ('  INTPK/@1110 LBLOCK=',4I4,I7,2I4,O20,5I3,2I8,5I2)       
C        
      IF (ENTRY-14) 1120,1120,1300        
C        
C ... GINO-GETSTR        
C        
 1120 CONTINUE        
      CBP = BUFF(JBUFF2)        
      CLR = BUFF(JBUFF3)        
      IF (ANDF(GFLAG,MASK2F) .NE. MASK2F) GO TO 1200        
      IMHERE = 1125        
      IF (CBP .GT. BUFF(8+JBUFF1)) GO TO 550        
 1130 IBUFF = BUFF(CLR+JBUFF1)        
C        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1140 TO DCBACK        
      GO TO 500        
 1140 IF (IBUFF.NE.15 .OR. IBUFFM.EQ.MASK7) GO TO 1150        
      IF (IBUFFM.EQ.MASK2 .OR. IBUFFM.EQ.MASK1) GO TO 1170        
      IMHERE = 1145        
      IF (IBUFFM .NE. MASK5) GO TO 550        
C        
C1150 CALL GINOIO (*1060,RD,BUFF(JBUFF4),1150)        
 1150 ASSIGN 1160 TO GOBACK        
      ASSIGN 1060 TO ERBACK        
      IMHERE = 1155        
      GO TO 1000        
 1160 CBP = CLR0        
      CLR = CLR0        
      GO TO 1130        
 1170 GFLAG = ANDF(GFLAG,MASK6F)        
      CBP = CBP + 1        
      IF (IBUFFE .NE. 0) GO TO 1180        
      IF (CLR+2 .GE. BUFF(8+JBUFF1)) GO TO 1150        
      IMHERE = 1175        
      GO TO 550        
 1180 IBUFF = BUFF(CBP+JBUFF1)        
C        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1190 TO DCBACK        
      GO TO 500        
 1190 IF (IBUFF .NE. 4) GO TO 1150        
      GCOL   = IBUFFM        
      GTYPE  = IBUFFE/16        
      GFORM  = ANDF(IBUFFE,15)        
      GFILEX = FILEX        
      GSFT   = 0        
      IF (GTYPE.EQ.2 .OR. GTYPE.EQ.3) GSFT = 1        
      IF (GTYPE .EQ. 4) GSFT = 2        
      GPREC  = 0        
      IF (GTYPE.EQ.2 .OR. GTYPE.EQ.4) GPREC = 1        
 1200 CBP = CBP + 1        
      IBUFF = BUFF(CBP+JBUFF1)        
C        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1210 TO DCBACK        
      GO TO 500        
 1210 IF (IBUFF  .NE.    15) GO TO 1220        
      IF (IBUFFM .EQ. MASK8) GO TO 1230        
      IF (IBUFFM .EQ. MASKB) GO TO 1250        
      IF (IBUFFM .EQ. MASKA) GO TO 1270        
      IMHERE = 1215        
      GO TO 550        
 1220 IF (IBUFF .EQ. 8) GO TO 1280        
      IF (IBUFF .EQ. 3) GO TO 1250        
      IMHERE = 1225        
      GO TO 550        
 1230 GROW   = BUFF(CBP+1+JBUFF1)        
      GNBRAV = IBUFFE        
      GNBWRT = 0        
      GPOINT = LOCFX(BUFF(CBP+2+JBUFF1)) - LOCFX1        
      IF (GPREC .NE. 0) GPOINT  = GPOINT/2        
      GPOINT = GPOINT + 1        
      BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM = NAMEX        
C     IF (DEBUG) WRITE (NOUT,1240) LBLOCK        
C1240 FORMAT (3X,'INTPK/@1240 LBLOCK=',4I4,I7,2I4,O20,5I3,2I8,5I2)      
      GO TO GINO, (710,930)        
C        
C1250 CALL GINOIO (*1060,RD,BUFF(JBUFF4),1250)        
 1250 ASSIGN 1260 TO GOBACK        
      ASSIGN 1060 TO ERBACK        
      IMHERE = 1255        
      GO TO 1000        
 1260 CBP = CLR0        
      CLR = CLR0        
      GO TO 1200        
 1270 CBP = CBP + IBUFFE        
      GO TO 1200        
 1280 ASSIGN 1290 TO RET3        
      GO TO 1400        
C        
 1290 GFLAG = 1        
      BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM = NAMEX        
      GO TO RET1, (720,960)        
C        
C ... GINO-ENDGET        
C        
 1300 CONTINUE        
      CBP = BUFF(JBUFF2)        
      CLR = BUFF(JBUFF3)        
      CBP = LSHIFT(ANDF(BUFF(CBP+JBUFF1),TWO121),GSFT) + CBP + 1        
      IF (GFORM .EQ. 0) GO TO 1310        
      CBP = CBP + 2        
 1310 BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM = NAMEX        
C     IF (DEBUG) WRITE (NOUT,1320) LBLOCK        
C1320 FORMAT (4X,'INTPK/@1320 LBLOCK=',4I4,I7,2I4,O20,5I3,2I8,5I2)      
      GO TO GINO, (710,930)        
C        
C     INTERNAL ROUTINE TO SKIP TO THE END OF A LOGICAL RECORD.        
C        
 1400 CONTINUE        
      CBP  = ANDF(BUFF(CLR+JBUFF1),TWO121) + CLR + 1        
      IF (RSHIFT(BUFF(CBP+JBUFF1),28) .EQ. 3) GO TO 1420        
      CLR  = CBP + 1        
      IEND = RSHIFT(LSHIFT(BUFF(CLR+JBUFF1),4),16)        
      IF (IEND .NE. MASK5) GO TO 1440        
C        
C     CALL GINOIO (*1410,RD,BUFF(JBUFF4),1405)        
      ASSIGN 1410 TO GOBACK        
      ASSIGN 1410 TO ERBACK        
      GO TO 1000        
 1410 CBP = CLR0        
      CLR = CLR0        
      GO TO 1450        
C        
C1420 CALL GINOIO (*1550,RD,BUFF(JBUFF4),1420)        
 1420 ASSIGN 1430 TO GOBACK        
      ASSIGN 1550 TO ERBACK        
      GO TO 1000        
 1430 CBP = CLR0        
      CLR = CLR0        
      GO TO 1400        
 1440 CLR = CBP + 1        
      CBP = CLR        
 1450 GO TO RET3, (1290)        
 1550 CBP = CLR0        
      CLR = CLR0        
      GO TO RET1, (720,960)        
C        
 1600 RETURN        
      END        
