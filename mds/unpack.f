      SUBROUTINE UNPACK (*,NAME,ELEM)        
C        
C     THIS ROUTINE UNPACKS A MATRIX WHICH WAS WRITTEN IN PACK FORM      
C     A UNIFIED VERSION FOR SPEED IMPROVEMENT.        
C     ASSEMBLED BY G.CHAN/SPERRY, 8/86        
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL*1        DEBUG        
      INTEGER          BLOCK(20) ,TYPES(4)  ,XYZ(2)    ,BUFF(1)        
C     INTEGER          IDUM2(2)        
      REAL             COREX(1)  ,ELEM(1)   ,ELEMT(4)        
      DOUBLE PRECISION DELEM1    ,DELEM2    ,DZERO        
      COMMON /UNPAKX/  TYPOUT    ,IROW      ,LROW      ,INCR        
      COMMON /SYSTEM/  SYSBUF    ,NOUT      ,NOGO        
      COMMON /ZZZZZZ/  CORE(1)        
      COMMON /GINOX /  LENGTH    ,FILEX     ,EOR       ,OP        ,      
     1                 ENTRY     ,LSTNAM    ,N         ,NAMEX     ,      
     2                 NTAPE     ,NBLOCK    ,NLR       ,UNITAB(75),      
     3                 BUFADD(75),NBUFF3    ,PRVOPN    ,UNITS(300),      
     4                 IBLOCK(20),KLOSE     ,LOCFX1    ,R12345    ,      
     5                 R(75)     ,IOPEN(75)        
      COMMON /BUFCOM/  BUFOFF    ,BUFBGN    ,BUFEND    ,BUFFLG        
      COMMON /XXFIAT/  EXFIAT(1)        
      COMMON /XFIAT /  FIAT(1)        
      COMMON /XFIST /  MFIST     ,NFIST     ,FIST(1)        
      EQUIVALENCE      (BLOCK( 1),BNAME  )  ,(BLOCK( 2),BTYPE  )  ,      
     1                 (BLOCK( 3),BFORM  )  ,(BLOCK( 4),BROW   )  ,      
     2                 (BLOCK( 5),BPOINT )  ,(BLOCK( 6),BNBRAV )  ,      
     3                 (BLOCK( 7),BNBWRT )  ,(BLOCK( 8),BFLAG  )  ,      
     4                 (BLOCK( 9),BSFT   )  ,(BLOCK(10),BPREC  )  ,      
     5                 (BLOCK(12),BCOL   )  ,(BLOCK(13),BCOUNT )  ,      
     6                 (BLOCK(14),BADDR1 )  ,(BLOCK(15),BADDR2 )        
      EQUIVALENCE      (IBLOCK( 1),GNAME )  ,(IBLOCK( 2),GTYPE )  ,      
     1                 (IBLOCK( 3),GFORM )  ,(IBLOCK( 4),GROW  )  ,      
     2                 (IBLOCK( 5),GPOINT)  ,(IBLOCK( 6),GNBRAV)  ,      
     3                 (IBLOCK( 7),GNBWRT)  ,(IBLOCK( 8),GFLAG )  ,      
     4                 (IBLOCK( 9),GSFT  )  ,(IBLOCK(10),GPREC )  ,      
     5                 (IBLOCK(11),GFILEX)  ,(IBLOCK(12),GCOL  )        
      EQUIVALENCE      (XYZ  ( 1),NBLOCK )  ,(ELEMT( 1),DELEM1 )  ,      
     1                 (ELEMT( 3),DELEM2 )  ,(INFIN    ,MASK6F )  ,      
     2                 (CORE (1),COREX(1 )  ,BUFF(1))        
      DATA             TYPES                 , CLR0      , BLANK     /
     1                 1,   2,  2,  4        , 9         , 4H        /
      DATA             BUFLGX    , TWO121    , DZERO     , DEBUG     /   
     1                 0         , 4095      , 0.0D+0    , .FALSE.   /
C VAX:   
      DATA             RECHDR    , MTXHDR    , LASTCW    , EQF       /   
     1                'F1111000'X,'F2222000'X,'F5555000'X,'F7777000'X/   
      DATA             SHEAD     , STRAIL    , DUMSTR    , LSTSTR    /   
     1                'F8888000'X,'F9999000'X,'FAAAA000'X,'FBBBB000'X/   
      DATA             MASK1     , MASK2     , MASK5     , MASK7     /   
     1                '00001111'X,'00002222'X,'00005555'X,'00007777'X/   
      DATA             MASK8     , MASKA     , MASKB     , MASKF     /   
     1                '00008888'X,'0000AAAA'X,'0000BBBB'X,'0000FFFF'X/   
      DATA             MASK2F    , MASK3F    , MASK6F    , CHEAD     /   
     1                'FF000000'X,'00000FFF'X,'00FFFFFF'X,'40000000'X/   
      DATA             CTRAIL    /   
     1                '80000000'X/   
C UNIX:   
C     DATA             RECHDR    , MTXHDR    , LASTCW    , EQF       /   
C    1                X'F1111000',X'F2222000',X'F5555000',X'F7777000'/   
C     DATA             SHEAD     , STRAIL    , DUMSTR    , LSTSTR    /   
C    1                X'F8888000',X'F9999000',X'FAAAA000',X'FBBBB000'/   
C     DATA             MASK1     , MASK2     , MASK5     , MASK7     /   
C    1                X'00001111',X'00002222',X'00005555',X'00007777'/   
C     DATA             MASK8     , MASKA     , MASKB     , MASKF     /   
C    1                X'00008888',X'0000AAAA',X'0000BBBB',X'0000FFFF'/   
C     DATA             MASK2F    , MASK3F    , MASK6F    , CHEAD     /   
C    1                X'FF000000',X'00000FFF',X'00FFFFFF',X'40000000'/   
C     DATA             CTRAIL    /   
C    1                X'80000000'/   
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
C
C     LOCFX1 = LOCFX(CORE(1))        
C
C     IF (INCR.LE.0 .AND. IROW.GT.0 .AND. LROW.GT.0) GO TO 1500 
      IF (INCR.LE.0 .AND. IROW.GT.0) GO TO 1500                 
      BFLAG  = MASK2F        
C        
C     SET FLAG FOR INTPK CALL.        
C        
C     FLAG = 2        
C     CALL INTPK (*390,NAME,BLOCK,TYPOUT,FLAG)        
      GO TO 400        
C        
   10 IF (IROW.GT.0 .AND. LROW.GT.0) GO TO 20        
      IROW = BROW        
      LROW = INFIN        
C        
   20 NWPCK= RSHIFT(BADDR2,24)        
      ICRP = IROW        
      NTPR = 0        
      IP   = BPOINT        
      IOFF = 0        
C     IMHERE = 20
C     IF (DEBUG) WRITE (NOUT,30) IMHERE,BLOCK        
C  30 FORMAT ('  UNPACK/BLOCK@',I3,' =',4I4,I7,2I4,I11,5I3,2I8,5I2)        
C        
   40 NWDS   = RSHIFT(BADDR1,24)        
      BRNCH1 = ANDF(BADDR1,INFIN)        
      IF (BRNCH1.GE.1 .AND. BRNCH1.LE.32) GO TO 45        
      WRITE  (NOUT,42) BRNCH1        
   42 FORMAT ('0*** SYSTEM FATAL ERROR. UNPACK/@42 BAD BRNCH1 =',I5,
     1        /5X,10(4H====))        
      CALL MESAGE (-61,0,0)        
   45 DO 50 K = 1,NWDS        
      ELEMT(K) = COREX(IP+K-1)        
   50 CONTINUE        
C        
      IP = IP + NWDS        
      ASSIGN 280 TO BRNCH2        
C        
   60 IF (BROW+NTPR-ICRP) 70,100,80        
C        
   70 ASSIGN 40 TO BRNCH2        
      GO TO 300        
C        
C     STORE ZERO FOR MISSING ELEMENT.        
C        
   80 DO 90 I = 1,NWPCK        
      ELEM(I+IOFF) = 0.0        
   90 CONTINUE        
      ASSIGN 60 TO BRNCH3        
      GO TO 360        
C        
C     ELEMENT CONVERSION.        
C        
C           OUT=  SP,  DP, SCX, DCX   !  IN=        
  100 GO TO (    270, 120, 110, 120,  !  +SP        
     1           140, 270, 140, 130,  !  +DP        
     2           110, 120, 270, 150,  ! +SCX        
     3           140, 130, 160, 270,  ! +DCX        
     4           180, 220, 210, 220,  !  -SP                           
     5           240, 200, 240, 230,  !  -DP                           
     6           210, 220, 170, 250,  ! -SCX                           
     7           240, 230, 260, 190   ! -DCX                           
     8      ),   BRNCH1        
C        
  110 ELEMT(2) = 0.0                  ! SCX= +SP,   SP=+SCX        
      GO TO 270        
  120 DELEM1 = DBLE(ELEMT(1))         ! DCX= +SP,   DP=+SCX,   DP = +SP        
  130 DELEM2 = DZERO                  ! DCX= +DP,   DP=+DCX 
      GO TO 270        
  140 ELEMT(1) = SNGL(DELEM1)         ! SP = +DP,   SP=+DCX,   SCX= +DP 
      ELEMT(2) = 0.0        
      GO TO 270        
  150 DELEM2 = DBLE(ELEMT(2))         ! DCX=+SCX        
      DELEM1 = DBLE(ELEMT(1))        
      GO TO 270        
  160 ELEMT(1) = SNGL(DELEM1)         ! SCX=+DCX        
      ELEMT(2) = SNGL(DELEM2)        
      GO TO 270
C                                     
  170 ELEMT(2) = -ELEMT(2)            ! SCX=-SCX
  180 ELEMT(1) = -ELEMT(1)            ! SP =-SP                         
      GO TO 270        
  190 DELEM2 = -DELEM2                ! DCX=-DCX
  200 DELEM1 = -DELEM1                ! DP =-DP                         
      GO TO 270        
  210 ELEMT(1) = -ELEMT(1)            ! SCX= -SP,   SP=-SCX 
      ELEMT(2) = 0.0                  
      GO TO 270        
  220 DELEM1 = -DBLE(ELEMT(1))        ! DCX= -SP,   DP=-SCX,   DP = -SP      
      DELEM2 =  DZERO          
      GO TO 270        
  230 DELEM1 = -DELEM1                ! DCX= -DP,   DP=-DCX             
      DELEM2 =  DZERO
      GO TO 270
  240 ELEMT(1) = -SNGL(DELEM1)        ! SP = -DP,   SP=-DCX,   SCX= -DP 
      ELEMT(2) = 0.0        
      GO TO 270        
  250 DELEM2 = -DBLE(ELEMT(2))        ! DCX=-SCX                        
      DELEM1 = -DBLE(ELEMT(1))        
      GO TO 270        
  260 ELEMT(1) = -SNGL(DELEM1)        ! SCX=-DCX                        
      ELEMT(2) = -SNGL(DELEM2)        
C        
  270 GO TO BRNCH2, (40,280)        
C        
C     STORE NON-ZERO ELEMENT.        
C        
  280 DO 290 I = 1,NWPCK        
      ELEM(I+IOFF) = ELEMT(I)        
  290 CONTINUE        
      ASSIGN  40 TO BRNCH3        
      ASSIGN 360 TO BRNCH2        
C        
C     IF END OF CURRENT STRING, START NEW STRING.        
C        
  300 NTPR = NTPR + 1        
      IF (NTPR .LT. BNBRAV) GO TO BRNCH2, (40,360)        
C     CALL ENDGET (BLOCK)        
C     CALL GETSTR (*330,BLOCK)        
      ASSIGN 310 TO ENDGET        
      GO TO 700        
  310 ASSIGN 320 TO GETSTR        
      ASSIGN 330 TO BADSTR        
      GO TO 900        
  320 IF (BPREC .EQ. 1) BPOINT = BPOINT*2 - 1        
      GO TO 350        
C        
  330 IF (LROW .NE. INFIN) GO TO 340        
      LROW = ICRP        
      RETURN        
C        
  340 BROW = INFIN        
  350 ASSIGN 40 TO BRNCH3        
      IP   = BPOINT        
      NTPR = 0        
      GO TO BRNCH2, (40,360)        
C        
C     IF NOT END OF COLUMN, ADVANCE TO NEXT POSITION IN COLUMN.        
C        
  360 IF (ICRP .GE. LROW) GO TO 370        
      ICRP = ICRP + 1        
      IOFF = IOFF + NWPCK*INCR        
      GO TO BRNCH3, (40,60)        
C        
C     END OF COLUMN.        
C        
  370 IF (BFLAG .NE. 1) CALL FWDREC (*380,BNAME)        
  380 RETURN        
C        
  390 RETURN 1        
C        
C     ----- END OF UNPACK ------        
C        
C     THE FOLLOWINGS ARE EXCERTS FROM VARIOUS SUPPORTING ROUTINES       
C     ===========================================================       
C        
C        
C     SUBROUTINE INTPK (*,NAME,BLOCK,TYPOUT,FLAG)        
C        
C     FLAG = 2        
C        
C        
  400 BNAME = NAME        
      IRET  = 1        
C     IMHERE= 400
C     IF (DEBUG) WRITE (NOUT,30) IMHERE,IBLOCK        
C     CALL GETSTR (*450,BLOCK)        
      ASSIGN 420 TO GETSTR        
      ASSIGN 450 TO BADSTR        
      GO TO 900        
  420 IRET  = 0        
C     IMHERE= 420
C     IF (DEBUG) WRITE (NOUT,30) IMHERE,BLOCK        
      IF (BPREC .EQ. 1) BPOINT = BPOINT*2 - 1        
      IADD = 0        
      IF (TYPOUT .LT. 0) IADD = 16        
      OUTYPE = IABS(TYPOUT)        
      BADDR2 = LSHIFT(TYPES(OUTYPE),24)        
      IMHERE = 430        
      INDEX  = 4*(BTYPE-1) + OUTYPE        
      IF (INDEX.LE.0 .OR. INDEX.GT.16) GO TO 460        
      INDEX  = INDEX + IADD        
      BADDR1 = ORF(LSHIFT(TYPES(BTYPE),24),INDEX)        
C     IF (DEBUG) WRITE (NOUT,30) IMHERE,BLOCK        
  450 IF (IRET) 10,10,390        
C        
C        
  460 WRITE  (NOUT,470) IMHERE        
  470 FORMAT ('0*** SYSTEM FATAL ERROR IN UNPACK @',I6)        
      IMHERE = IMHERE - 10000        
      IF (IMHERE .GT. 0) WRITE (NOUT,480) IMHERE        
  480 FORMAT (1H+,35X,'<==',I5)        
      CALL ERRTRC (0)        
C        
C        
C     SUBROUTINE DCODE (IBUFF,IBUFFE,IBUFFM)        
C        
C        
  500 IBUFFE = ANDF(IBUFF,MASK3F)        
      IBUFF  = RSHIFT(IBUFF,12)        
      IBUFFM = ANDF(IBUFF,MASKF)        
      IBUFF  = RSHIFT(IBUFF,16)        
      GO TO DCBACK, (1140,1190,1210)        
C        
C        
C     SUBROUTINE INIT (*,RDWRT,JBUFF)        
C        
C        
  600 NAM = NAMEX        
      IF (NAM .LT.    400) GO TO 620        
C     IF (NAM .NE. LSTNAM) GO TO 610        
      IF (NAM .NE. LSTNAM) GO TO 800        
      GO TO 630        
C 610 CALL GETURN (NAM)        
C     GO TO 630        
  620 NAM   = NAM - 100        
      FILEX = UNITS(NAM)        
  630 IMHERE= 630        
      IF (FILEX .EQ. 0 ) GO TO 460        
  640 JBUFF = ANDF(BUFADD(FILEX),MASK6F)        
      IMHERE= 640        
      IF (JBUFF .EQ. 0) GO TO 460        
  650 RDWRT = RSHIFT(BUFADD(FILEX),24)        
      GO TO 920        
C        
C        
C     SUBROUTINE ENDGET (BLOCK)        
C        
C        
  700 NAMEX = BLOCK(1)        
      ENTRY = 15        
      IBLOCK(3) = BLOCK(3)        
      IBLOCK(9) = BLOCK(9)        
      IBLOCK(11)= BLOCK(11)        
C        
      FILEX = IBLOCK(11)        
      JBUFF = ANDF(BUFADD(FILEX),MASK6F)        
C     CALL GINO (*720,*720,CORE(JBUFF),IDUM1,IDUM2,RDWRT)        
      ASSIGN 710 TO GINO        
      ASSIGN 720 TO RET1        
C     ASSIGN 720 TO RET2        
      GO TO 1100        
  710 GO TO ENDGET, (310)        
C        
  720 IMHERE = 720        
      GO TO 460        
C        
C        
C     SUBROUTINE GETURN (NAMDUM)        
C        
C        
  800 NN = 2*NFIST - 1        
      DO 810 I = 1,NN,2        
      IF (FIST(I) .EQ. NAMEX) GO TO 820        
  810 CONTINUE        
      FILEX = 0        
      GO TO 870        
  820 J = FIST(I+1)        
      IF (J) 830,840,850        
  830 J = -J        
  840 IIII= EXFIAT(J+1)        
      GO TO 860        
  850 IIII= FIAT(J+1)        
  860 FILEX  = ANDF(IIII,32767)        
      NTAPE  = ANDF(IIII,32768)        
      PRVOPN = RSHIFT(UNITAB(FILEX),28)        
      UNITAB(FILEX) = RSHIFT(LSHIFT(UNITAB(FILEX),4),4)        
      XYZ(1) = RSHIFT(UNITAB(FILEX),12)        
      XYZ(2) = ANDF(UNITAB(FILEX),TWO121)        
      IF (NAMEX .GE. 400) GO TO 870        
      UNITS(NAMEX-100) = FILEX        
  870 GO TO 630        
C        
C        
C     SUBROUTINE GETSTR (*,BLOCK)        
C        
C        
  900 NAMEX = BLOCK(1)        
      ENTRY = 14        
      IBLOCK(8) = BLOCK(8)        
C        
      IF (ANDF(IBLOCK(8),MASK2F) .NE. MASK2F) GO TO 910        
C     CALL INIT (*960,RDWRT,JBUFF)        
C     GO TO 920        
      GO TO 600        
  910 IBLOCK(10) = BLOCK(10)        
      IBLOCK(11) = BLOCK(11)        
      FILEX = IBLOCK(11)        
      JBUFF = ANDF(BUFADD(FILEX),MASK6F)        
C        
C 920 CALL GINO (*960,*990,CORE(JBUFF),IDUM1,IDUM2,RDWRT)        
  920 ASSIGN 930 TO GINO        
      ASSIGN 960 TO RET1        
C     ASSIGN 990 TO RET2        
      GO TO 1100        
  930 DO 940 I = 2,12        
      BLOCK(I) = IBLOCK(I)        
  940 CONTINUE        
C     IMHERE = 950
C     IF (DEBUG) WRITE (NOUT,30) IMHERE,BLOCK        
      GO TO GETSTR, (320,420)        
C        
  960 DO 970 I = 2,12        
      BLOCK(I) = IBLOCK(I)        
  970 CONTINUE        
C     IMHERE = 980
C     IF (DEBUG) WRITE (NOUT,300) IMHERE,BLOCK        
      GO TO BADSTR, (330,450)        
C        
C 990 IMHERE = 990        
C     GO TO 460        
C        
C        
C     SUBROUTINE GINOIO (*,OPCODE,B,MHERE)        
C        
C     ENTRY = 3 ONLY - READ ONE BLOCK        
C     B IS BUFF, OPCODE AND MHERE NOT USED HERE        
C        
C        
 1000 IF (R12345 .NE. -1234567890) GO TO 1020        
      IF (FILEX .LE. 1) STOP 'UNPACK/FILEX ERROR @1000'        
      IF (IOPEN(FILEX) .EQ. 0) GO TO 1020        
C        
C ... READ ONE BLOCK:        
C
C     IF (DEBUG) WRITE (NOUT,1010)        
C1010 FORMAT (/,' ===== UNPACK GINOIO/READ @1010 =====')        
      IMHERE = IMHERE + 10000        
      I = R(FILEX)        
      READ (FILEX,REC=I,ERR=1050) (BUFF(J),J=JBUFF4,JEND)        
      R(FILEX) = R(FILEX) + 1        
      IF (KLOSE .NE. 1) GO TO 1040        
      IOPEN(FILEX) = 0        
      CLOSE (UNIT=FILEX,STATUS='KEEP')        
      GO TO 1040        
 1020 CALL GINOIO (*1050,3,BUFF(JBUFF4),1020)        
C     IF (DEBUG) WRITE (NOUT,1030)        
C1030 FORMAT (/,' ===== UNPACK CALLING GINOIO TO READ @1020 =====')     
 1040 GO TO GOBACK, (1160,1260,1410,1430)        
 1050 GO TO ERBACK, (1410,1460,1060)        
 1060 GO TO 460        
C        
C        
C     SUBROUTINE GINO (*,*,BUFF,A,INCT,RDWRT)        
C        
C     INTEGER      A(1),INCT(2),BUFF(1)                                
C                  A,RDWRT,INCT ARE NOT USED HERE        
C        
C     ENTRY = 14,  CALL GETSTR (*,BLOCK)        
C     ENTRY = 15,  CALL ENDGET (BLOCK)        
C                  BLOCK(1) = NAME OF ALL STRING CALLS        
C        
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
C     IMHERE = 1110        
C     IF (DEBUG) WRITE (NOUT,1110) IMHERE,IBLOCK        
C1110 FORMAT (' UNPACK/IBLOCK@',I4,' =',4I4,I7,2I4,I11,5I3,2I8,5I2)       
C        
      IF (ENTRY-14) 1120,1120,1300        
C        
C     GETSTR        
C        
 1120 CBP = BUFF(JBUFF2)        
      CLR = BUFF(JBUFF3)        
      IF (ANDF(GFLAG,MASK2F) .NE. MASK2F) GO TO 1200        
      IMHERE = 1125        
      IF (CBP .GT. BUFF(8+JBUFF1)) GO TO 460        
 1130 IBUFF = BUFF(CLR+JBUFF1)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1140 TO DCBACK        
      GO TO 500        
 1140 IF (IBUFF.NE.15 .OR. IBUFFM.EQ.MASK7) GO TO 1150        
      IF (IBUFFM.EQ.MASK2 .OR. IBUFFM.EQ.MASK1) GO TO 1170        
      IMHERE = 1145        
      IF (IBUFFM .NE. MASK5) GO TO 460        
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
      GO TO 460        
 1180 IBUFF = BUFF(CBP+JBUFF1)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1190 TO DCBACK        
      GO TO 500        
 1190 IF (IBUFF .NE. 4) GO TO 1150        
      GCOL   = IBUFFM        
      GTYPE  = IBUFFE/16        
      GFORM  = ANDF(IBUFFE,15)        
      GFILEX = FILEX        
      GSFT   = 0        
      IF (GTYPE.EQ.2 .OR. GTYPE.EQ.3)  GSFT = 1        
      IF (GTYPE .EQ. 4) GSFT = 2        
      GPREC  = 0        
      IF (GTYPE.EQ.2 .OR. GTYPE.EQ.4) GPREC = 1        
 1200 CBP = CBP + 1        
      IBUFF = BUFF(CBP+JBUFF1)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1210 TO DCBACK        
      GO TO 500        
 1210 IF (IBUFF  .NE.    15) GO TO 1220        
      IF (IBUFFM .EQ. MASK8) GO TO 1230        
      IF (IBUFFM .EQ. MASKB) GO TO 1250        
      IF (IBUFFM .EQ. MASKA) GO TO 1270        
      IMHERE = 1215        
      GO TO 460        
 1220 IF (IBUFF .EQ. 8) GO TO 1280        
      IF (IBUFF .EQ. 3) GO TO 1250        
      IMHERE = 1225        
      GO TO 460        
 1230 GROW   = BUFF(CBP+1+JBUFF1)        
      GNBRAV = IBUFFE        
      GNBWRT = 0        
      GPOINT = LOCFX(BUFF(CBP+2+JBUFF1)) - LOCFX1        
      IF (GPREC .NE. 0) GPOINT  = GPOINT/2        
      GPOINT = GPOINT + 1        
      BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM = NAMEX        
C     IMHERE = 1240
C     IF (DEBUG) WRITE (NOUT,1110) IMHERE,IBLOCK        
      GO TO GINO, (710,930)        
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
C     ENDGET        
C        
 1300 CBP = BUFF(JBUFF2)        
      CLR = BUFF(JBUFF3)        
      CBP = LSHIFT(ANDF(BUFF(CBP+JBUFF1),TWO121),GSFT) + CBP + 1        
      IF (GFORM .EQ. 0) GO TO 1310        
      CBP = CBP + 2        
 1310 BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM = NAMEX        
C     IMHERE = 1320
C     IF (DEBUG) WRITE (NOUT,1110) IMHERE,IBLOCK        
      GO TO GINO, (710,930)        
C        
C     INTERNAL ROUTINE TO SKIP TO THE END OF A LOGICAL RECORD.        
C        
 1400 CBP  = ANDF(BUFF(CLR+JBUFF1),TWO121) + CLR + 1        
      IF (RSHIFT(BUFF(CBP+JBUFF1),28) .EQ. 3) GO TO 1420        
      CLR  = CBP + 1        
      IEND = RSHIFT(LSHIFT(BUFF(CLR+JBUFF1),4),16)        
      IF (IEND .NE. MASK5) GO TO 1440        
C     CALL GINOIO (*2605,RD,BUFF(JBUFF4),1405)        
      ASSIGN 1410 TO GOBACK        
      ASSIGN 1410 TO ERBACK        
      GO TO 1000        
 1410 CBP = CLR0        
      CLR = CLR0        
      GO TO 1450        
C1420 CALL GINOIO (*1460,RD,BUFF(JBUFF4),1420)        
 1420 ASSIGN 1430 TO GOBACK        
      ASSIGN 1460 TO ERBACK        
      GO TO 1000        
 1430 CBP = CLR0        
      CLR = CLR0        
      GO TO 1400        
 1440 CLR = CBP + 1        
      CBP = CLR        
 1450 GO TO RET3, (1290)        
 1460 CBP = CLR0        
      CLR = CLR0        
      GO TO RET1, (720,960)        
C
 1500 WRITE  (NOUT,1510) INCR,IROW,LROW                            
 1510 FORMAT ('0*** ERROR INPUT TO UNPACK.  INCR,IROW,LROW =',2I5)
      CALL ERRTRC (0)
      RETURN                                                       
      END        
