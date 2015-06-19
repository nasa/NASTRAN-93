      SUBROUTINE BLDPK (TYPIN,TYPOUT,NAME,IBLOCK,FLAG)        
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL          DEBUG        
      INTEGER          IBLOCK(1) ,JBLOCK(1) ,KBLOCK(1) ,BLOCK(20) ,     
     1                 INDX(16)  ,LIST(4)   ,CONV(16)  ,TYPES(4)  ,     
     2                 MXZERO(4) ,MCB(7)    ,BUFF(1)        
      REAL             CORE      ,ELEML(4)  ,ELEM(4)   ,ELEMX        
      DOUBLE PRECISION DELEM1    ,DELEM2    ,DZERO        
      COMMON /ZBLPKX/  ELEMX(4)  ,LROW        
      COMMON /ZZZZZZ/  CORE(1)        
      COMMON /MACHIN/  MAC(3)    ,LQRO        
      COMMON /SYSTEM/  SYSBUF    ,NOUT        
      COMMON /GINOX /  LENGTH    ,FILEX     ,EOR       ,OP        ,     
     1                 ENTRY     ,LSTNAM    ,N         ,NAMEX     ,     
     2                 NTAPE     ,XYZ(2)    ,UNITAB(75),BUFADD(75),     
     3                 NBUFF3    ,PRVOPN    ,UNITS(300),LBLOCK(20),     
     4                 KLOSE     ,LOCFX1    ,R12345    ,R(75)     ,     
     5                 IOPEN(75)        
      COMMON /BUFCOM/  BUFOFF    ,BUFBGN    ,BUFEND    ,BUFFLG        
      COMMON /XXFIAT/  EXFIAT(1)        
      COMMON /XFIAT /  FIAT(1)        
      COMMON /XFIST /  MFIST(1)  ,NFIST     ,FIST(1)        
      EQUIVALENCE      (BLOCK( 1),BNAME )   ,(BLOCK( 2),BTYPE )   ,     
     1                 (BLOCK( 3),BFORM )   ,(BLOCK( 4),BROW  )   ,     
     2                 (BLOCK( 5),BPOINT)   ,(BLOCK( 6),BNBRAV)   ,     
     3                 (BLOCK( 7),BNBWRT)   ,(BLOCK( 8),BFLAG )   ,     
     4                 (BLOCK(10),BPREC )   ,(BLOCK(11),BFILEX)   ,     
     5                 (BLOCK(12),BCOL  )   ,(BLOCK(13),BCOUNT)   ,     
     6                 (BLOCK(14),BADDR1)   ,(BLOCK(15),BADDR2)        
      EQUIVALENCE      (LBLOCK(1),GNAME )   ,(LBLOCK(2),GTYPE )   ,     
     1                 (LBLOCK(3),GFORM )   ,(LBLOCK(4),GROW  )   ,     
     2                 (LBLOCK(5),GPOINT)   ,(LBLOCK(6),GNBRAV)   ,     
     3                 (LBLOCK(7),GNBWRT)   ,(LBLOCK(8),GFLAG )   ,     
     4                 (LBLOCK(9),GSFT  )   ,(LBLOCK(10),GPREC)   ,     
     5                 (LBLOCK(11),GFILEX)  ,(LBLOCK(12),GCOL )        
      EQUIVALENCE      (ELEML( 1),DELEM1)   ,(ELEML( 3),DELEM2)        
      EQUIVALENCE      (BUFF(1) ,CORE(1))        
      DATA             TYPES      /        
     1                 1, 2, 2, 4 /        
      DATA             MXZERO     /  4*0    /      
C    1                 6, 3, 3, 2 /        
      DATA             CONV       /        
     1                 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 4, 4 / 
      DATA             INDX       /        
     1                 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 3, 3, 2, 2, 4, 4 / 
      DATA             LIST       /        
     1                 110,  120,  130,   140      /        
      DATA             CLR0      , BLANK     , DZERO     , BUFLGX     / 
     1                 9         , 4H        , 0.0D+0    , 0          / 
      DATA             TWO12     , TWO121    , DEBUG                  / 
     1                 4096      , 4095      , .FALSE.                / 
C VAX:        
      DATA             RECHDR    , MTXHDR    , LASTCW    , EQF        / 
     1                'F1111000'X,'F2222000'X,'F5555000'X,'F7777000'X / 
      DATA             SHEAD     , STRAIL    , DUMSTR    , LSTSTR     / 
     1                'F8888000'X,'F9999000'X,'FAAAA000'X,'FBBBB000'X / 
      DATA             CHEAD     , CTRAIL    , MASKA     , MASKB      / 
     1                '40000000'X,'80000000'X,'FF000000'X,'FF00FFFF'X / 
      DATA             MASK1     , MASK2     , MASK3     , MASK4      / 
     1                '10000000'X,'20000000'X,'30000000'X,'04000000'X / 
      DATA             MASKF     /        
     1                '00FFFFFF'X/        
C UNIX:        
C     DATA             RECHDR    , MTXHDR    , LASTCW    , EQF        / 
C    1                X'F1111000',X'F2222000',X'F5555000',X'F7777000' / 
C     DATA             SHEAD     , STRAIL    , DUMSTR    , LSTSTR     / 
C    1                X'F8888000',X'F9999000',X'FAAAA000',X'FBBBB000' / 
C     DATA             CHEAD     , CTRAIL    , MASKA     , MASKB      / 
C    1                X'40000000',X'80000000',X'FF000000',X'FF00FFFF' / 
C     DATA             MASK1     , MASK2     , MASK3     , MASK4      / 
C    1                X'10000000',X'20000000',X'30000000',X'04000000' / 
C     DATA             MASKF     /        
C    1                X'00FFFFFF'/        
C        
C     1. BNAME  = GINO REF NAME        
C     2. BTYPE  = TYPE OF STRINGS (1=RSP, 2=RDP, 3=CSP, 4=CDP)        
C     3. BFORM  = FORMAT OF STRING (0=NO TRAILERS,  NON-ZERO=TRAILERS)  
C     4. BROW   = ROW POSITION OF 1ST NON-ZERO TERM IN PRESENT STRING   
C     5. BPOINT = ADDR OF STRING        
C     6. BNBRAV = NO. OF TERMS IN CORE AVAILABLE FOR STRINGS        
C     7. BNBWRT = NO. OF TERMS OF THE PRESENT STRING ALREADY WRITTEN    
C     8. BFLAG  = BEGIN/END FLAG, -1 1ST CALL, 0 INTERMED, +1 LAST CALL 
C     9. BSFT   = SHIFT COUNT PER TERM, 0=RSP, 1=RDP OR SCP, 2=CDP      
C    10. BPREC  = PRECISION OF OUTPUT ELEM (0=RSP OR CSP, 1=RDP, OR CDP)
C    11  BFILEX = NAME OF CURRENT FILE BEING USED        
C    12. BCOL   = CURRENT COLUMN NO.        
C    13. BCOUNT = NO. OF NON-ZERO WORDS IN COLUMN        
C                 (6 OR LESS CONSEQ. ZEROS ARE TREATED AS NON-ZEROS)    
C    14. BADDR1 = NO. OF WORDS PER INPUT  ELEM. & BRANCH INDEX, COMBINED
C    15. BADDR2 = NO. OF WORDS PER OUTPUT ELEM. & BRANCH INDEX, COMBINED
C    16. BEOL   = EOL ADDR (NOT USED)        
C        
C        MXZERO = MAX. NO. OF CONSECUTIVE ZEROS ALLOWED TO BE PACKED    
C                 6 FOR S.P.REAL, 3 FOR D.P.REAL AND S.P.COMPLEX, AND   
C                 2 FOR D.P.COMPLEX        
C        NWPCK  = 1,2,2,OR 4 (DEPENDS ON BTYPE)        
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
      JFLAG  = FLAG        
      BFORM  = 0        
      IF (FLAG .LT. 0) BFORM = 1        
      BROW   = 0        
      BCOUNT = 0        
      BFLAG  = MASKB        
      BNAME  = NAME        
      BTYPE  = TYPOUT        
      INTYP  = TYPES(TYPIN)        
      INDEX  = 4*(TYPIN-1) + TYPOUT        
      IF (INDEX.LE.0 .OR. INDEX.GT.16) CALL VAXERR (10)        
      BADDR1 = ORF(LIST(INDX(INDEX)),LSHIFT(CONV(INDEX),24))        
      BADDR2 = ORF(INDEX,LSHIFT(TYPES(TYPOUT),24))        
C        
C     CALL SAVPOS (BNAME,POS)        
      GO TO 630        
   10 IF (ANDF(BUFADD(FILEX),MASK4) .EQ. MASK4) GO TO 20        
      BUFADD(FILEX) = ORF(BUFADD(FILEX),MASK4)        
      BCOL = 0        
C        
   20 BCOL = BCOL + 1        
C        
C     CALL PUTSTR (BLOCK)        
      ASSIGN 30 TO PUTSTR        
      GO TO 400        
   30 IF (FLAG .NE. 0) GO TO 40        
      NWPUP  = RSHIFT(BADDR1,24)        
      NWPCK  = RSHIFT(BADDR2,24)        
      KZERO  = MXZERO(NWPCK)        
      BRNCH1 =(ANDF(BADDR1,MASKF)-100)/10        
      BRNCH2 = ANDF(BADDR2,MASKF)        
      GO TO 60        
   40 DO 50 I = 1,16        
      IBLOCK(I) = BLOCK(I)        
   50 CONTINUE        
   60 RETURN        
C        
C*****        
      ENTRY BLDPKI (ELEM,IROW,NAME,JBLOCK)        
C     ====================================        
C     MULTI-COLUMN        
C*****        
C        
      DO 70 I = 1,16        
      BLOCK(I) = JBLOCK(I)        
   70 CONTINUE        
      NWPUP  = RSHIFT(BADDR1,24)        
      NWPCK  = RSHIFT(BADDR2,24)        
      KZERO  = MXZERO(NWPCK)        
      BRNCH1 =(ANDF(BADDR1,MASKF)-100)/10        
      BRNCH2 = ANDF(BADDR2,MASKF)        
      DO 80 I = 1,INTYP        
   80 ELEML(I) = ELEM(I)        
      LROW = IROW        
      GO TO 100        
C        
C*****        
      ENTRY ZBLPKI        
C     ============        
C     SINGLE COLUMN        
C*****        
C        
      DO 90 I = 1,INTYP        
   90 ELEML(I) = ELEMX(I)        
C        
C     CHECK TO ENSURE THAT ROW NUMBERS ARE INCREASING IN A MONOTONIC    
C     SEQUENCE        
C        
  100 IF (LROW .LE. BROW) CALL MESAGE (-104,0,0)        
C        
      GO TO (120,140,110,130), BRNCH1        
  110 IF (ELEML(2) .NE. 0.0) GO TO 150        
  120 IF (ELEML(1) .NE. 0.0) GO TO 150        
      GO TO 320        
  130 IF (DELEM2 .NE. DZERO) GO TO 150        
  140 IF (DELEM1 .NE. DZERO) GO TO 150        
      GO TO 320        
C        
  150 IF (BROW .NE. 0) GO TO 160        
C     BRNCH2 = ANDF(BADDR2,MASKF)        
      GO TO 210        
C        
  160 NXROW = LROW - BROW - BNBWRT        
      IF (NXROW) 170,220,180        
  170 CALL VAXERR (170)                           ! BAD ROW POSITION.   
C        
  180 IF (NXROW.GT.KZERO .OR. BNBWRT+KZERO.GE.BNBRAV) GO TO 200        
      BNBWRT = BNBWRT + NXROW        
      NXROW  = NXROW  * NWPCK        
      J = BPOINT        
      DO 190 I = 1,NXROW        
      CORE(J) = 0.0        
  190 J = J + 1        
      BPOINT = J        
      BCOUNT = BCOUNT + NXROW            ! SHOULD NOT COUNT THESE ZEROS?
      GO TO 220        
C        
C 200 CALL ENDPUT (BLOCK)        
C     CALL PUTSTR (BLOCK)        
  200 ASSIGN 400 TO ENDPUT        
      ASSIGN 210 TO PUTSTR        
      GO TO 460        
  210 BROW = LROW        
C        
C           OUT=  SP,  DP, SCX, DCX     ! IN=        
  220 GO TO (    290, 230, 260, 230,    !  SP        
     1           250, 290, 250, 240,    !  DP        
     2           260, 230, 290, 270,    ! SCX        
     3           250, 240, 280, 290     ! DCX        
     4      ),   BRNCH2        
C        
  230 DELEM2 = DZERO        
      DELEM1 = DBLE(ELEML(1))        
      GO TO 290        
  240 DELEM2 = DZERO        
      GO TO 290        
  250 ELEML(1) = SNGL(DELEM1)        
  260 ELEML(2) = 0.0        
      GO TO 290        
  270 DELEM2 = DBLE(ELEML(2))        
      DELEM1 = DBLE(ELEML(1))        
      GO TO 290        
  280 ELEML(1) = SNGL(DELEM1)        
      ELEML(2) = SNGL(DELEM2)        
C        
  290 J = BPOINT        
      DO 300 I = 1,NWPCK        
      CORE(J) = ELEML(I)        
  300 J = J + 1        
      BPOINT = J        
      BCOUNT = BCOUNT + NWPCK        
      BNBWRT = BNBWRT + 1        
      IF (BNBWRT .LT. BNBRAV) GO TO 320        
C        
C     CALL ENDPUT (BLOCK)        
C     CALL PUTSTR (BLOCK)        
      ASSIGN 400 TO ENDPUT        
      ASSIGN 310 TO PUTSTR        
      GO TO 460        
  310 BROW = 0        
  320 IF (JFLAG .EQ. 0) GO TO 340        
C        
      DO 330 I = 1,16        
      JBLOCK(I) = BLOCK(I)        
  330 CONTINUE        
C        
  340 RETURN        
C        
C*****        
      ENTRY BLDPKN (NAME,KBLOCK,MCB)        
C     ==============================        
C     TERMINATE PROCESSING OF COLUMN.        
C*****        
C        
      IF (KBLOCK(1) .EQ. 0) GO TO 360        
      DO 350 I = 1,16        
      BLOCK(I) = KBLOCK(I)        
  350 CONTINUE        
  360 IMHERE = 360        
      IF (BNAME .NE. NAME) CALL VAXERR (360)        
      IF (MCB(2) .EQ. 0) MCB(7) = CHEAD        
      I = BCOUNT        
C     IF (BTYPE .LE. 2) I = I/(BPREC+1)        
      MCB(7) = MCB(7) + I        
      MCB(2) = MCB(2) + 1        
      MCB(6) = MAX0(MCB(6),I)        
      BFLAG  = 1        
C        
C     CALL ENDPUT (BLOCK)        
      ASSIGN 370 TO ENDPUT        
      GO TO 460        
  370 IF (KBLOCK(1) .EQ. 0) GO TO 390        
C        
      DO 380 I = 1,16        
      KBLOCK(I) = BLOCK(I)        
  380 CONTINUE        
C        
  390 RETURN        
C        
C     ====================== END OF BLDPK =======================       
C        
C        
C     THE FOLLOWINGS ARE EXCERTS FROM VARIOUS SUPPORTING ROUTINES       
C     ===========================================================       
C        
C*****        
C     SUBROUTINE PUTSTR (BLOCK)        
C*****        
C        
 400  NAMEX = BLOCK(1)        
C     IF (DEBUG) WRITE (NOUT,405) BLOCK        
C405  FORMAT (2X,'BLDPK/PUTSTR@405...',4I4,I7,2I4,I14,5I4,I7,6I4)       
      IMHERE= 405        
      ENTRY = 12        
      LBLOCK( 3) = BLOCK( 3)        
      LBLOCK( 8) = BLOCK( 8)        
C        
      IF (ANDF(LBLOCK(8),MASKA) .NE. MASKA) GO TO 410        
      LBLOCK( 2) = BLOCK( 2)        
      LBLOCK(12) = BLOCK(12)        
C        
C     CALL INIT (*650,RDWRT,JBUFF)        
      ASSIGN 420 TO INIT        
      GO TO 500        
 410  LBLOCK( 9) = BLOCK( 9)        
      LBLOCK(10) = BLOCK(10)        
      LBLOCK(11) = BLOCK(11)        
      FILEX = LBLOCK(11)        
      JBUFF = ANDF(BUFADD(FILEX),MASKF)        
 420  IMHERE= 420        
C     IF (DEBUG) WRITE (NOUT,425) BLOCK        
C425  FORMAT (3X,'BLDPK/PUTSTR@425...',4I4,I7,2I4,I14,5I4,I7,6I4)       
C        
C     CALL GINO (*650,*650,CORE(JBUFF),DUM1,DUM2,RDWRT)        
      GO TO 1100        
 430  IF (DEBUG) WRITE (NOUT,435) RDWRT,JBUFF        
 435  FORMAT (/4X,'BLDPK/ @435 RDWRT,JBUFF=',I3,I7)        
      DO 440 I = 5,11        
      BLOCK(I) = LBLOCK(I)        
 440  CONTINUE        
      IF (BPREC .EQ. 1) BPOINT = BPOINT*2 - 1        
      GO TO PUTSTR, (30,210,310)        
C        
C*****        
C     SUBROUTINE ENDPUT (BLOCK)        
C*****        
C        
 460  NAMEX = BLOCK(1)        
C     IF (DEBUG) WRITE (NOUT,465) BLOCK        
C465  FORMAT (5X,'BLDPK/ENDPUT@465...',4I4,I7,2I4,I14,5I4,I7,6I4)       
      IMHERE= 465        
      ENTRY = 13        
      DO 470 I = 2,12        
      LBLOCK(I) = BLOCK(I)        
 470  CONTINUE        
C        
      FILEX = LBLOCK(11)        
      JBUFF = ANDF(BUFADD(FILEX),MASKF)        
      IMHERE= 480        
C        
C     CALL GINO (*650,*650,CORE(JBUFF),DUM1,DUM2,RDWRT)        
      GO TO 1100        
C        
 480  GO TO ENDPUT, (370,400)        
C        
C*****        
C     SUBROUTINE INIT (*,RDWRT,JBUFF)        
C*****        
C        
 500  NAM = NAMEX        
      IF (NAM .LT.    400) GO TO 520        
      IF (NAM .NE. LSTNAM) GO TO 550        
      GO TO 530        
C        
 520  NAM   = NAM - 100        
      FILEX = UNITS(NAM)        
 530  IMHERE= 530        
      IF (FILEX .EQ. 0) GO TO 650        
      JBUFF = ANDF(BUFADD(FILEX),MASKF)        
      IMHERE= 531        
      IF (JBUFF .EQ. 0) GO TO 650        
      RDWRT = RSHIFT(BUFADD(FILEX),24)        
      GO TO INIT, (420,1100)        
C        
C*****        
C     SUBROUTINE GETURN (NAMDUM)        
C*****        
C        
 550  NN = 2*NFIST - 1        
      DO 560 I = 1,NN,2        
      IF (FIST(I) .EQ. NAMEX) GO TO 570        
 560  CONTINUE        
      FILEX = 0        
      GO TO 620        
 570  J = FIST(I+1)        
      IF(J) 580,590,600        
 580  J = -J        
 590  IIII= EXFIAT(J+1)                   
      GO TO 610        
 600  IIII= FIAT(J+1)                     
 610  FILEX  = ANDF(IIII,32767)        
      NTAPE  = ANDF(IIII,32768)        
      PRVOPN = RSHIFT(UNITAB(FILEX),28)        
      UNITAB(FILEX) = RSHIFT(LSHIFT(UNITAB(FILEX),4),4)        
      XYZ(1) = RSHIFT(UNITAB(FILEX),12)        
      XYZ(2) = ANDF(UNITAB(FILEX),TWO121)        
      IF (NAMEX .LT. 400) UNITS(NAMEX-100) = FILEX        
 620  GO TO 530        
C        
C*****        
C     SUBROUTINE SAVPOS (NAME,POS)        
C*****        
C        
 630  NAMEX = BNAME        
      IMHERE= 630        
      ENTRY = 10        
C        
C     CALL INIT (*650,RDWRT,JBUFF)        
C     CALL GINO (*650,*650,CORE(JBUFF),IDUM2,INCT,RDWRT)        
      ASSIGN 1100 TO INIT        
      GO TO 500        
 640  POS = INCT        
      GO TO 10        
C        
C        
C     ERROR MESSAGE.        
C        
 650  WRITE  (NOUT,660) IMHERE        
 660  FORMAT (/,' *** FATAL ERROR IN BLDPK @',I4)        
      WRITE (NOUT,670) FILEX,JBUFF,JBUFF1,NBUFF2,CBP,GSFT,GNBWRT,GNBRAV,
     1                 NBUFF3,GROW        
 670  FORMAT (//,'  FILEX,JBUFF,JBUFF1,NBUFF2,CBP,GSFT,GNBWRT,GNBRAV',  
     1        ',NBUFF3,GROW=',/1X,10I7)        
      CALL MESAGE (-61,0,0)        
C        
C*****        
C     INTERNAL ROUTINE TO WRITE A BLOCK        
C     USED ONLY BY GINO BELOW        
C*****        
C        
 700  J8 = 8 + JBUFF1        
      BUFF(J8) = CLR        
C        
C     CALL GINOIO (*650,WRT,BUFF(JBUFF4),500)        
      GO TO 800        
 710  BUFF(JBUFF4) = BUFF(JBUFF4) + 1        
      CBP = CLR0        
      CLR = CLR0        
      GO TO RET, (1205,1210,1325)        
C        
C*****        
C     INTERNAL ROUTINE TO WRITE CONTROL WORD IN BUFFER AT THE END       
C     OF A LOGICAL RECORD. USED ONLY BY GINO BELOW        
C*****        
C        
 750  BUFF(CLR+JBUFF1) = ORF(CBP-CLR,IHEADR)        
      MASK = MASK1        
      IF (RSHIFT(UNITAB(FILEX),12) .NE. BUFF(JBUFF4)) MASK = MASK2      
      BUFF(CBP+1+JBUFF1) = ORF(UNITAB(FILEX),MASK)        
      CLR = CBP + 2        
      CBP = CLR        
      GO TO 1330        
C        
C*****        
C     SUBROUTINE GINOIO (*,OPCODE,B,MHERE)        
C*****        
C     OPCODE=2 ONLY, TO WRITE ONE BLOCK        
C     B IS BUFF, AND MHERE IS NOT USED HERE        
C        
C     IF FILE HAS NOT BEEN OPENED BEFORE, WE MUST GO TO REGULAR GINOIO  
C     BECAUSE OF THE ASSOCIATE-VARIABLE INVOLVED, OTHERWISE MISTERIOUS  
C     ERROR MIGHT OCCUR        
C        
 800  IF (R12345 .NE. -1234567890) GO TO 810        
      IMHERE = 800        
      IF (FILEX .LE. 1) GO TO 650        
      IF (IOPEN(FILEX) .EQ. 0) GO TO 810        
      I = R(FILEX)        
C     IF (DEBUG) WRITE (NOUT,805)        
C805  FORMAT (/6X,'=====BLDPK GINOIO WRITE @805 ======')        
      WRITE (FILEX,REC=I) (BUFF(J),J=JBUFF4,JEND)        
      R(FILEX) = R(FILEX) + 1        
      IF (KLOSE .NE. 1) GO TO 710        
      IOPEN(FILEX) = 0        
      CLOSE (UNIT=FILEX,STATUS='KEEP')        
      GO TO 710        
 810  IMHERE = 810        
C     IF (DEBUG) WRITE (NOUT,815)        
C815  FORMAT (/7X,'===== BLDPK CALLING GINOIO TO WRITE @815 =====')     
C        
      CALL GINOIO (*650,2,BUFF(JBUFF4),810)        
      GO TO 710        
C        
C*****        
C     SUBROUTINE GINO (*,*,BUFF,A,INCT,RDWRT)        
C        
C     INTEGER   A(1)   ,INCT(2),BUFF(1)        
C               A,RDWRT,INCT(2), AND RETURN 2 ARE NOT USED HERE        
C        
C     ENTRY=10, CALL SAVPOS (NAME,POS)        
C     ENTRY=12, CALL PUTSTR (BLOCK)        
C     ENTRY=13, CALL ENDPUT (BLOCK)        
C               LBLOCK(1) =  NAME FOR ALL STRING CALLS        
C*****        
C        
 1100 CONTINUE        
      BUFOFF = JBUFF        
      BUFBGN = 1        
      BUFEND = NBUFF3        
      BUFFLG = BUFLGX        
      NBUFF2 = NBUFF3        
      LOCFX1 = LOCFX(CORE(1))        
      JBUFF1 = JBUFF  - 1        
      JBUFF2 = JBUFF1 + 2        
      JBUFF3 = JBUFF1 + 3        
      JBUFF4 = JBUFF1 + 4        
      JEND   = JBUFF3 + NBUFF3        
C     IF (DEBUG) WRITE (NOUT,1110) ENTRY        
C1110 FORMAT (/8X,'BLDPK/ @1110  ENTRY=',I4)        
      IF (ENTRY-12) 1150,1200,1300        
C        
C ... GINO-SAVPOS (ENTRY=10)        
C        
 1150 CBP     = BUFF(JBUFF2)        
      CLR     = BUFF(JBUFF3)        
      INCT    = TWO12*BUFF(JBUFF4) + CLR        
      IF (ANDF(RDWRT,1) .EQ. 1) INCT = UNITAB(FILEX)        
      BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM  = NAMEX        
      GO TO 640        
C        
C ... GINO-PUTSTR (ENTRY=12)        
C        
 1200 CONTINUE        
      CBP = BUFF(JBUFF2)        
      CLR = BUFF(JBUFF3)        
      IF (ANDF(GFLAG,MASKA) .NE. MASKA) GO TO 1210        
      GFLAG = ANDF(GFLAG,MASKF)        
      IF (CLR+4 .LT. NBUFF2) GO TO 1205        
      BUFF(CLR+JBUFF1) = LASTCW        
      ASSIGN 1205 TO RET        
      GO TO 700        
C        
 1205 UNITAB(FILEX) = TWO12*BUFF(JBUFF4) + CLR        
      GFILEX = FILEX        
      GPREC  = 0        
      IF (GTYPE.EQ.2 .OR. GTYPE.EQ.4) GPREC = 1        
      GSFT = 0        
      IF (GTYPE.EQ.2 .OR. GTYPE.EQ.3) GSFT  = 1        
      IF (GTYPE .EQ. 4) GSFT = 2        
C     IF (DEBUG) WRITE (NOUT,1206) GPREC,GTYPE,GSFT,GFILEX        
C1206 FORMAT (9X,'BLDPK/@1206 GPREC,GTYPE,GSFT,GFILEX=',4I6)        
      CBP  = CBP + 1        
      JREG = ORF(CHEAD,16*GTYPE+GFORM)        
      BUFF(CBP+JBUFF1) = ORF(TWO12*ANDF(GCOL,TWO121),JREG)        
C        
 1210 IAVIAL = CBP + 4        
      IF (GFORM .NE. 0) IAVIAL = IAVIAL + 2        
      IAVIAL = NBUFF2 - IAVIAL        
      IF (GSFT  .NE. 0) IAVIAL = IAVIAL/(2*GSFT)        
C     IF (DEBUG) WRITE (NOUT,1211) IAVIAL,CBP,GFORM,GSFT,NBUFF2        
C1211 FORMAT (15X,'BLDPK/ @1211 IAVIAL,CBP,GFORM,GSFT,NBUFF2=',5I7)     
      IF (IAVIAL .GT. 0) GO TO 1230        
 1215 CBP = CBP + 1        
      BUFF(CBP+JBUFF1) = LSTSTR        
      IF (CBP-NBUFF2) 1215,1225,1220        
 1220 IMHERE = 1220        
      GO TO 650        
C        
 1225 BUFF(CLR  +JBUFF1) = ORF(CBP-CLR,MTXHDR)        
      BUFF(CBP+1+JBUFF1) = ORF(UNITAB(FILEX),MASK3)        
      BUFF(CBP+2+JBUFF1) = LASTCW        
      CLR = CBP + 2        
      ASSIGN 1210 TO RET        
      GO TO 700        
C        
 1230 I = GNBWRT        
      J = GNBRAV        
      GNBWRT = 0        
      GNBRAV = IAVIAL        
C     IF (DEBUG) WRITE (NOUT,1231) J,GNBRAV,I,GNBWRT,NBUFF2        
C1231 FORMAT (10X,'BLDPK GINO-PUTSTR/ @1231 GNBRAV(O/N),GBNWRT(O/N),',  
C    1        'NBUFF2=',2(I7,1H/,I6),I7)        
      IF (GPREC .NE. 0) GO TO 1235        
      GPOINT = LOCFX(BUFF(CBP+3+JBUFF1)) - LOCFX1 + 1        
C     IF (DEBUG) WRITE (NOUT,1232) GPOINT        
C1232 FORMAT (12X,'BLDPK/@1232 GPOINT=',I8)        
      GO TO 1245        
C        
 1235 GPOINT = (LOCFX(BUFF(CBP+3+JBUFF1))-LOCFX1)*(LQRO/1000)        
      IF (MOD(GPOINT,8) .EQ. 0) GO TO 1240        
      CBP = CBP + 1        
      BUFF(CBP+JBUFF1) = DUMSTR        
      GO TO 1210        
C        
 1240 GPOINT = GPOINT/8 + 1        
C     IF (DEBUG) WRITE (NOUT,1241) GPOINT        
C1241 FORMAT (13X,'BLDPK/@1241 GPOINT=',I8,'<==')        
 1245 BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM  = NAMEX        
      GO TO 430        
C        
C ... GINO-ENDPUT (ENTRY=13)        
C        
 1300 CONTINUE        
      CBP = BUFF(JBUFF2)        
      CLR = BUFF(JBUFF3)        
C     IF (DEBUG) WRITE (NOUT,1301) GNBRAV,GNBWRT,CBP        
C1301 FORMAT (15X,'BLDPK GINO-ENDPUT/ @1301 GNBRAV,GBNWRT,CBP=',3I7)    
      IF (GNBWRT .EQ. 0) GO TO 1305        
      IMHERE = 1300        
      IF (GNBWRT .GT. GNBRAV) GO TO 650       !WRITTEN EXCEEDS AVAILABLE
      BUFF(CBP+1+JBUFF1) = ORF(GNBWRT,SHEAD)        
      BUFF(CBP+2+JBUFF1) = GROW        
      CBP = CBP + 2 + LSHIFT(GNBWRT,GSFT)        
      IMHERE = 1302        
      IF (CBP .GT. NBUFF2) GO TO 650                    !BUFFER OVERFLOW
      IF (GFORM .EQ.  0) GO TO 1305        
      BUFF(CBP+1+JBUFF1) = ORF(GNBWRT,STRAIL)        
      BUFF(CBP+2+JBUFF1) = GROW+GNBWRT - 1        
      CBP = CBP + 2        
      IMHERE = 1304        
      IF (CBP .GT. NBUFF2) GO TO 650                    !BUFFER OVERFLOW
 1305 IF (GNBWRT .EQ. GNBRAV) GO TO 1310        
      GO TO 1325        
 1310 CBP = CBP + 1        
      BUFF(CBP+JBUFF1) = LSTSTR        
      IF (CBP-NBUFF2) 1310,1320,1315        
 1315 IMHERE = 1315        
      GO TO 650                                         !BUFFER OVERFLOW
 1320 BUFF(CLR+  JBUFF1) = ORF((CBP-CLR),MTXHDR)        
      BUFF(CBP+1+JBUFF1) = ORF(UNITAB(FILEX),MASK3)        
      BUFF(CBP+2+JBUFF1) = LASTCW        
      CLR = CBP + 2        
      ASSIGN 1325 TO RET        
      GO TO 700        
C        
 1325 IF (GFLAG .NE. 1) GO TO 1330        
      CBP = CBP + 1        
      BUFF(CBP+JBUFF1) =        
     1     ORF(CTRAIL,TWO12*ANDF(GCOL,TWO121)+16*GTYPE+GFORM)        
      IHEADR = MTXHDR        
      GO TO 750        
 1330 BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM  =  NAMEX        
      GO TO 480        
C        
      END        
