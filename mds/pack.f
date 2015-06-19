      SUBROUTINE PACK (ELEM,NAME,MCB)        
C        
C     MATRIX PACKING ROUTINE        
C     A UNIFIED VERSION FOR SPEED IMPROVEMENT.        
C     ASSEMBLED BY G.CHAN/SPERRY  8/86        
C        
C     PACK DOES NOT CROSS THE REAL AND COMPLEX BOUNDARAY.        
C     i.e. DOES NOT HANDLE REAL MATRIX INPUT AND COMPLEX MATRIX OUTPUT  
C     AND VISE VERSA        
C     PACK DOES NOT CHANGE SIGN ON PACKING OUT OUTPUT MATRIX ELEMENTS.  
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL*1        DEBUG        
      INTEGER          TYPES(4)  ,INDX1(16),INDX2(16) ,LISTX(3) ,       
     1                 BLOCK(20) ,CONV(16) ,MCB(7)    ,BLK(12)  ,       
     2                 BUFF(1)        
      REAL             ELEM(1)   ,ELEML(4) ,ELEML1    ,ELEML2   ,       
     1                 ELEML3    ,RCORE(1)        
      DOUBLE PRECISION DELEM1    ,DELEM2   ,DZERO        
      COMMON /SYSTEM/  SYSBUF    ,NOUT     ,NOGO        
      COMMON /MACHIN/  MAC(3)    ,LQRO        
      COMMON /PACKX /  TYPIN     ,TYPOUT   ,IROW      ,LROW     ,       
     1                 INCR        
      COMMON /ZZZZZZ/  CORE(1)        
      COMMON /GINOX /  LENGTH    ,FILEX    ,EOR       ,OP       ,       
     1                 ENTRY     ,LSTNAM   ,N         ,NAMEX    ,       
     2                 NTAPE     ,XYZ(2)   ,UNITAB(75),BUFADD(75)       
     3,                NBUFF3    ,PRVOPN   ,UNITS(300),IBLOCK(20)       
     4,                KLOSE     ,LOCFX1   ,R12345    ,R(75)    ,       
     5                 IOPEN(75)        
      COMMON /XXFIAT/  EXFIAT(1)        
      COMMON /XFIAT /  FIAT(1)        
      COMMON /XFIST /  MFIST     ,NFIST    ,FIST(1)        
      COMMON /BUFCOM/  BUFOFF    ,BUFBGN   ,BUFEND    ,BUFFLG        
      EQUIVALENCE      (BLOCK( 1),BNAME  ) ,(BLOCK( 2),BTYPE  ) ,       
     1                 (BLOCK( 3),BFORM  ) ,(BLOCK( 4),BROW   ) ,       
     2                 (BLOCK( 5),BPOINT ) ,(BLOCK( 6),BNBRAV ) ,       
     3                 (BLOCK( 7),BNBWRT ) ,(BLOCK( 8),BFLAG  ) ,       
     4                 (BLOCK( 9),BSFT   ) ,(BLOCK(10),BPREC  ) ,       
     5                 (BLOCK(11),BFILEX ) ,(BLOCK(12),BCOL   ) ,       
     6                 (BLOCK(13),BCOUNT ) ,(BLOCK(14),BADDR1 ) ,       
     7                 (BLOCK(15),BADDR2 ) ,(BLOCK( 1),BLK(1) )        
      EQUIVALENCE      (IBLOCK( 1),GNAME ) ,(IBLOCK( 2),GTYPE ) ,       
     1                 (IBLOCK( 3),GFORM ) ,(IBLOCK( 4),GROW  ) ,       
     2                 (IBLOCK( 5),GPOINT) ,(IBLOCK( 6),GNBRAV) ,       
     3                 (IBLOCK( 7),GNBWRT) ,(IBLOCK( 8),GFLAG ) ,       
     4                 (IBLOCK( 9),GSFT  ) ,(IBLOCK(10),GPREC ) ,       
     5                 (IBLOCK(11),GFILEX) ,(IBLOCK(12),GCOL  )        
      EQUIVALENCE      (XYZ  ( 1),NBLOCK ) ,(ELEML( 2),ELEML2 ) ,       
     1                 (ELEML( 1),ELEML1   ,DELEM1  )           ,       
     2                 (ELEML( 3),ELEML3   ,DELEM2  )        
      EQUIVALENCE      (BUFF ( 1),CORE(1)  ,RCORE(1))        
      DATA    TYPES /  1, 2, 2, 4  /,       LISTX   /   1, 2, 3      /  
      DATA    CONV  /  1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 4, 4/  
      DATA    INDX1 /  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 3, 3/  
      DATA    INDX2 /  1, 2, 1, 2, 3, 1, 3, 1, 1, 2, 1, 2, 3, 1, 3, 1/  
      DATA             BUFLGX    , TWO12     , TWO121    , DZERO     /  
     1                 0         , 4096      , 4095      , 0.0D+0    /  
      DATA             CLR0      , BLANK     , DEBUG     , ERRCNT    /  
     1                 9         , 4H        , .FALSE.   , 0         /  
C VAX:        
      DATA             RECHDR    , MTXHDR    , LASTCW    , EQF       /  
     1                'F1111000'X,'F2222000'X,'F5555000'X,'F7777000'X/  
      DATA             SHEAD     , STRAIL    , DUMSTR    , LSTSTR    /  
     1                'F8888000'X,'F9999000'X,'FAAAA000'X,'FBBBB000'X/  
      DATA             CHEAD     , CTRAIL    , MASK2F    , MASKF     /  
     1                '40000000'X,'80000000'X,'FF000000'X,'FF00FFFF'X/  
      DATA             MASK6F    , MASK1     , MASK2     , MASK3     /  
     1                '00FFFFFF'X,'10000000'X,'20000000'X,'30000000'X/  
C UNIX:        
C     DATA             RECHDR    , MTXHDR    , LASTCW    , EQF       /  
C    1                X'F1111000',X'F2222000',X'F5555000',X'F7777000'/  
C     DATA             SHEAD     , STRAIL    , DUMSTR    , LSTSTR    /  
C    1                X'F8888000',X'F9999000',X'FAAAA000',X'FBBBB000'/  
C     DATA             CHEAD     , CTRAIL    , MASK2F    , MASKF     /  
C    1                X'40000000',X'80000000',X'FF000000',X'FF00FFFF'/  
C     DATA             MASK6F    , MASK1     , MASK2     , MASK3     /  
C    1                X'00FFFFFF',X'10000000',X'20000000',X'30000000'/  
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
C     IF (TYPOUT .EQ. TYPIN) GO TO 5        
C     IF (TYPOUT .LT. 0) CALL ERRTRC (0)        
C     IF ((TYPOUT.EQ.1 .OR. TYPOUT.EQ.2) .AND.      ! ERROR CPLX==>REAL 
C    1    (TYPIN .EQ.3 .OR. TYPIN .EQ.4)) CALL ERRTRC (0)        
C     IF ((TYPOUT.EQ.3 .OR. TYPOUT.EQ.4) .AND.      ! ERROR REAL==>CPLX 
C    1    (TYPIN .EQ.1 .OR. TYPIN .EQ.2)) CALL ERRTRC (0)        
C 5   CONTINUE        
C        
      IF (INCR .EQ. 0) CALL ERRTRC (0)        
      BNAME  = NAME        
      BCOL   = MCB(2) + 1        
      BROW   = 0        
      BCOUNT = 0        
      BFORM  = 0        
      BFLAG  = MASKF        
      BTYPE  = TYPOUT        
      NWPCK  = TYPES(TYPOUT)        
      INTYPE = TYPES(TYPIN )*INCR        
      IMHERE = 10        
      J      = 4*(TYPIN-1) + TYPOUT        
      IF (J.LT.0 .OR. J.GT.16) GO TO 450        
      BADDR1 = LISTX(INDX1(J))        
      BADDR2 = LISTX(INDX2(J))        
      NWDS   = CONV(J)        
      NBUFF2 = NBUFF3        
      LOCFX1 = LOCFX(CORE(1))        
C     CALL PUTSTR (BLOCK)        
      ASSIGN 10 TO PUTSTR        
      GO TO 200        
 10   IOFF   = 0        
      ITERM  = 0        
      IROWX  = IROW        
      ELEML2 = 0.0        
      ELEML3 = 0.0        
      ELEML(4) = .0        
C        
 20   DO 30 I = 1,NWDS        
      ELEML(I) = ELEM(I+IOFF)        
 30   CONTINUE        
C        
      GO TO (45, 55,  40,  50), TYPIN        
C            SP  DP  SCX  DCX        
C        
 40   IF (ELEML2 .NE. 0.0) GO TO 60        
 45   IF (ELEML1 .NE. 0.0) GO TO 60        
      GO TO 170        
 50   IF (DELEM2 .NE. DZERO) GO TO 60        
 55   IF (DELEM1 .NE. DZERO) GO TO 60        
      GO TO 170        
C        
C     HERE WE HAVE -        
C        
C     IROW   - FIRST ROW TO BE PACK, USUALLY = 1        
C     LROW   - LAST  ROW TO BE PACK, USUALLY = TOTAL NO. OF ROWS        
C     IROWX  - RUNNING INDEX ALONG THE ROWS        
C     BROW   - ROW POSITION OF FIRST NON-ZERO ROW        
C     ITERM  - NO. OF ZON-ZERO ROWS PRESENTLY ENCOUNTERED        
C     BCOUNT - NO. OF WORDS PRESENTLY ACCUMULATED FOR THE NON-ZERO      
C              ROWS (EACH NON-ZERO ITERM HAS "NWPCK" WORDS)        
C     BPOINT - POINTER, WITH RESPECT TO THE GINO BUFFER IN OPEN CORE,   
C              OF THER LAST NON-ZERO TERM        
C     BNBWRT - IS NO. OF WORDS TO BE WRITTEN OUT BY GINO, =ITERM        
C     IOFF   - RUNNING ROW POINTER TO THE INPUT STRING        
C     JBUFF  - POINTER TO THE BUFFER SPACE IN CORE ALLOCATED TO        
C              GINO FILE FILEX        
C     FILEX  - IS GINO FILE CURRENTLY IN USE        
C     NAMEX  - IS GINO FILE NAME CURRENTLY IN USE        
C     LSTNAM - GINO FILE NAME PREVIOUSLY USED        
C     LQRO   - LQRO/1000 IS THE BYTE OR WORD COUNT FOR LOCFX FUNCTION 
C        
C     INPUT IS NON-ZERO        
C     SAVE BEGINNING ROW POINTER IN 'BROW' IF THIS IS FIRST NON-ZERO    
C        
 60   IF (ITERM .NE. 0) GO TO 70        
      BROW = IROWX        
      GO TO 90        
C        
 70   IF (BROW+ITERM .EQ. IROWX) GO TO 90        
C        
C     WE MUST JUST PICK UP A ZERO ROW, WRITE THE NON-ZERO STRING OUT    
C        
      BNBWRT = ITERM        
C     CALL ENDPUT (BLOCK)        
C     CALL PUTSTR (BLOCK)        
      ASSIGN 200 TO ENDPUT        
      ASSIGN  80 TO PUTSTR        
      GO TO 260        
 80   ITERM = 0        
      BROW  = IROWX        
C        
 90   GO TO (130,110,120), BADDR2        
C        
C     INPUT IS S.P. AND OUTPUT PACKING IN D.P.        
C        
 110  DELEM2 = DBLE(ELEML2)        
      DELEM1 = DBLE(ELEML1)        
      GO TO 130        
C        
C     INPUT IS D.P. AND OUPUT PACKING IN S.P.        
C        
 120  ELEML1 = SNGL(DELEM1)        
      ELEML2 = SNGL(DELEM2)        
C        
C     INPUT AND OUTPUT PACKING ARE THE SAME TYPE        
C        
 130  J = BPOINT        
C     IF (DEBUG) WRITE (NOUT,140) ITERM,BCOUNT,BNBRAV,BPOINT        
C140  FORMAT ('  PACK/ITERM,BCOUNT,BNBRAV,BPOINT@130=',3I6,I12)        
      DO 150 K = 1,NWPCK        
      RCORE(J) = ELEML(K)        
 150  J = J + 1        
      BPOINT = J        
      ITERM  = ITERM  + 1        
      BCOUNT = BCOUNT + NWPCK        
      IF (ITERM .LT. BNBRAV) GO TO 170        
C        
C     GINO BUFFER IS FULL, WRITE THE STRING OUT        
C        
      BNBWRT = ITERM        
C     CALL ENDPUT (BLOCK)        
C     CALL PUTSTR (BLOCK)        
      ASSIGN 200 TO ENDPUT        
      ASSIGN 160 TO PUTSTR        
      GO TO 260        
 160  ITERM = 0        
C        
C     REPEAT FOR MORE IF WE HAVE NOT REACHED THE LAST ROW        
C        
 170  IF (IROWX .GE. LROW) GO TO 180        
      IROWX = IROWX + 1        
      IOFF  = IOFF  + INTYPE        
      GO TO 20        
C        
C     LAST ROW REACHED, WRAP IT UP FOR THIS COLUMN PACKING        
C        
 180  BNBWRT = ITERM        
      CALL BLDPKN (NAME,BLOCK,MCB)        
      RETURN        
C        
C     -------END OF PACK ROUTINE-------        
C        
C     THE FOLLOWINGS ARE EXCERTS FROM VARIOUS SUPPORTING ROUTINES       
C     ===========================================================       
C        
C*****        
C     SUBROUTINE PUTSTR (BLK)        
C*****        
C        
 200  NAMEX = BLK(1)        
      IMHERE= 200        
      IF (DEBUG) WRITE (NOUT,205) IMHERE,BLK        
 205  FORMAT ('0 PACK/BLK@',I3,'...',4I4,I7,2I4,I14,4I4)        
      ENTRY = 12        
      IBLOCK( 3) = BLK( 3)        
      IBLOCK( 8) = BLK( 8)        
C        
      IF (ANDF(IBLOCK(8),MASK2F) .NE. MASK2F) GO TO 210        
      IBLOCK( 2) = BLK( 2)        
      IBLOCK(12) = BLK(12)        
C     CALL INIT (*50,RDWRT,JBUFF)        
C     GO TO 220        
      GO TO 300        
 210  IBLOCK( 9) = BLK( 9)        
      IBLOCK(10) = BLK(10)        
      IBLOCK(11) = BLK(11)        
      FILEX = IBLOCK(11)        
      JBUFF = ANDF(BUFADD(FILEX),MASK6F)        
 220  IMHERE = 220        
      IF (DEBUG) WRITE (NOUT,205) IMHERE,BLK        
C     CALL GINO (*450,*450,CORE(JBUFF),DUM1,DUM2,RDWRT)        
      GO TO 1100        
 230  DO 240 I = 5,11        
      BLK(I) = IBLOCK(I)        
 240  CONTINUE        
      IF (BPREC .EQ. 1) BPOINT = BPOINT*2 - 1        
      GO TO PUTSTR, (10,80,160)        
C        
C*****        
C     SUBROUTINE ENDPUT (BLK)        
C*****        
C        
 260  NAMEX = BLK(1)        
      IF (DEBUG) WRITE (NOUT,265) BLK        
 265  FORMAT (' PACK/BLK265...',4I4,I7,2I4,I14,4I4)        
      ENTRY = 13        
      DO 270 I = 2,12        
      IBLOCK(I) = BLK(I)        
 270  CONTINUE        
C        
      FILEX = IBLOCK(11)        
      JBUFF = ANDF(BUFADD(FILEX),MASK6F)        
      IMHERE = 280        
C     CALL GINO (*450,*450,CORE(JBUFF),DUM1,DUM2,RDWRT)        
      GO TO 1100        
 280  GO TO ENDPUT, (200)        
C        
C*****        
C     SUBROUTINE INIT (*,RDWRT,JBUFF)        
C*****        
C        
 300  NAM = NAMEX        
      IF (NAM .LT.    400) GO TO 320        
C     IF (NAM .NE. LSTNAM) GO TO 310        
      IF (NAM .NE. LSTNAM) GO TO 350        
      GO TO 330        
C310  CALL GETURN (NAM)        
C     GO TO 330        
 320  NAM = NAM - 100        
      FILEX  = UNITS(NAM)        
 330  IMHERE = 330        
C        
C     ERROR @ 330 - FILE MAY NOT BE OPENED        
C        
      IF (FILEX .EQ. 0) GO TO 450        
      JBUFF  = ANDF(BUFADD(FILEX),MASK6F)        
      IMHERE = 331        
      IF (JBUFF .EQ. 0) GO TO 450        
      RDWRT = RSHIFT(BUFADD(FILEX),24)        
      GO TO 220        
C        
C*****        
C     SUBROUTINE GETURN (NAMDUM)        
C*****        
C        
 350  NN = 2*NFIST - 1        
      DO 360 I = 1,NN,2        
      IF (FIST(I) .EQ. NAMEX) GO TO 370        
 360  CONTINUE        
      FILEX = 0        
      GO TO 420        
 370  J = FIST(I+1)        
      IF(J) 380,390,400        
 380  J = -J        
 390  IIII= EXFIAT(J+1)        
      GO TO 410        
 400  IIII= FIAT(J+1)        
 410  FILEX  = ANDF(IIII,32767)        
      NTAPE  = ANDF(IIII,32768)        
      PRVOPN = RSHIFT(UNITAB(FILEX),28)        
      UNITAB(FILEX) = RSHIFT(LSHIFT(UNITAB(FILEX),4),4)        
      XYZ(1) = RSHIFT(UNITAB(FILEX),12)        
      XYZ(2) = ANDF(UNITAB(FILEX),TWO121)        
      IF (NAMEX .LT. 400) UNITS(NAMEX-100) = FILEX        
 420  GO TO 330        
C        
C     ERROR MESSAGE.        
C        
C430 -TOO MANY TERMS        
 430  CONTINUE        
C440 -BUFFER OVERFLOW        
 440  CONTINUE        
C        
 450  WRITE  (NOUT,460) IMHERE        
 460  FORMAT ('0*** SYSTEM FATAL ERROR IN PACK @',I4)        
      CALL ERRTRC (0)        
C        
C*****        
C     INTERNAL ROUTINE TO WRITE A BLOCK        
C     USED ONLY BY GINO BELOW        
C*****        
C        
 500  BUFF(8+JBUFF1) = CLR        
C     CALL GINOIO (*450,WRT,BUFF(JBUFF4),500)        
      GO TO 600        
 510  BUFF(JBUFF4) = BUFF(JBUFF4) + 1        
      CBP = CLR0        
      CLR = CLR0        
      GO TO RET, (1205,1210,1325)        
C        
C*****        
C     INTERNAL ROUTINE TO WRITE CONTROL WORD IN BUFFER AT THE END       
C     OF A LOGICAL RECORD. USED ONLY BY GINO BELOW        
C*****        
C        
 550  BUFF(CLR+JBUFF1) = ORF(CBP-CLR,IHEADR)        
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
C     OPCODE = 2 ONLY, TO WRITE ONE BLOCK        
C     B IS BUFF, AND MHERE IS NOT USED HERE        
C        
C     IF FILE HAS NOT BEEN OPENED BEFORE, WE MUST GO TO REGULAR GINOIO  
C     BECAUSE OF THE ASSOCIATE-VARIABLE INVOLVED, OTHERWISE MISTERIOUS  
C     ERROR MIGHT OCCUR        
C        
 600  IF (R12345 .NE. -1234567890) GO TO 610        
      IMHERE = 600        
      IF (FILEX .LE. 1) GO TO 450        
      IF (IOPEN(FILEX) .EQ. 0) GO TO 610        
      I = R(FILEX)        
      IF (DEBUG) WRITE (NOUT,605)        
 605  FORMAT (/2X,'=========== GINOIO WRITE/605 ===========')        
      WRITE (FILEX,REC=I) (BUFF(J),J=JBUFF4,JEND)        
      R(FILEX) = R(FILEX) + 1        
      IF (KLOSE .NE. 1) GO TO 510        
      IOPEN(FILEX) = 0        
      CLOSE (UNIT=FILEX,STATUS='KEEP')        
      GO TO 510        
 610  IMHERE = 610        
      IF (DEBUG) WRITE (NOUT,615)        
 615  FORMAT (/2X,'====== CALLING GINOIO TO WRITE/615 ======')        
      CALL GINOIO (*450,2,BUFF(JBUFF4),610)        
      GO TO 510        
C        
C*****        
C     SUBROUTINE GINO (*,*,BUFF,A,INCT,RDWRT)        
C        
C     INTEGER     A(1),INCT(2),BUFF(1)        
C                 A,RDWRT,INCT, AND RETURN 2 ARE NOT USED HERE        
C        
C     ENTRY = 12, CALL PUTSTR (BLOCK)        
C     ENTRY = 13, CALL ENDPUT (BLOCK)        
C                 BLOCK(1) =  NAME FOR ALL STRING CALLS        
C*****        
C        
 1100 BUFOFF = JBUFF        
      BUFBGN = 1        
      BUFEND = NBUFF3        
      BUFFLG = BUFLGX        
      JBUFF1 = JBUFF  - 1        
      JBUFF2 = JBUFF1 + 2        
      JBUFF3 = JBUFF1 + 3        
      JBUFF4 = JBUFF1 + 4        
      JEND   = JBUFF3 + NBUFF3        
      IF (ENTRY-12) 1200,1200,1300        
C        
C ... GINO-PUTSTR        
C        
 1200 CBP = BUFF(JBUFF2)        
      CLR = BUFF(JBUFF3)        
      IF (ANDF(GFLAG,MASK2F) .NE. MASK2F) GO TO 1210        
      GFLAG = ANDF(GFLAG,MASK6F)        
      IF (CLR+4 .LT. NBUFF2) GO TO 1205        
      BUFF(CLR+JBUFF1) = LASTCW        
      ASSIGN 1205 TO RET        
      GO TO 500        
C        
 1205 UNITAB(FILEX) = TWO12*BUFF(JBUFF4) + CLR        
      GFILEX = FILEX        
      GPREC  = 0        
      IF (GTYPE.EQ.2 .OR. GTYPE.EQ.4) GPREC = 1        
      GSFT = 0        
      IF (GTYPE.EQ.2 .OR. GTYPE.EQ.3) GSFT  = 1        
      IF (GTYPE .EQ. 4) GSFT = 2        
      IF (DEBUG) WRITE (NOUT,1206) GPREC,GTYPE,GSFT,GFILEX        
 1206 FORMAT ('  PACK/GPREC,GTYPE,GSFT,GFILEX@1206=',4I6)        
      CBP  = CBP + 1        
      JREG = ORF(CHEAD,16*GTYPE+GFORM)        
      BUFF(CBP+JBUFF1) = ORF(TWO12*ANDF(GCOL,TWO121),JREG)        
C        
 1210 IAVIAL = CBP + 4        
      IF (GFORM .NE. 0) IAVIAL = IAVIAL + 2        
      IAVIAL = NBUFF2 - IAVIAL        
      IF (GSFT   .NE. 0) IAVIAL = IAVIAL/(2*GSFT)        
      IF (DEBUG) WRITE (NOUT,1211) IAVIAL,CBP,GFORM,GSFT,NBUFF2        
 1211 FORMAT (5X,'PACK/IAVIAL,CBP,GFORM,GSFT,NBUFF2@1211=',5I7)        
      IF (IAVIAL .GT. 0) GO TO 1230        
 1215 CBP = CBP + 1        
      BUFF(CBP+JBUFF1) = LSTSTR        
      IF (CBP-NBUFF2) 1215,1225,1220        
 1220 IMHERE = 1220        
      GO TO 450        
C        
 1225 BUFF(CLR  +JBUFF1) = ORF(CBP-CLR,MTXHDR)        
      BUFF(CBP+1+JBUFF1) = ORF(UNITAB(FILEX),MASK3)        
      BUFF(CBP+2+JBUFF1) = LASTCW        
      CLR = CBP + 2        
      ASSIGN 1210 TO RET        
      GO TO 500        
C        
 1230 GNBWRT = 0        
      GNBRAV = IAVIAL        
C     IF (DEBUG) WRITE (NOUT,1231) GNBRAV        
C1231 FORMAT (30X,'PACK/GNBRAV@1231=',I8)        
      IF (GPREC .NE. 0) GO TO 1235        
      GPOINT = LOCFX(BUFF(CBP+3+JBUFF1)) - LOCFX1 + 1        
C     IF (DEBUG) WRITE (NOUT,1232) GPOINT        
C1232 FORMAT (30X,'PACK/GPOINT@1232=',I8)        
      GO TO 1245        
C        
 1235 GPOINT = (LOCFX(BUFF(CBP+3+JBUFF1))-LOCFX1)*(LQRO/1000)        
      IF (MOD(GPOINT,8) .EQ. 0) GO TO 1240        
      CBP = CBP + 1        
      BUFF(CBP+JBUFF1) = DUMSTR        
      GO TO 1210        
C        
 1240 GPOINT  = GPOINT/8 + 1        
C     IF (DEBUG) WRITE (NOUT,1241) GPOINT        
C1241 FORMAT (30X,'PACK/GPOINT@1241=',I8,'<==')        
 1245 BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM  = NAMEX        
      GO TO 230        
C        
C ... GINO-ENDPUT        
C        
 1300 CBP = BUFF(JBUFF2)        
      CLR = BUFF(JBUFF3)        
      IF (GNBWRT .EQ. 0) GO TO 1305        
      IMHERE = 1300        
      IF (GNBWRT .GT. GNBRAV) GO TO 430        
      BUFF(CBP+1+JBUFF1) = ORF(GNBWRT,SHEAD)        
      BUFF(CBP+2+JBUFF1) = GROW        
      CBP = CBP + 2 + LSHIFT(GNBWRT,GSFT)        
      IMHERE = 1302        
      IF (CBP .GT. NBUFF2) GO TO 440        
      IF (GFORM .EQ.  0) GO TO 1305        
      BUFF(CBP+1+JBUFF1) = ORF(GNBWRT,STRAIL)        
      BUFF(CBP+2+JBUFF1) = GROW+GNBWRT - 1        
      CBP = CBP + 2        
      IMHERE = 1304        
      IF (CBP .GT. NBUFF2) GO TO 440        
 1305 IF (GNBWRT .EQ. GNBRAV) GO TO 1310        
      GO TO 1325        
 1310 CBP = CBP + 1        
      BUFF(CBP+JBUFF1) = LSTSTR        
      IF (CBP-NBUFF2) 1310,1320,1315        
 1315 IMHERE = 1315        
      GO TO 440        
 1320 BUFF(CLR+  JBUFF1) = ORF((CBP-CLR),MTXHDR)        
      BUFF(CBP+1+JBUFF1) = ORF(UNITAB(FILEX),MASK3)        
      BUFF(CBP+2+JBUFF1) = LASTCW        
      CLR = CBP + 2        
      ASSIGN 1325 TO RET        
      GO TO 500        
C        
 1325 IF (GFLAG .NE. 1) GO TO 1330        
      CBP = CBP + 1        
      BUFF(CBP+JBUFF1) =        
     1     ORF(CTRAIL,TWO12*ANDF(GCOL,TWO121)+16*GTYPE+GFORM)        
      IHEADR = MTXHDR        
      GO TO 550        
 1330 BUFF(JBUFF2) = CBP        
      BUFF(JBUFF3) = CLR        
      LSTNAM  =  NAMEX        
      GO TO 280        
      END        
