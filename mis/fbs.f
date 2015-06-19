      SUBROUTINE FBS (ZS,ZD)        
C        
C     GIVEN A LOWER TRIANGULAR FACTOR WITH DIAGONAL SUPERIMPOSED, AND   
C     WRITTEN WITH TRAILING STRING DEFINITION WORDS, FBS WILL PERFORM   
C     THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE A LINEAR     
C     SYSTEM OF EQUATIONS.        
C        
C     MODIFIED FOR SPEED IMPROVEMENT BY G.CHAN/UNISYS -        
C     9/88,  ONE MORE BUFFER SPACE IS ADDED SO THAT THE TRIANGULAR      
C     FACTOR FILE CAN REMAIN OPENED DURING THE ENTIRE FBS PROCESSING.   
C     11/91, NEW FBS METHODS (SUBROUTINES FBSI, FBSII, FBSIII, FBSIV AND
C     UNPSCR) FOR FBS OPERATION WITH EXCESSIVE PASSES. STRING DATA AND  
C     TRAILING STRING ARE NOT USED IN THIS NEW METHOD        
C     NOTE - CALLER MUST SUPPLY A SCRATCH FILE VIA 32ND WORD OF /FBSX/  
C     IF NEW FBS METHOD IS USED        
C     NOTE - THE VALUE OF 'LAST' WAS NOT COMPUTED ACCURATELY BEFORE     
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL         IDENT        
      INTEGER         SUBNAM(5) ,BLOCK(15)        
      REAL            T1        ,T2       ,ZS(1)    ,XS(4)    ,YS(4)    
      DOUBLE  PRECISION          ZD(1)    ,XD       ,YD        
      CHARACTER       UFM*23    ,UWM*25   ,UIM*29        
      COMMON /XMSSG / UFM       ,UWM      ,UIM        
      COMMON /FBSX  / DBL(7)    ,DBU(7)   ,DBB(7)   ,DBX(7)   ,LCORE   ,
     1                PREC      ,SIGN     ,SCRX        
      COMMON /SYSTEM/ SYSBUF    ,NOUT     ,SKIP(91) ,KSYS94        
      COMMON /NAMES / RD        ,RDREW    ,WRT      ,WRTREW   ,REW     ,
     1                NOREW     ,EOFNRW   ,RSP      ,RDP      ,CSP     ,
     2                CDP        
      COMMON /TYPE  / PRC(2)    ,WORDS(4) ,RLCMPX(4)        
      COMMON /PACKX / ITYPE1    ,ITYPE2   ,I1       ,J1       ,INCR1    
      COMMON /UNPAKX/ ITYPE3    ,I2       ,J2       ,INCR2        
      COMMON /ZNTPKX/ XD(2)     ,IX       ,EOL        
      COMMON /ZBLPKX/ YD(2)     ,IY        
      EQUIVALENCE     (DBL(2),NL),   (DBB(5),TYPEB), (DBX(5),TYPEX),    
     1                (XD(1),XS(1)), (YD(1),YS(1))        
      DATA    SUBNAM/ 3HFBS  ,2*4HBEGN, 3HEND, 4HFBS. /        
C        
C     GENERAL INITIALIZATION        
C        
      CALL SSWTCH (11,L11)        
      BUF2   = LCORE - SYSBUF        
      BUF1   = BUF2  - SYSBUF        
      RC     = RLCMPX(TYPEB)        
      TYPEL  = DBL(5)        
      WDS    = WORDS(TYPEL)        
      NWDS   = WDS*NL        
      NBRLOD = DBB(2)        
      IDENT  = .FALSE.        
      IF (DBB(4) .EQ. 8) IDENT = .TRUE.        
      IF (IDENT) NBRLOD = NL        
      SWITCH = 1        
      IF (TYPEL.EQ.RSP .AND. RC.EQ.2) SWITCH = 2        
      IF (TYPEL.EQ.RDP .AND. RC.EQ.2) SWITCH = 3        
      DBL1   = DBL(1)        
      NEWFBS = 0        
      NNN    = BUF1 - 1        
      NVECS  = NNN/NWDS        
      IF (NVECS .EQ. 0) CALL MESAGE (-8,NWDS-NNN,SUBNAM)        
      IF (SWITCH .NE. 1) NVECS = NVECS/2        
      NPASS  = (NBRLOD+NVECS-1)/NVECS        
      SUBNAM(2) = SUBNAM(3)        
C        
C     SKIP NEW FBS METHODS IF DIAG 41 IS TURNED ON, OR SCRATCH FILE IS  
C     NOT AVAILABLE        
C        
C     IF EXCESSIVE PASSES, SET NEWFBS FLAG TO 4 FOR NEW FBS METHODS.    
C     CALL UNPSCR TO MOVE THE LOWER TRIANGULAR FACTOR INTO A SCRATCH    
C     FILE. IN THIS NEW METHOD, HALF OF THE OPEN CORE, Z(1)...Z(NNN),   
C     WILL BE USED TO HOLD THE LOAD VECTORS, AND THE REMAINING HALF,    
C     Z(NN1)...Z(NN2), TO HOLD THE LOWER TRIANGULAR FACTOR COLUMNS.     
C     IF PLENTY OF OPEN CORE IS AVAILABLE, ADJUST THE CORE ALLOCATION   
C     TO 6/4, 7/3, 8/2, OR 9/1 RATIO DISTRIBUTION FOR THE TWO ARRAYS.   
C        
C     THE NEW FBS USES ONLY ONE GINO BUFFER.        
C     THE 4,5,6,7 TRAILER WORDS OF DBL WILL BE ALTERED BY UNPSCR.       
C        
C          1<-NVECS->1        
C          +---------+            1\        
C          1         1            1 \        
C          1  LOAD   1            1  \        
C          1  VEC    1            1 T \        
C          1         1    NNN     1 F  \        
C          +---------+      \     +-----+    BUFFER        
C      +---------------------+--------------+------+  OPEN CORE        
C       \                     \ <-- NET --> \        
C        1                    NN1           NN2        
C        
C                     IF (SCRX.LT.200 .OR. SCRX.GE.400) GO TO 30  ! TESTING ONLY
      IF (NPASS.LT.7 .OR. SCRX.LT.200 .OR. SCRX.GE.400) GO TO 30        
C
C     CURRENTLY FBSIII AND FBSIV ARE NOT GETTING THE RIGHT ANSWERS, AND
C     MORE TIME IS NEEDED TO CHECK THEM OUT. REMOVE THEM TEMPORARY
C     (FBSI AND FBSII WORK OK)                         G.C./UNISYS 4/93
C
      IF (TYPEL .GE. 3) GO TO 30
C
      J = MOD(KSYS94,1000)/100        
      IF (J .EQ. 1) GO TO 30        
      CALL SSWTCH (41,J)        
      IF (J .EQ. 1) GO TO 30        
      NEWFBS= 4        
      DBL1  = SCRX        
      NN2   = BUF2 - 1        
      NVECS = (NN2/2)/NWDS        
      IF (LCORE .GT.  190000) NVECS = NVECS*6/5        
      IF (LCORE .GT.  290000) NVECS = NVECS*7/6        
      IF (LCORE .GT.  590000) NVECS = NVECS*8/7        
C     IF (LCORE .GT. 1190000) NVECS = NVECS*9/8        
      IF (SWITCH .NE. 1) NVECS = NVECS/2        
      NVECS = MIN0(NVECS,NBRLOD)        
      NNN   = NVECS*NWDS        
      NN1   = ((NNN+1)/2)*2 + 1        
      NET   = NN2 - NN1        
      IF (L11 .EQ. 1) WRITE (NOUT,10) NVECS,NNN,NN1,NN2,NET,NPASS,      
     1                SWITCH,NBRLOD,NL,NWDS        
   10 FORMAT ('0...FBS/ NVECS,NNN,NN1,NN2,NET,NPASS,SWITCH,NBRLOD,NL',  
     1        ',NWDS =', /1X,10I8)        
      SUBNAM(1) = SUBNAM(5)        
      CALL CONMSG (SUBNAM,2,0)        
      CALL UNPSCR (DBL,DBL1,ZS,BUF2,BUF1,NET,0,2)        
C        
C     IF ALLOCATED CORE SPACE IS MORE THAN ENOUGH FOR ENTIRE TRIANGULAR 
C     FACTER, RETURN THE UNUSED PORTION TO THE LOAD VECTORS ARRAY BY    
C     MOVING NN1 POINTER FORWARD. HOWEVER, MAKE SURE THAT THE LOAD      
C     VECTORS CAN FILL UP ALL THE ADDITIONAL SPACE AND NO EMPTY ROOM    
C     LEFT.        
C        
      IF (DBL(6) .GE. NET) GO TO 40        
      NN1   = NN2 - DBL(6) - 2        
      I     = NVECS*NWDS        
      IF (I .LT. NN1) NN1 = I + 1        
      NN1   = (NN1/2)*2 + 1        
      NNN   = NN1 - 1        
      NET   = NN2 - NN1        
      NVECS = NNN/NWDS        
      IF (SWITCH .NE. 1) NVECS = NVECS/2        
      NVECS = MIN0(NVECS,NBRLOD)        
      I     = (NBRLOD+NVECS-1)/NVECS        
      IF (L11 .EQ. 1) WRITE (NOUT,20) NVECS,NNN,NN1,NN2,NET,DBL(6),I    
   20 FORMAT ('0REVISED - NVECS,NNN,NN1,NN2,NET,TOTAL,NPASS =',7I9)     
      GO TO 40        
C        
   30 CALL CONMSG (SUBNAM,2,0)        
   40 NPASS  = (NBRLOD+NVECS-1)/NVECS        
      I2     = 1        
      J2     = NL        
      INCR2  = 1        
      I1     = 1        
      J1     = NL        
      INCR1  = 1        
      ITYPE1 = TYPEL        
      ITYPE2 = TYPEX        
      ITYPE3 = SIGN*TYPEL        
      DBX(2) = 0        
      DBX(6) = 0        
      DBX(7) = 0        
      NNNDBL = NNN/2        
      NTERMS = RLCMPX(TYPEL)*NL        
      K1     = 1        
      OPRD   = RDREW        
      OPWRT  = WRTREW        
      BLOCK(1) = DBL(1)        
C        
C     OPEN LOWER TRIANGULAR FACTOR FILE (DBL1), CHECK TIMING, AND ISSUE 
C     MESSAGE        
C        
      CALL GOPEN (DBL1,ZS(BUF2),RDREW)        
      IF (NPASS .GE. 10) L11 = 1        
      IF (L11 .NE. 1) GO TO 130        
      CALL PAGE2 (-4)        
      WRITE  (NOUT,50) UIM,NPASS        
   50 FORMAT (A29,' FROM FBS - NO. OF PASSES NEEDED TO COMPLETE FBS ',  
     1       'OPERATION =',I5)        
      IF (NPASS.LE.15 .OR. L11.EQ.-1) GO TO 120        
      WRITE  (NOUT,60) NWDS,NVECS        
   60 FORMAT (5X,'FURTHER INCREASE OF OPEN CORE COULD REDUCE NO. OF ',  
     1       'PASSES.',9X,'CORE UTILIZATION =',I8,' BY',I6)        
      IF (NEWFBS .EQ. 4) WRITE (NOUT,70) NET        
   70 FORMAT (1H+,106X,',  AND',I9,' WORDS')        
      GO TO 120        
C        
  100 IF (L11 .LE. 0) GO TO 130        
      CALL CPUTIM (T2,T2,1)        
      T2  = ABS(T2-T1)        
      IF (L11 .GT. 0) WRITE (NOUT,110) T2        
  110 FORMAT (5X,'TIME TO COMPLETE ONE PASS =',F8.4,' CPU SECONDS',//)  
      L11 = -1        
      CALL TMTOGO (J)        
      I   = NPASS*T2*1.05        
      IF (J .LT. I) CALL MESAGE (-50,I,SUBNAM)        
      GO TO 130        
  120 CALL CPUTIM (T1,T1,1)        
C        
C     OPEN LOAD VECTORS FILE (DBB) AND COMPUTE EXTENT OF THIS PASS      
C        
  130 KN    = MIN0(K1+NVECS-1,NBRLOD)        
      LAST  = (KN-K1+1)*NWDS        
      OPCLS = NOREW        
      IF (KN .EQ. NBRLOD) OPCLS = REW        
      IF (IDENT) GO TO 280        
      CALL GOPEN (DBB,ZS(BUF1),OPRD)        
      GO TO (140,180,230), SWITCH        
C        
C     NORMAL CASE - FILL CORE WITH LOAD VECTORS        
C        
  140 DO 170 L = 1,LAST,NWDS        
      CALL UNPACK (*150,DBB,ZS(L))        
      GO TO 170        
  150 LN = L + NWDS - 1        
      DO 160 LL = L,LN        
  160 ZS(LL) = 0.        
  170 CONTINUE        
      GO TO 390        
C        
C     SPECIAL CASE - FACTOR IS RSP AND VECTORS ARE CSP        
C        
  180 LAST = 2*(KN-K1+1)*NWDS        
      L = 0        
      DO 190 K = 1,NNNDBL        
  190 ZD(K) = 0.0D+0        
      DO 220 K = K1,KN        
      ICSPSG = CSP*SIGN        
      CALL INTPK (*210,DBB,0,ICSPSG,0)        
  200 CALL ZNTPKI        
      ZS(L+IX   ) = XS(1)        
      ZS(L+IX+NL) = XS(2)        
      IF (EOL .EQ. 0) GO TO 200        
  210 L = L + 2*NL        
  220 CONTINUE        
      GO TO 390        
C        
C     SPECIAL CASE - FACTOR IS RDP AND VECTORS ARE CDP        
C        
  230 LAST = 2*(KN-K1+1)*NWDS        
      L = 0        
      DO 240 K = 1,NNNDBL        
  240 ZD(K) = 0.0D+0        
      DO 270 K = K1,KN        
      ICDPSG = CDP*SIGN        
      CALL INTPK (*260,DBB,0,ICDPSG,0)        
  250 CALL ZNTPKI        
      ZD(L+IX   ) = XD(1)        
      ZD(L+IX+NL) = XD(2)        
      IF (EOL .EQ. 0) GO TO 250        
  260 L = L + 2*NL        
  270 CONTINUE        
      GO TO 390        
C        
C     SPECIAL CASE - GENERATE IDENTITY MATRIX        
C        
  280 DO 290 K = 1,NNNDBL        
  290 ZD(K) = 0.0D+0        
      L = 0        
      GO TO (300,320,340,360), TYPEL        
  300 DO 310 K = K1,KN        
      ZS(L+K) = 1.0        
  310 L = L + NTERMS        
      GO TO 400        
  320 DO 330 K = K1,KN        
      ZD(L+K) = 1.0D+0        
  330 L = L + NTERMS        
      GO TO 400        
  340 DO 350 K = K1,KN        
      ZS(L+2*K-1) = 1.0        
  350 L = L + NTERMS        
      GO TO 400        
  360 DO 370 K = K1,KN        
      ZD(L+2*K-1) = 1.0D+0        
  370 L = L + NTERMS        
      GO TO 400        
C        
C    CLOSE LOAD VECTORS FILE (DBB).        
C    START FORWARD-BACKWARD SUBSTITUTION ON LOAD VECTORS NOW IN CORE    
C        
  390 CALL CLOSE  (DBB,OPCLS)        
  400 CALL REWIND (DBL1)        
      CALL FWDREC (*610,DBL1)        
C        
      J = TYPEL + NEWFBS        
      GO TO (410,420,430,440,450,460,470,480), J        
C        
C     NASTRAN ORIGINAL METHODS        
C        
  410 CALL FBS1 (BLOCK,ZS,ZS(LAST),NWDS)        
      GO TO 500        
  420 CALL FBS2 (BLOCK,ZS,ZS(LAST),NWDS)        
      GO TO 500        
  430 CALL FBS3 (BLOCK,ZS,ZS(LAST),NWDS)        
      GO TO 500        
  440 CALL FBS4 (BLOCK,ZS,ZS(LAST),NWDS)        
      GO TO 500        
C        
C     NEW FBS METHODS        
C        
  450 CALL FBSI  (DBL1,ZS,ZS(NN1),NWDS)        
      GO TO 500        
  460 CALL FBSII (DBL1,ZS,ZS(NN1),NWDS)        
      GO TO 500        
  470 CALL FBSIII (DBL1,ZS,ZS(NN1),NWDS)        
      GO TO 500        
  480 CALL FBSIV (DBL1,ZS,ZS(NN1),NWDS)        
C        
C     OPEN AND PACK SOLUTION VECTORS ONTO OUTPUT FILE (DBX)        
C        
  500 CALL GOPEN (DBX,ZS(BUF1),OPWRT)        
      GO TO (510,530,560), SWITCH        
C        
C     NORMAL CASE - CALL PACK        
C        
  510 DO 520 L = 1,LAST,NWDS        
      CALL PACK (ZS(L),DBX,DBX)        
  520 CONTINUE        
      GO TO 600        
C        
C     SPECIAL CASE - FACTOR IS RSP AND VECTORS ARE CSP, CALL BLDPK      
C        
  530 L = 0        
      DO 550 K = K1,KN        
      CALL BLDPK (CSP,TYPEX,DBX,0,0)        
      DO 540 I = 1,NL        
      YS(1) = ZS(L+I   )        
      YS(2) = ZS(L+I+NL)        
      IY = I        
      CALL ZBLPKI        
  540 CONTINUE        
      CALL BLDPKN (DBX,0,DBX)        
      L = L + 2*NL        
  550 CONTINUE        
      GO TO 600        
C        
C     SPECIAL CASE - FACTOR IS RDP AND VECTORS ARE CDP, CALL BLDPK      
C        
  560 L = 0        
      DO 580 K = K1,KN        
      CALL BLDPK (CDP,TYPEX,DBX,0,0)        
      DO 570 I = 1,NL        
      YD(1) = ZD(L+I   )        
      YD(2) = ZD(L+I+NL)        
      IY = I        
      CALL ZBLPKI        
  570 CONTINUE        
      CALL BLDPKN (DBX,0,DBX)        
      L = L + 2*NL        
  580 CONTINUE        
C        
C     CLOSE OUTPUT FILE, AND TEST FOR MORE PASSES        
C        
  600 CALL CLOSE (DBX,OPCLS)        
      IF (KN .EQ. NBRLOD) GO TO 620        
      K1   = KN + 1        
      OPRD = RD        
      OPWRT= WRT        
      GO TO 100        
C        
C     ERROR        
C        
  610 CALL MESAGE (-2,DBL1,SUBNAM)        
C        
C     JOB DONE. CLOSE TRIANGULAR FACTOR FILE.        
C        
C     IF NEW FBS IS USED, RESTORE DBL TRAILER JUST IN CASE, AND MAKE    
C     SURE SCARTCH FILE IS PHYSICALLY REDUCED TO ZERO SIZE.        
C     AND JOB IS DONE        
C        
  620 CALL CLOSE (DBL1,REW)        
      IF (NEWFBS .NE. 4) GO TO 630        
      CALL RDTRL (DBL(1))        
      CALL GOPEN (SCRX,ZS(BUF2),WRTREW)        
      CALL CLOSE (SCRX,REW)        
  630 SUBNAM(2) = SUBNAM(4)        
      CALL CONMSG (SUBNAM,2,0)        
      RETURN        
      END        
