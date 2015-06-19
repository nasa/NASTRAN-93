      SUBROUTINE GINO (*,*,BUFF,A,INCT,RDWRT)        
C        
C     REVISED 1/06/87, BY G.CHAN/UNISYS        
C     REVISED 8/ 8/88, CONSOLIDATED GINO/GINOIO/DCODE INTO ONE ROUTINE  
C                      THIS VERSION USES THE NEW GINOIO        
C     REVISED 5/22/89, ALL HEX CONSTANTS ARE DEFINED IN DATA STATEMENTS 
C                      A PREREQUISITE FOR THIS ROUTINE TO BE USED IN    
C                      OTHER FORTRAN 77 COMPILERS.        
C                      LOGFILE CONTROL FOR OPEN/CLOSE MESSAGES VIA      
C                      DIAG 20        
C     REVISED 6/06/91, ADDING SUBROUTINE QREAD/QWRITE CALLS        
C     REVISED 8/01/91, PICK UP WORD OR BYTE INFORMATION FOR RECL AND    
C                      LOCFX VARIABLES FROM THE 4TH WORD OF /MACHIN/    
C        
C     THE FOLLOWING 2 NASTRAN FUNCTIONS WERE REPLACED DIRECTLY BY THE   
C     FORTRAN 77 FUNCTIONS        
C        
C         LSHIFT(I,J)  BY   ISHFT(I, J)        
C         RSHIFT(I,J)  BY   ISHFT(I,-J)        
C        
C UNIX:        
C     REMOVE ABOVE 2 ON-LINE FUNCTIONS IF ISHFT SYSTEM FUNCTION IS NOT  
C     AVAILABLE. L/RSHIFT ARE ALREADY ENTRY POINTS IN SUBROUTINE MAPFNS.
C        
      IMPLICIT INTEGER       (A-Z)        
      INTEGER         A(1)  ,INCT(2),BUFF(8)  ,NAMBCD(2), SUB(2)        
      REAL            X     ,Y        
      CHARACTER*44    DSNAMES
      CHARACTER*8     OPX(9)        
      COMMON /DSNAME/ DSNAMES(80)
      COMMON /GINOX / LENGTH,FILEX ,EOR   ,OP    ,ENTRY ,LSTNAM    ,    
     1                N     ,NAME  ,NTAPE ,NBLOCK,NLR   ,UNITAB(75),    
     2                BUFADD(75)   ,NBUFF3,PRVOPN,UNITS(300)       ,    
     3                BLOCK(20)    ,KLOSE ,LOCFX1,R12345,R(75)     ,    
     4                IOPEN(75)        
      COMMON /MACHIN/ MAC(3),LQRO        
      COMMON /SYSTEM/ SYSBUF,NOUT        
      COMMON /ZZZZZZ/ CORE(1)        
      COMMON /L15 L8/ L15        
      COMMON /BUFCOM/ BUFOFF,BUFBGN,BUFEND,BUFFLG        
      EQUIVALENCE     (FILEX    ,    F ), (PRVOPN   ,   RW )        
      EQUIVALENCE     (BLOCK( 2),BTYPE ), (BLOCK( 3),BFORM ),        
     1                (BLOCK( 4),BROW  ), (BLOCK( 5),BPOINT),        
     2                (BLOCK( 6),BNBRAV), (BLOCK( 7),BNBWRT),        
     3                (BLOCK( 8),BFLAG ), (BLOCK( 9),BSFT  ),        
     4                (BLOCK(10),BPREC ), (BLOCK(11),BFILEX),        
     5                (BLOCK(12),BCOL  )        
      DATA     OPX  / 2*'EOF&RWD', 'RWD&RD', 'RWD&WRT', 'READ', 'WRITE',
C              OPEN :     -2(,-1)      0         1         2        3   
     1                'EOF&RWD',  'NO EOF', 'EOF ONLY'                 /
C              CLOSE:     1          2         3        
      DATA     REW, WRT, RD, BS1B, FS1B, REPOS, CLS, RR, CLSREW, REWRD /
     1           1,   2,  3,    4,    5,     6,   7,  8,      9,    10 /
      DATA          CLR0       ,  BLANK     ,  BUFLGX    ,  BUFCLL     /
     1              9          ,  4H        ,  0         ,  0          /
      DATA          KOC        ,  KOCL      ,  SUB       ,  LOGFL      /
     1              0          ,  1000000   ,  4HGINO,1H ,  4          /
C VAX:        
      DATA          RECHDR     ,  MTXHDR    ,  LASTCW    ,  EQF        /
     1              'F1111000'X, 'F2222000'X, 'F5555000'X, 'F7777000'X /
      DATA          SHEAD      ,  STRAIL    ,  DUMSTR    ,  LSTSTR     /
     1              'F8888000'X, 'F9999000'X, 'FAAAA000'X, 'FBBBB000'X /
      DATA          CHEAD      ,  CTRAIL    ,  MASK1F    ,  MASK2F     /
     1              '40000000'X, '80000000'X, '0000000F'X, 'FF000000'X /
      DATA          MASK3F     ,  MASK4F    ,  MASK6F    ,  MASK01     /
     1              '00000FFF'X, '0000FFFF'X, '00FFFFFF'X, '00000001'X /
      DATA          MASK10     ,  MASK20    ,  MASK30    ,  MASK77     /
     1              '10000000'X, '20000000'X, '30000000'X, '77777777'X /
      DATA          MASK1      ,  MASK2     ,  MASK5     ,  MASK7      /
     1              '00001111'X, '00002222'X, '00005555'X, '00007777'X /
      DATA          MASK8      ,  MASK9     ,  MASKA     ,  MASKB      /
     1              '00008888'X, '00009999'X, '0000AAAA'X, '0000BBBB'X /
C UNIX:        
C     DATA          RECHDR     ,  MTXHDR    ,  LASTCW    ,  EQF        /
C    1              X'F1111000', X'F2222000', X'F5555000', X'F7777000' /
C     DATA          SHEAD      ,  STRAIL    ,  DUMSTR    ,  LSTSTR     /
C    1              X'F8888000', X'F9999000', X'FAAAA000', X'FBBBB000' /
C     DATA          CHEAD      ,  CTRAIL    ,  MASK1F    ,  MASK2F     /
C    1              X'40000000', X'80000000', X'0000000F', X'FF000000' /
C     DATA          MASK3F     ,  MASK4F    ,  MASK6F    ,  MASK01     /
C    1              X'00000FFF', X'0000FFFF', X'00FFFFFF', X'00000001' /
C     DATA          MASK10     ,  MASK20    ,  MASK30    ,  MASK77     /
C    1              X'10000000', X'20000000', X'30000000', X'77777777' /
C     DATA          MASK1      ,  MASK2     ,  MASK5     ,  MASK7      /
C    1              X'00001111', X'00002222', X'00005555', X'00007777' /
C     DATA          MASK8      ,  MASK9     ,  MASKA     ,  MASKB      /
C    1              X'00008888', X'00009999', X'0000AAAA', X'0000BBBB' /
C        
C     MASK4F = '0000FFFF'X (OR X'0000FFFF') = 65535        
C        
C    /GINOX/        
C     LENGTH = GINO BUFFER LENGTH (=NO. OF I/O WORDS GINO READS OR WRITE
C     FILEX  = GINO FILE BEING USED PRESENTLY        
C     EOR    = END OF RECORD MARK?        
C     OP     = SUB OPERATION FLAG - e.g. REWIND, EOF, CLOSE etc.        
C     ENTRY  = GINO OPERATION REQUEST, RD, WRT, BACKSPACE, FORWARD SPACE
C     LSTNAM = NAME OF GINO FILE LAST USED        
C     N      = NO. OF WORDS SKIP FORWARD(+), OR BACKWARD(-)        
C     NAME   = NAME OF PRESENT GINO FILE        
C     NTAPE  = ?        
C     NBLOCK = DIRECT ACCESS FILE RECORD NUMBER (AN INDEX)        
C     NLR    = POINTER TO LAST CONTROL WORD IN BLOCK        
C     UNITAB = POINTERS TO THE LAST RECORDS WRITTEN  (UP TO 75 FILES)   
C     BUFADD = FLAGS THAT FILES ARE OPENED OR CLOSED (UP TO 75 FILES)   
C     NBUFF3 = SYSBUF-3 (= SYSTEM-4, UNIVAC, VAX)        
C     PRVOPN = PREVIOUS OPEN FLAG        
C     UNITS  =        
C     KLOSE  = (NOT USED HERE)        
C     BLOCK  = 20-WORD STRING COMMUNICATION BLOCK (PROG.MANUAL P.3.4-19)
C             WORD   DESCRIPTION                        I/O ENTRY PTS.  
C           -------  --------------------------------   --------------- 
C           1)       GINO FILE NAME                     ALL(I)        
C           2)BTYPE  TYPE OF ELEMENTS,  1,2,3,4 FOR     PUTSTR(I)       
C                    S.P.,D.P.,S.P.CMPLX & D.P.COMPLX   GETSTR,GETSTB(O)
C           3)BFORM  FORMAT OF DATA, 0 FOR REGULAR      PUTSTR(I)       
C                                    1 FOR STRING       GETSTR,GETSTB(O)
C           4)BROW   ROW POSITN OF FIRST ELEM. IN STRG  ENDPUT(I)       
C                             "    LAST   "             GETSTR,GETSTB(O)
C           5)BPOINT POINTER TO STRING, OFFSET RELATIVE GETSTR,GETSTB(O)
C                    TO OPEN CORE /XNSTRN/              & PUTSTR(O)     
C           6)BNBRAV NO. OF TERMS AVAIALBE IN BUFFER    GETSTR,GETSTB(O)
C                                                       & PUTSTR(O)     
C           7)BNNWRT NUMBERS OF TERMS WRITTEN IN STRG   ENDPUT(I)       
C                    (SET TO ZERO FROM PUTSTR IF BNBRAV IS USED UP)     
C           8)BFLAG  BEGIN/END FLAG,  -1,0,+1           ALL(I)        
C                    BEGIN WITH FIRST BYTE ZERO (or -1) ENDGET,ENDTAB(O)
C                    END   WITH FIRST BYTE ONE  (or +1)        
C           9)BSFT   0,1,1,2 FOR S.P.,D.P., OR COMPLEX  (INTERNAL USE BY
C          10)BPREC  0,1,0,1 FOR S.P.,D.P., OR COMPLEX   VARIOUS ENTRY  
C          11)BFILEX CURRENT FILE BEING USED             POINTS)        
C          12)BCOL   COLUMN NUMBER                      PUTSTR(I)       
C                                                       GETSTR,GETSTB(O)
C          13)BCOUNT                                    (13 THRU 16 ARE 
C          14)BADDR1 =1,2,OR 3                           USED IN PACK,  
C          15)BADDR2 =1,2,OR 3                           UNPACK, INTPK  
C          16)BEOL   END OF LOGICLA RECORD???            ONLY)        
C          17-20)    (NOT USED)        
C        
C        
C    /XNSTRN/        
C     CORE   = PSEUDO-OPEN CORE        
C              CORE(1) IS THE REFERENT POINT FOR ALL OPEN CORES USED    
C              IN NASTRAN WITHIN A NASTRAN LINK.        
C              /XNSTRN/ MUST RESIDE UP IN THE ROOT SEGMENT SO THAT THE  
C              CORE ADDRESS OF CORE(1) REMAINS CONSTANT THROUGHOUT      
C              EACH NASTRAN LINK.        
C        
C    /BUFCOM/        
C     BUFOFF = BUFFER OFFSET, BETWEEN ADDRESSED OF 'CORE' & 'BUFF'      
C     BUFBGN = POINTER TO THE BEGINNING OF GINO BUFFER = 1        
C     BUFEND = POINTER TO THE END       OF GINO BUFFER = NBUFF3        
C     BUFFLG = BUFFER FLAG        
C        
C     BUFF   = GINO BUFFER, SET UP BY BTSTRP        
C              USUALLY AT THE END OF OPEN CORE        
C     A      = ARRAY FOR DATA TRANSMISSION        
C     INCT(1)= 0 IN SUCCESSFUL READ, OR        
C              NO. OF WORDS READ IF EOF ENCOUNTERED        
C     INCT(2)= NO. OF UNUSED WORDS IN LAST BUFFER IF EOF READ        
C     RDWRT  = READ/WRITE FLAG - e.g. FILE FOR INPUT OR OUTPUT        
C              (CALLING ROUITNE MAY EQUIVALENCING RDWRT & PRVOPN)       
C     NBUFF2 = END OF BUFFER -2        
C     NBUFF  = LAST WORD IN BLOCK        
C     NBUFF34= BUFFER POINTER AT 3-QUARTER FULL        
C     RECHDR = RECORD HEADER (ORDINARY RECORD)        
C     MTXHDR = RECORD HEADER (STRING DATA - MATRIX COL)        
C     LASTCW = POINTER TO LAST CONTROL WORD IN BLOCK        
C     EQF    = END-OF-FLE        
C     SHEAD  = STRING HEADER        
C     CTRAIL = STRING TRAILER        
C     DUMSTR = DUMMY STRING        
C     LSTSTR = LAST STRING        
C     CHEAD  = CONTROL WORD HEADING        
C     CTRAIL = CONTROL WORD TRAILER        
C     KOC    = DIAG 15 MESSAGE COUNTER        
C     KOCL   = 1,000,000, UPPER LIMIT FOR KOC        
C     BUFCLL = 0, BUFFER CHECKING IF NOT ZERO        
C        
C     CBP    = CURRETN BUFFER POINTER        
C     CLR    = CURRENT LOGICAL RECORD BEGIN POINTER        
C              (IS THE CBP OF PREVIOUS LOGICAL RECORD)        
C     CLR0   = 9, (SEE BUFF(9))        
C     EOD    = END OF DATA POINTER        
C     NLR    = (SEE BUFF(8))        
C     NX     = OLD VALUE OF N        
C        
C     THE WORDS 'BLOCK' AND 'RECORD' ARE SYNONYMOUS        
C        
C     GINO BUFFER BLOCK, SYSBUF WORDS     !  CURRENT  !        
C                                         !<-LOCICAL->!        
C           !<----I/O PORTION             !  RECORD   !  I/O PORTION-->!
C     __________________________________________________________________
C     !1!2!3!4!5...!8!9!                  *----------->            ! ! !
C     ------------------------------------------------------------------
C       /   \ \    / /  !--->DATA RECORDS !            !           / \  
C     CBP  CLR \  / HEADER               CLR          CBP         /NBUFF
C          NBLOCK NLR                                         TRAILER   
C        
C     CALLING SEQUENCES FOR VARIOUS ENTRIES:        
C        
C          1.  CALL OPEN   (*N,NAME,BUFFER,OP)        
C          2.  CALL WRITE  (NAME,ARRAY,N,EOR)        
C          3.  CALL READ   (*N1,*N2,NAME,ARRAY,N,EOR,M)        
C          4.  CALL CLOSE  (NAME,OP)        
C          5.  CALL BCKREC (NAME)        
C          6.  CALL FWDREC (*N1,NAME)        
C          7.  CALL SKPFIL (NAME,N)        
C          8.  CALL REWIND (NAME)        
C          9.  CALL EOF    (NAME)        
C         10.  CALL SAVPOS (NAME,POS)        
C         11.  CALL FILPOS (NAME,POS)        
C         12.  CALL PUTSTR (BLOCK)    BLOCK(1)=NAME FOR ALL STRING CALLS
C         13.  CALL ENDPUT (BLOCK)        
C         14.  CALL GETSTR (*N1,BLOCK)        
C         15.  CALL ENDGET (BLOCK)        
C         16.  CALL GETSTB (*N1,BLOCK)        
C         17.  CALL ENDGTB (BLOCK)        
C         18.  CALL RECTYP (NAME,TYPE)        
C         19.  CALL GETRUN (NAME)     NAME PREVIOUSLY STORED IN /GINOX/ 
C         20.  CALL RDBLK  (*N1,NAME,FIRST,LEFT)        
C         21.  CALL WRTBLK (NAME,EOF)        
C                                   THESE LAST TWO ARE NOT PART OF GINO 
C        NEW.  CALL EDGTSG (*,BLOCK)         = END+GET STRING        
C        NEW.  CALL EDPTSG (BLOCK,BLOCK7)    = END+PUT STRING        
C                                              - - - - -    -        
C     GINO BUFFER:        
C        
C     BUFF(1)   = NUMERIC DATA BLOCK NAME        
C     (IBM,VAX,CDC ONLY)        
C     BUFF(2)   = CBP, CURRENT BUFFER POINTER        
C     BUFF(3)   = CLR, CURRENT LOGICAL RECORD POINTER        
C     BUFF(4)   = EOD, END OF DATA POINTER - THE ADDRESS OF BUFF(NBUFF2)
C     (UNIVAC ONLY) I.E. THE ADDRESS OF LAST WORD IN THE BUFFER MINUS   
C                 TWO. UNIVAC THEREFORE ALSO HAS 3 CONTROL WORDS - CBP, 
C                 CLR,EOD. BUT... UNIVAC ACTUALLY BLOCK OFF 4 WORDS FROM
C                 SYSBUF. I.E. NBUFF3 = SYSBUF-4. UNIVAC GINO I/O BEGINS
C                 ON  BUFF(4), LAST WORD IN BUFF IS NOT WRITTEN OUT     
C        
C            -- BEGINNING OF GINO I/O BLOCK --        
C        
C     BUFF(4)     = NBLOCK - BLOCK NUMBER, STARTING FROM 1        
C     BUFF(5,6,7) = 12 BYTES OF DCB CONTROL INFORMATION PLACED AND      
C     (IBM ONLY)    MAINTAINED BY IO360        
C     BUFF(8)     = NLR, POINTER TO LAST CONTROL WORD IN BLOCK        
C     BUFF(9)     = LOGICAL RECORD HEADER - INDICATES THE TYPE OF RECORD
C                   (REGULAR OR "STRING") AND NO. OF WORDS IN THE RECORD
C     BUFF(10,...)= USER DATA (REGULAR OR "STRING")        
C     BUFF(NBUFF-1)=LOGICAL RECORD TRAILER - CONTAINS THE 'GINO ADDRESS'
C                   (BLOCK NO. & POSITION) OF RECORD, AND CONTINUATION  
C                   INDICATORS        
C     BUFF(NBUFF) = LAST CONTROL WORD, EITHER AN EOF FLAG OR AN INDICA- 
C                   TOR, THAT MORE INFORMATION IS CONTAINED IN NEXT     
C                   BLOCK        
C             ----- END OF GINO I/O BLOCK -----        
C        
C     FORMAT OF LOGICAL RECORD CONTAINING STRING DATA IS AS FOLLOWS:    
C        
C     BUFF(10,...)        
C            COLUMN HEADER        
C              STRING HEADER        
C                     TERM        
C                      :        
C                     TERM        
C              STRING TRAILER        
C              STRING HEADER        
C                     TERM        
C                      :        
C                      :        
C            COLUMN TRAILER        
C        
C     WHERE AS THE COLUMN HEADER AND TRAILER CONTAIN THE TYPE OF ELEMENT
C        IN THE STRING, FORMAT OF STRINGS, AND COLUMN NUMBER.        
C     WHERE AS THE STRING HEADER & TRAILER CONTAIN THE ROW POSITIONS OF 
C        THE FIRST AND LAST TERMS IN THE STRING, AND NO. OF TERMS IN    
C        STRING HEADER AND TRAILER IS ONE WORD EACH IN UNIVAC AND CDC   
C        WHILE ON IBM AND VAX, TWO WORDS EACH        
C     IN ADDITON, 2 CONTROL WORDS MAY APPEAR IN PLACE OF STRING HEADER: 
C       -1 OR DUMSTR = "DUMMY" STRING WORD - TO ENSURE D.P. BOUNDARY    
C                      ALIGNMENT        
C       -2 OR LSTSTR = "LAST"  STRING WORD - NO MORE STRING IN CURRENT  
C                      BLOCK        
C        
C     DEFINITION OF CODES FOR CONTROL WORDS:        
C        
C       FORMAT OF CONTROL WORDS:        
C                      31        28 27       12 11       00   VAX       
C          BITS:       00        03 04       19 20       31   IBM       
C                      35        30 29       15 14       00   UNIVAC    
C                 59   53        36 35       18 17       00   CDC       
C                 ----+------------+-----------+----------+        
C        TYPE 1:    0 !  PRI CODE  !  FIELD 1  !  FIELD 2 !        
C                     !            !           !          !        
C        TYPE 2:    0 !   77 OR F  !  SEC CODE !  FIELD 1 !        
C                 ----+------------+-----------+----------+        
C        
C                     +-----+------------------+----------+        
C        IBUFF: ----> !IBUFF!     IBUFFM       !  IBUFFE  ! <==BUFF(CLR)
C                     +-----+------------------+----------+        
C           BITS :    !<-4->!<-------16------->!<---12--->!        
C        
C        
C                   PRIMARY  SECONDARY   DESCRIPTION        
C                  --------- ---------  ---------------------------     
C        TYPE 1 (REGULAR DATA):        
C                      1        N/A     RECORD TRAILER - RECORD ENTIRELY
C                                       CONTAINED IN BLOCK        
C                      2        N/A     RECORD TRAILER - LAST SEGMENT OF
C                                       A CONTINUED RECORD        
C                      3        N/A     RECORD TRAILER - RECORD CONTINU-
C                                       ED IN NEXT BLOCK        
C                      4        N/A     COLUMN HEADER        
C                      8        N/A     COLUMN TRAILER        
C        TYPE 2 (STRING DATA):        
C                      F       1111     RECHDR - ORDINARY RECORD        
C                      F       2222     MTXHDR - STRING DATA (MATRX COL)
C                      F       5555     LASTCW, END-OF-BLCK, NOMORE DATA
C                      F       6666     (NOT USED)        
C                      F       7777     EQF        
C                      F       8888     STRING HEADER  (IBM,VAX)        
C                      F       9999     STRING TRAILER (IBM,VAX)        
C                      F       AAAA     DUMMY STRING        
C                      F       BBBB     END-OF-BLOCK, NOMORE STRING DATA
C                     31        N/A     STRING HEADER  (UNIVAC)        
C                     37        N/A     STRING TRAILER (UNIVAC)        
C                     77      33333     DUMMY STRING   (UNIVAC)        
C                     77      44444     NO MORE STRINGS IN BLCK (UNIVAC)
C        
C     SEE ALSO PAGES 3.4-19f and 3.4-19g (12/31/77), PROGRAMMER'S MANUAL
C     FOR GINO DEFINITION WORDS        
C        
      ANDF(I,J)   = IAND(I,J)        
      ORF (I,J)   = IOR (I,J)        
C        
CUNIX: REPLACE ABOVE 'IAND' AND 'IOR' BY 'AND' AND 'OR' SYSTEM FUNCTIONS
C        
C        
C     FOLLOWING STATEMENTS INITIALIZE BUFFER CHECKING ROUTINE        
C        
C     EXECUTABLE STATEMENTS        
C     FOLLOWING STATEMENTS INITIALIZE BUFFER CHECKING ROUTINE        
C        
      IF (R12345 .EQ. -1234567890) GO TO 60        
      R12345  =-1234567890        
      DO 50 I = 1,75        
      IOPEN(I)= 0        
   50 R(I)    = 1        
C     SYSBF1  = SYSBUF - 1        
      LOCFX1  = LOCFX(CORE(1))        
C        
C     MOVE OPEN-AND-CLOSE LOG MESSAGES INTO NASTRAN TEXT IF DEBUG FLAG  
C     (DIAG 20) IS ON        
C        
      CALL SSWTCH (20,J)        
      IF (J .EQ. 1) LOGFL = 6        
C        
   60 BUFOFF = IABS(LOCFX1-LOCFX(BUFF(1))) + 1        
      BUFBGN = 1        
      BUFEND = NBUFF3        
      BUFFLG = BUFLGX        
      NAM    = NAME        
      IF (NAME.LT.100 .OR. NAME.GT.999) NAM = 100        
      ASSIGN 3300 TO RETN        
C        
      GO TO (100 ,300 ,400 ,600 ,900 ,1100,1200,1300,1400,1500,1600,    
     1       1700,1800,1900,2100,2200,2400,2500,2600,2700,2800,700),    
     2       ENTRY        
C*****        
C     QOPEN        
C*****        
  100 CONTINUE        
      NBUFF2 = NBUFF3        
      NBUFF  = NBUFF2 + 2        
      NBUFF34= 3*NBUFF/ 4        
      BUFF(1)= NAME        
      IF (OP-1) 110,130,160        
C        
C     FILE OPENED TO READ WITH REWIND.        
C        
C 110 CALL GINOIO (*3300,REW,BUFF(4),110)        
C 120 CALL GINOIO (*3300,RD ,BUFF(4),120)        
  110 OPCODE = REW        
      IMHERE = 110        
      ASSIGN 120 TO GINOIO        
      GO TO 4000        
  120 OPCODE = RD        
      IMHERE = 120        
      ASSIGN 125 TO GINOIO        
      GO TO 4000        
  125 CBP = CLR0        
      CLR = CLR0        
      GO TO 190        
C        
C     FILE OPENED TO WRITE WITH REWIND.        
C        
C 130 CALL GINOIO (*3300,CLS,BUFF(4),130)  !DELETE ANY PRE-EXISTING FILE
C     CALL GINOIO (*3300,REW,BUFF(4),135)        
  130 OPCODE = CLS        
      IMHERE = 130        
      ASSIGN 135 TO GINOIO        
      GO TO 4000        
  135 OPCODE = REW        
      IMHERE = 135        
      ASSIGN 140 TO GINOIO        
      GO TO 4000        
  140 BUFF(4) = 1        
  150 CBP = CLR0        
      CLR = CLR0        
      GO TO 190        
C        
C     FILE OPENED WITHOUT REWIND, RESTORE FILE POSITION.        
C     IF FILE AT LOAD POINT, TREAT AS IF OPENED WITH REWIND.        
C        
  160 IF (NBLOCK.EQ.1 .AND. NLR.EQ.0) IF (OP-2) 140,120,140        
C        
C     FILE NOT AT LOAD POINT.  READ BLOCK, RESTORE CURRENT LOGICAL      
C     RECORD POINTER, AND TEST FOR CORRECT FILE POSITION.        
C        
      BUFF(4) = NBLOCK        
      IF (NLR .NE. 0) GO TO 170        
      IF (OP  .NE. 3) GO TO 167        
      NBLOCK = NBLOCK - 1        
C     CALL GINOIO (*3300,REPOS,BUFF(4),160)        
      OPCODE = REPOS        
      IMHERE = 160        
      ASSIGN 165 TO GINOIO        
      GO TO 4000        
  165 NBLOCK = NBLOCK + 1        
      BUFF(4)= NBLOCK        
      GO TO 150        
  167 IF (PRVOPN .EQ. 0) GO TO 120        
      NBLOCK = NBLOCK - 1        
C        
C     REREAD LAST BLOCK.        
C        
C 170 CALL GINOIO (*3300,BS1B,BUFF(4),170)        
C     CALL GINOIO (*3300,RD  ,BUFF(4),175)        
  170 OPCODE = BS1B        
      IMHERE = 170        
      ASSIGN 175 TO GINOIO        
      GO TO 4000        
  175 OPCODE = RD        
      IMHERE = 175        
      ASSIGN 177 TO GINOIO        
      GO TO 4000        
  177 IF (NLR .EQ. 0) NLR = BUFF(8)        
      CBP = NLR        
      CLR = NLR        
      IF (BUFF(4) .EQ. NBLOCK) GO TO 180        
C     CALL GINOIO (*3300,REPOS,BUFF(4),178)        
      OPCODE = REPOS        
      IMHERE = 180        
      ASSIGN 180 TO GINOIO        
      GO TO 4000        
C 180 IF (OP .EQ. 3) CALL GINOIO (*3300,BS1B,BUFF(4),180)        
  180 IF (OP .NE. 3) GO TO 190        
      OPCODE = BS1B        
      IMHERE = 190        
      ASSIGN 190 TO GINOIO        
      GO TO 4000        
  190 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
C        
C     DIAG 15  (TRACE OPEN CALLS)        
C        
      IF (ENTRY .NE. 1) GO TO 220        
      IF (L15 .EQ. 0) GO TO 220        
      CALL FNAME (NAME,NAMBCD)        
      IF (NAMBCD(1) .NE. 0) GO TO 200        
      NAMBCD(1) = BLANK        
      NAMBCD(2) = BLANK        
  200 KOC=KOC+1        
      WRITE (LOGFL,210) NAMBCD,OPX(OP+3),NAM,FILEX        
  210 FORMAT (14X,'OPEN ',2A4,' FOR  ',A7,' : DMAP UNIT=',I4,        
     1       ', LOGICAL UNIT=',I4)        
      IF (KOC .EQ. KOCL) CALL VAXBRK        
  220 RETURN        
C*****        
C     WRITE        
C*****        
  300 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (ANDF(RDWRT,MASK01) .NE. 1) CALL VAXERR (300)        
      IF (CLR .EQ. CBP)  UNITAB(FILEX) = 4096*BUFF(4) + CLR        
      IF (N) 310,370,320        
  310 CALL VAXEND        
  320 NDXUSR = 0        
  330 IBUFF  = NBUFF2 - CBP        
      IF (IBUFF .EQ. 0) GO TO 360        
      IF (IBUFF .LE. N) GO TO 340        
      IBUFF = N        
  340 N     = N - IBUFF        
      LBP   = CBP        
      CBP   = CBP + IBUFF        
      DO 350 NDX = 1,IBUFF        
      BUFF(LBP+NDX) = A(NDX+NDXUSR)        
  350 CONTINUE        
      IF (N .EQ. 0) GO TO 370        
      NDXUSR = NDXUSR + IBUFF        
C        
C     CALL CLSEG        
C        
  360 BUFF(CLR) = ORF(CBP-CLR,RECHDR)        
      BUFF(CBP + 1) = ORF(UNITAB(FILEX),MASK30)        
      BUFF(CBP + 2) = LASTCW        
      CLR = CBP+ 2        
      ASSIGN 330 TO RET        
      GO TO 2900        
C        
C     END CLSEG        
C        
C     IF EOR .NE. 0 , CLOSE OPERATIONS ON CURRENT LOGICAL RECORD.       
C        
  370 IF (EOR .EQ. 0) GO TO 390        
      IHEADR = RECHDR        
      ASSIGN 380 TO RET1        
      GO TO 3100        
  380 IF (CLR+2 .LT. NBUFF2) GO TO 390        
      BUFF(CLR) = LASTCW        
      ASSIGN 390 TO RET        
      GO TO 2900        
  390 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     READ        
C*****        
  400 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (ANDF(RDWRT,1) .EQ. 1) CALL VAXERR (400)        
      NX = IABS(N)        
C        
C     ASSIGN RETURNS FOR EXTERNAL ENTRIES.        
C        
  410 ASSIGN 570 TO SWITCH        
      ASSIGN 430 TO SWTCH1        
C        
C     IF POSITIONED AT BEGINNING OF RECORD, TEST CODE.        
C        
  420 IBUFF = BUFF(CLR)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 425 TO DCODE        
      GO TO 3500        
  425 IF (CBP    .NE.    CLR) GO TO 470        
      IF (CLR    .GT.BUFF(8)) GO TO 460        
      IF (IBUFF  .NE. MASK1F) GO TO 450        
      IF (IBUFFM .NE. MASK7 ) GO TO 440        
      CLR = CLR + 1        
      CBP = CLR        
C        
C     END-OF-FILE        
C        
      GO TO SWTCH1, (430,1240)        
  430 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN 1        
  440 IF (IBUFFM.EQ.MASK1 .OR. IBUFFM.EQ.MASK2) GO TO 470        
      IF (IBUFFM .EQ. MASK5) GO TO 460        
  450 CALL VAXERR (450)        
C        
C     READ NEXT BLOCK.        
C        
C 460 CALL GINOIO (*3300,RD,BUFF(4),460)        
  460 OPCODE = RD        
      IMHERE = 460        
      ASSIGN 465 TO GINOIO        
      GO TO 4000        
  465 CBP = CLR0        
      CLR = CLR0        
      GO TO 420        
  470 IF (NX .EQ. 0) GO TO 550        
      NDXUSR = 0        
  480 IBUFFE = IBUFFE + CLR - CBP        
      IF (IBUFFE .EQ.  0) GO TO 520        
      IF (IBUFFE .LE. NX) GO TO 490        
      IBUFFE = NX        
  490 NX  = NX  - IBUFFE        
      LBP = CBP        
      CBP = CBP + IBUFFE        
      IF (N .LT. 0) GO TO 510        
      DO 500 NDX = 1,IBUFFE        
      A(NDX+NDXUSR) = BUFF(LBP+NDX)        
  500 CONTINUE        
  510 IF (NX .EQ. 0) GO TO 550        
      NDXUSR = NDXUSR + IBUFFE        
  520 NDX = CLR + 1 + ANDF(BUFF(CLR),MASK3F)        
      IF (ISHFT(BUFF(NDX),-28) .EQ. 3) GO TO 540        
      INCT(1) = IABS(N) - NX        
      ASSIGN 530 TO RET3        
      GO TO 3000        
  530 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN 2        
C 540 CALL GINOIO (*3300,RD,BUFF(4),540)        
  540 OPCODE = RD        
      IMHERE = 540        
      ASSIGN 545 TO GINOIO        
      GO TO 4000        
  545 CBP = CLR0        
      CLR = CLR0        
      IBUFFE = ANDF(BUFF(CLR),MASK3F)        
      GO TO 480        
C        
C     MOVE OR SKIP IS COMPLETE, IF EOR IS NOT ZERO,  ADVANCE TO THE     
C     NEXT RECORD.        
C        
  550 IF (EOR .EQ. 0) GO TO 560        
      ASSIGN 560 TO RET3        
      GO TO 3000        
  560 GO TO SWITCH, (570,420)        
  570 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     CLOSE        
C*****        
  600 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
C        
C     CALL DIAG15        
C        
  700 IF (OP-2) 710,730,760        
C        
C     CLOSE WITH REWIND        
C        
  710 IF (ANDF(RDWRT,1) .EQ. 1) GO TO 770        
C 720 CALL GINOIO (*3300,REW,BUFF(4),720)        
  720 OPCODE = REW        
      IMHERE = 720        
      ASSIGN 725 TO GINOIO        
      GO TO 4000        
  725 NBLOCK = 1        
      LLR = 0        
      GO TO 810        
C        
C     CLOSE WITHOUT REWIND.        
C        
  730 IF (ANDF(RDWRT,1) .EQ. 1) GO TO 770        
  740 IF (CBP .EQ. CLR) GO TO 750        
C        
C     SKIP TO END-OF-RECORD        
C        
      ASSIGN 750 TO RET3        
      GO TO 3000        
  750 NBLOCK = BUFF(4)        
      LLR = CLR        
      GO TO 810        
C        
C     CLOSE WITH END-OF-FILE AND NO REWIND.        
C        
  760 IF (ANDF(RDWRT,1) .EQ. 0) GO TO 740        
  770 IF (CLR .EQ. CBP) GO TO 780        
      IHEADR = RECHDR        
      ASSIGN 780 TO RET1        
      GO TO 3100        
  780 NBLOCK = BUFF(4)        
      LLR = CLR        
      IF (OP .GE. 2) GO TO 790        
      BUFF(CLR) = EQF        
C        
C     WRITE BLOCK        
C        
      ASSIGN 720 TO RET        
      GO TO 2900        
  790 BUFF(CLR) = LASTCW        
      ASSIGN 800 TO RET        
      GO TO 2900        
  800 IF (LLR .LT. NBUFF34) GO TO 810        
      NBLOCK = NBLOCK + 1        
      LLR = 0        
C        
C     SAVE POSITION IN UNITAB, CLOSE FILE, AND RELEASE BUFFER.        
C        
  810 IF (ANDF(RDWRT,1).EQ.0 .OR. ANDF(RDWRT,2).EQ.0 .OR. LLR.EQ.0 .OR. 
     1    OP.EQ.1) GO TO 820        
      NBLOCK = NBLOCK + 1        
      LLR = 0        
  820 UNITAB(FILEX) = 4096*NBLOCK + LLR        
      IF (ANDF(RDWRT,1) .EQ. 1)        
     1    UNITAB(FILEX) = ORF(UNITAB(FILEX),MASK10)        
C        
C     CLOSE FILE        
C?    CALL GINOIO (*3300,WRT,BUFF(4),820)        
C?    CALL GINOIO (*3300,REW,BUFF(4),825)        
C        
      BUFF(1) = MASK77        
      BUFADD(FILEX) = 0        
      IF (NAME .LE. 400) UNITS(NAME-100) = 0        
      LSTNAM = BUFF(1)        
C        
C     DIAG 15  (TRACE CLOSE CALLS)        
C        
      IF (L15 .EQ. 0) GO TO 850        
      CALL FNAME (NAME,NAMBCD)        
      IF (NAMBCD(1) .NE. 0) GO TO 830        
      NAMBCD(1) = BLANK        
      NAMBCD(2) = BLANK        
  830 KOC = KOC + 1        
      WRITE  (LOGFL,840) NAMBCD,OPX(OP+6),NAM,FILEX        
  840 FORMAT (13X,'CLOSE ',2A4,' WITH ',A8,': DMAP UNIT=',I4,        
     1       ', LOGICAL UNIT=',I4)        
      IF (KOC .EQ. KOCL) CALL VAXBRK        
  850 RETURN        
C*****        
C     BCKREC        
C*****        
  900 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      ASSIGN 1070 TO SWITCH        
  910 IF (CBP .NE. CLR ) GO TO 980        
      IF (CLR .EQ. CLR0) GO TO 930        
C        
C     CBP.EQ.CLR AND CLR.NE.CLR0, ANALYZE CONTROL WORD AT CLR - 1       
C        
      CLR = CLR - 1        
      CBP = CLR        
      IBUFF = BUFF(CLR)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 915 TO DCODE        
      GO TO 3500        
  915 IF (IBUFF  .LT.      4) GO TO 1010        
      IF (IBUFF  .NE. MASK1F) GO TO 920        
      IF (IBUFFM .EQ. MASK7 ) GO TO 1060        
  920 CALL VAXERR (920)        
C        
C     CBP.EQ.CLR AND CLR.EQ.CLR0, POSITION TO END OF PREVIOUS BLOCK     
C        
  930 NBLOCK = BUFF(4) - 1        
      IF (NBLOCK .EQ. 0) GO TO 1060        
C     CALL GINOIO (*3300,REPOS,BUFF(4),930)        
      OPCODE = REPOS        
      IMHERE = 930        
      ASSIGN 940 TO GINOIO        
      GO TO 4000        
C 940 IF (ANDF(RDWRT,1) .EQ. 1) CALL GINOIO (*3300,BS1B,BUFF(4),940)    
  940 IF (ANDF(RDWRT,1) .NE. 1) GO TO 945        
      OPCODE = BS1B        
      IMHERE = 940        
      ASSIGN 945 TO GINOIO        
      GO TO 4000        
  945 CLR = BUFF(8)        
      CBP = CLR        
      IBUFF = BUFF(CLR)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 950 TO DCODE        
      GO TO 3500        
  950 IF (IBUFF  .NE. MASK1F) GO TO 960        
      IF (IBUFFM .EQ. MASK7 ) GO TO 1060        
      IF (IBUFFM .EQ. MASK5 ) GO TO 970        
  960 CALL VAXERR (960)        
  970 IF (CLR .LE. CLR0) GO TO 930        
      CLR = CLR - 1        
      CBP = CLR        
      IBUFF = BUFF(CLR)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 975 TO DCODE        
      GO TO 3500        
  975 IF (IBUFF .LT. 4) GO TO 1010        
      GO TO 950        
C        
C     CBP .NE. CLR, ADVANCE TO TRAILING CONTROL WORD.        
C        
  980 IBUFF = BUFF(CLR)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 985 TO DCODE        
      GO TO 3500        
  985 IF (IBUFF .NE. MASK1F) GO TO 990        
      IF (IBUFFM.EQ.MASK1 .OR. IBUFFM.EQ.MASK2) GO TO 1000        
  990 CALL VAXERR (990)        
 1000 CBP = CLR + IBUFFE + 1        
C        
C     DECODE TRAILING WORD        
C        
      IBUFF = BUFF(CBP)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1005 TO DCODE        
      GO TO 3500        
 1005 IF (IBUFF .GE. 4) CALL VAXERR (1010)        
 1010 IF (IBUFFM-BUFF(4)) 1030,1050,1020        
 1020 CALL VAXERR (1020)        
 1030 NBLOCK = IBUFFM        
C        
C     POSITION UNIT        
C        
C     IF (ANDF(RDWRT,1).EQ.1 .AND. NBLOCK.EQ.BUFF(4)-1)        
C    1   CALL GINOIO (*3300,BS1B,BUFF(4),1030)        
C     CALL GINOIO (*3300,REPOS,BUFF(4),1035)        
      IF (ANDF(RDWRT,1).NE.1 .OR. NBLOCK.NE.BUFF(4)-1) GO TO 1035       
      OPCODE = BS1B        
      IMHERE = 1030        
      ASSIGN 1035 TO GINOIO        
      GO TO 4000        
 1035 OPCODE = REPOS        
      IMHERE = 1035        
      ASSIGN 1040 TO GINOIO        
      GO TO 4000        
C1040 IF (ANDF(RDWRT,1) .EQ. 1) CALL GINOIO (*3300,BS1B,BUFF(4),1040)   
 1040 IF (ANDF(RDWRT,1) .NE. 1) GO TO 1050        
      OPCODE = BS1B        
      IMHERE = 1040        
      ASSIGN 1050 TO GINOIO        
      GO TO 4000        
 1050 IF (IBUFFM .NE. BUFF(4)) CALL VAXERR (1050)        
C        
C     SET CBP AND CLR TO RECORD POSITION.        
C        
      CLR = IBUFFE        
      CBP = IBUFFE        
 1060 GO TO SWITCH, (1070,1220)        
 1070 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     FWDREC        
C*****        
 1100 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      NX  = 0        
      N   = NX        
      EOR = 1        
      GO TO 410        
C*****        
C     SKPFIL        
C*****        
 1200 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (N) 1210,1250,1230        
C        
C     BACKWARD SKIP -- DRIVE CODE IN BCKREC.        
C        
 1210 ISKIP = IABS(N)        
      ASSIGN 1220 TO SWITCH        
      GO TO 910        
 1220 IF (BUFF(4).EQ.1 .AND. CLR.EQ.CLR0) GO TO 1250        
      IF (BUFF(CLR) .NE. EQF) GO TO 910        
      ISKIP = ISKIP - 1        
      IF (ISKIP .NE. 0) GO TO 910        
      GO TO 1250        
C        
C     FORWARD SKIP -- DRIVE CODE IN READ.        
C        
 1230 ISKIP = N        
      IF (ANDF(RDWRT,1) .NE. 0) CALL VAXERR (1230)        
      NX = 0        
      N  = NX        
      EOR= 1        
      ASSIGN  420 TO SWITCH        
      ASSIGN 1240 TO SWTCH1        
      GO TO 420        
 1240 ISKIP = ISKIP - 1        
      IF (ISKIP .NE. 0) GO TO 420        
 1250 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     REWIND        
C*****        
 1300 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (BUFF(4) .EQ. 1) GO TO 1310        
      IF (ANDF(RDWRT,1) .EQ. 1) GO TO 130        
      GO TO 110        
 1310 CLR = CLR0        
      CBP = CLR0        
      BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     EOF        
C*****        
 1400 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (ANDF(RDWRT,1) .NE. 1) CALL VAXERR (1400)        
C        
C     IF THE CURRENT LOGICAL RECORD IS NOT CLOSED, CLOSE IT        
C     AND WRITE AN END-OF-FILE.        
C        
      IF (CLR .EQ. CBP) GO TO 1410        
      IHEADR = RECHDR        
      ASSIGN 1410 TO RET1        
      GO TO 3100        
 1410 BUFF(CLR) = EQF        
      IF (CLR .LT. NBUFF2) GO TO 1420        
      ASSIGN 1430 TO RET        
      GO TO 2900        
 1420 CLR = CLR + 1        
      CBP = CLR        
 1430 BUFF(3) = CLR        
      BUFF(2) = CBP        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     SAVPOS        
C*****        
 1500 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      INCT(1) = 4096*BUFF(4) + CLR        
      IF (ANDF(RDWRT,1) .EQ. 1) INCT(1) = UNITAB(FILEX)        
      BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     FILPOS        
C*****        
 1600 CONTINUE        
      CBP  = BUFF(2)        
      CLR  = BUFF(3)        
      JPOS = INCT(1)        
C     CALL DCODE (JPOS,JCLR,NBLOCK)        
      JCLR  = ANDF(JPOS,MASK3F)        
      JPOS  = ISHFT(JPOS,  -12)        
      NBLOCK= ANDF(JPOS,MASK4F)        
      JPOS  = ISHFT(JPOS,  -16)        
      IF (ANDF(RDWRT,1) .EQ. 1) CALL VAXERR (1600)        
C     CALL GINOIO (*3300,REPOS,BUFF(4),1600)        
      OPCODE = REPOS        
      IMHERE = 1600        
      ASSIGN 1640 TO GINOIO        
      GO TO 4000        
 1640 CLR = JCLR        
      CBP = JCLR        
      BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     PUTSTR        
C*****        
 1700 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
C        
C     TEST BEGIN/END FLAG        
C        
      IF (ANDF(BFLAG,MASK2F) .NE. MASK2F) GO TO 1720        
C        
C     FIRST CALL        
C        
      BFLAG = ANDF(BFLAG,MASK6F)        
C        
C     IS THERE ENOUGH SPACE IN THE BUFFER TO START A STRING --        
C        
      IF (CLR+4 .LT. NBUFF2) GO TO 1710        
C        
C  -- NO        
C        
      BUFF(CLR) = LASTCW        
C        
C     WRITE BLOCK        
C        
      ASSIGN 1710 TO RET        
      GO TO 2900        
C        
C  -- YES        
C        
 1710 UNITAB(FILEX) = 4096*BUFF(4) + CLR        
      BFILEX = FILEX        
      BPREC  = 0        
      IF (BTYPE.EQ.2 .OR. BTYPE.EQ.4) BPREC = 1        
      BSFT = 0        
      IF (BTYPE.EQ.2 .OR. BTYPE.EQ.3) BSFT = 1        
      IF (BTYPE.EQ.4) BSFT = 2        
C        
C     FORM COLUMN HEADER        
C        
      CBP  = CBP + 1        
      JREG = ORF(CHEAD,16*BTYPE+BFORM)        
      BUFF(CBP) = ORF(4096*ANDF(BCOL,4095),JREG)        
C        
C     INTERMEDIATE OR LAST CALL        
C        
 1720 IAVIAL = CBP + 4        
      IF (BFORM .NE. 0) IAVIAL = IAVIAL + 2        
C        
C     NUMBER OF TERMS AVAILABLE        
C        
      IAVIAL = NBUFF2 - IAVIAL        
      IF (BSFT   .NE. 0) IAVIAL = IAVIAL/(2*BSFT)        
      IF (IAVIAL .GT. 0) GO TO 1760        
C        
C     END OF BLOCK STRING WORD        
C        
 1730 CBP = CBP + 1        
      BUFF(CBP) = LSTSTR        
      IF (CBP-NBUFF2) 1730,1750,1740        
 1740 CALL VAXEND        
C        
C     CLSEG (MTRXHDR)        
C        
 1750 BUFF(CLR) = ORF(CBP-CLR,MTXHDR)        
C        
C     RECORD TRAILER (RECORD CONTINUED IN NEXT BLOCK)        
C        
      BUFF(CBP + 1) = ORF(UNITAB(FILEX),MASK30)        
C        
C     END OF BLOCK        
C        
      BUFF(CBP + 2) = LASTCW        
      CLR= CBP + 2        
C        
C     WRITE BLOCK        
C        
      ASSIGN 1720 TO RET        
      GO TO 2900        
C        
C     MORE TERMS AVAILABLE IN STRING        
C        
 1760 BNBWRT = 0        
      BNBRAV = IAVIAL        
      IF (BPREC .NE. 0) GO TO 1770        
      BPOINT = LOCFX(BUFF(CBP+3)) - LOCFX1 + 1        
      GO TO 1790        
C        
C     CHECK BOUNDARY ALIGNMENT        
C        
 1770 BPOINT = (LOCFX(BUFF(CBP+3)) - LOCFX1)*(LQRO/1000)        
      IF (MOD(BPOINT,8) .EQ. 0) GO TO 1780        
C        
C     DUMMY STRING DEFINITION WORD        
C        
      CBP = CBP + 1        
      BUFF(CBP) = DUMSTR        
      GO TO 1720        
C        
C     GOOD BOUNDARY ALIGNMENT        
C        
 1780 BPOINT  = BPOINT/8 + 1        
 1790 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     ENDPUT        
C*****        
 1800 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
C        
C     TEST NUMBER OF TERMS WRITTEN IN STRING        
C        
      IF (BNBWRT .EQ. 0) GO TO 1810        
C        
C     DO THE NUMBER OF TERMS WRITTEN EQUAL THE NUMBER AVAILABLE        
C        
      IF (BNBWRT .GT. BNBRAV) CALL VAXERR (1800)        
C                             TOO MANY TERMS        
C        
C     STRING HEADER (NUMBER OF TERMS WRITTEN)        
C        
      BUFF(CBP+1) = ORF(BNBWRT,SHEAD)        
C        
C     STRING HEADER (ROW NUMBER)        
C        
      BUFF(CBP+2) = BROW        
C        
C     ADVANCE BUFFER POINTER FO LAST TERM IN STRING        
C        
      CBP = CBP + 2 + ISHFT(BNBWRT,BSFT)        
C        
C     HAS BUFFER OVERFLOWED        
C        
      IF (CBP .GT. NBUFF2) CALL VAXERR (1802)        
C                          BUFFER OVERFLOWED        
C        
C     TEST STRING FORMAT        
C        
      IF (BFORM .EQ. 0) GO TO 1810        
      BUFF(CBP+1) = ORF(BNBWRT,STRAIL)        
C        
C     STRING TRAILER        
C        
      BUFF(CBP+2) = BROW + BNBWRT - 1        
      CBP = CBP + 2        
C        
C     IS BUFFER FULL        
C        
      IF (CBP .GT. NBUFF2) CALL VAXERR (1806)        
C                          BUFFER OVERFLOWED        
C        
C     DO NUMBER OF TERMS WRITTEN EQUAL NUMBER OF TERMS AVAILABLE        
C        
 1810 IF (BNBWRT .EQ. BNBRAV) GO TO 1820        
      GO TO 1850        
C        
C     STRING TRAILER        
C        
 1820 CBP = CBP + 1        
      BUFF(CBP) = LSTSTR        
C        
C     IS BUFFER FULL        
C        
      IF (CBP-NBUFF2) 1820,1840,1830        
C        
C     BUFFER OVERFLOWED        
C        
 1830 CALL VAXERR (1830)        
C        
C     STRING DATA        
C        
 1840 BUFF(CLR) = ORF((CBP-CLR),MTXHDR)        
C        
C     RECORD TRAILER (RECORD CONTINUED IN NEXT BLOCK)        
C        
      BUFF(CBP+1) = ORF(UNITAB(FILEX),MASK30)        
C        
C     END OF BLOCK        
C        
      BUFF(CBP+2) = LASTCW        
      CLR = CBP + 2        
C        
C     WRITE BLOCK        
C        
      ASSIGN 1850 TO RET        
      GO TO 2900        
C        
C     TEST FOR LAST CALL        
C        
 1850 IF (BFLAG .NE. 1) GO TO 1860        
      CBP = CBP + 1        
      BUFF(CBP) = ORF(CTRAIL,4096*ANDF(BCOL,4095)+16*BTYPE+BFORM)       
      IHEADR = MTXHDR        
      ASSIGN 1860 TO RET1        
      GO TO 3100        
 1860 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
C        
C     BUFFER CHECKING SOUBOUTINE        
C        
      IF (BUFCLL .LE. 0) RETURN        
      BUFEND = CBP        
      CALL BUFCHK        
      RETURN        
C*****        
C     GETSTR        
C*****        
 1900 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (ANDF(BFLAG,MASK2F) .NE. MASK2F) GO TO 1950        
      IF (CBP .GT. BUFF(8)) CALL VAXERR (1900)        
C                           BUFFER OVERFLOWED        
C        
 1910 IBUFF = BUFF(CLR)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1915 TO DCODE        
      GO TO 3500        
 1915 IF (IBUFF .NE.   15 .OR. IBUFFM.EQ.MASK7) GO TO 1920        
      IF (IBUFFM.EQ.MASK2 .OR. IBUFFM.EQ.MASK1) GO TO 1930        
      IF (IBUFFM .NE. MASK5) CALL VAXEND        
C        
C1920 CALL GINOIO (*3300,RD,BUFF(4),1920)        
 1920 OPCODE = RD        
      IMHERE = 1920        
      ASSIGN 1925 TO GINOIO        
      GO TO 4000        
 1925 CBP = CLR0        
      CLR = CLR0        
      GO TO 1910        
C        
C1930 BFLAG = ANDF(BFLAG,MASK6F)        
 1930 CBP = CBP + 1        
      IF (IBUFFE .NE. 0) GO TO 1940        
      IF (CLR+2  .GE. BUFF(8)) GO TO 1920        
      CALL VAXEND        
C        
 1940 IBUFF = BUFF(CBP)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1945 TO DCODE        
      GO TO 3500        
 1945 IF (IBUFF .NE. 4) GO TO 1920        
C                       COLUMN HEADER NOT RECEIVED        
      BCOL  = IBUFFM        
      BTYPE = IBUFFE/16        
      BFORM = ANDF(IBUFFE,15)        
      BFILEX= FILEX        
      BSFT  = 0        
      IF (BTYPE.EQ.2 .OR. BTYPE.EQ.3) BSFT = 1        
      IF (BTYPE.EQ.4) BSFT = 2        
      BPREC = 0        
      IF (BTYPE.EQ.2 .OR. BTYPE.EQ.4) BPREC = 1        
      BFLAG = ANDF(BFLAG,MASK6F)        
C        
 1950 CBP   = CBP + 1        
      IBUFF = BUFF(CBP)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 1955 TO DCODE        
      GO TO 3500        
 1955 IF (IBUFF  .NE. 15   ) GO TO 1960        
      IF (IBUFFM .EQ. MASK8) GO TO 1970        
      IF (IBUFFM .EQ. MASKB) GO TO 1990        
      IF (IBUFFM .EQ. MASKA) GO TO 2000        
      CALL VAXERR (1950)        
C        
 1960 IF (IBUFF.EQ.8) GO TO 2010        
      IF (IBUFF.EQ.3) GO TO 1990        
      CALL VAXERR (1960)        
C        
 1970 BROW   = BUFF(CBP+1)        
      BNBRAV = IBUFFE        
      BNBWRT = 0        
      BPOINT = LOCFX(BUFF(CBP+2)) - LOCFX1        
      IF (BPREC .EQ. 0) GO TO 1980        
      BPOINT = BPOINT/2        
C        
 1980 BPOINT  = BPOINT + 1        
      BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C        
C     READ NEXT BLOCK.        
C        
C1990 CALL GINOIO (*3300,RD,BUFF(4),1990)        
 1990 OPCODE = RD        
      IMHERE = 1990        
      ASSIGN 1995 TO GINOIO        
      GO TO 4000        
 1995 CBP = CLR0        
      CLR = CLR0        
      GO TO 1950        
C        
 2000 CBP = CBP + IBUFFE        
      GO TO 1950        
C        
 2010 ASSIGN 2020 TO RET3        
      GO TO 3000        
C        
 2020 BFLAG   = 1        
      BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN 1        
C*****        
C     ENDGET        
C*****        
 2100 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      CBP = ISHFT(ANDF(BUFF(CBP),4095),BSFT) + CBP + 1        
      IF (BFORM .EQ. 0) GO TO 2110        
      CBP = CBP + 2        
 2110 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     GETSTB        
C*****        
 2200 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (ANDF(BFLAG,MASK2F) .NE. MASK2F) GO TO 2240        
      IF (CLR  .GT. CLR0) GO TO 2230        
      IF (BUFF(4) .NE. 1) GO TO 2210        
      BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN 1        
C        
C     POSITION TO PREVIOUS BLOCK.        
C        
 2210 NBLOCK = BUFF(4) - 1        
C     CALL GINOIO (*3300,REPOS,BUFF(4),2210)        
      OPCODE = REPOS        
      IMHERE = 2210        
      ASSIGN 2220 TO GINOIO        
      GO TO 4000        
 2220 CLR = BUFF(8)        
      IF (BUFF(CLR) .NE. LASTCW) CALL VAXERR (2220)        
C        
 2230 CBP = CLR - 1        
      IF (ISHFT(BUFF(CBP),-28) .GE. 4) CALL VAXERR (2230)        
      CBP = CBP - 1        
      IBUFF = BUFF(CBP)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 2235 TO DCODE        
      GO TO 3500        
 2235 IF (IBUFF .NE. 8) CALL VAXERR (2234)        
C                       COLUMN TRAILER NOT FOUND        
      BTYPE = IBUFFE/16        
      IF (BTYPE .EQ. 0) CALL VAXERR (2236)        
C                       STRINGS DO NOT INCLUDE TRAILERS        
C        
      BFORM = ANDF(IBUFFE,15)        
C     BFLAG = ANDF(BFLAG,MASK6F)        
      BFILEX= FILEX        
      BCOL  = IBUFFM        
      BPREC = 0        
      IF (BTYPE.EQ.2 .OR. BTYPE.EQ.4) BPREC = 1        
      BSFT = 1        
      IF (BTYPE .EQ. 1) BSFT = 0        
      IF (BTYPE .EQ. 4) BSFT = 2        
      BFLAG = ANDF(BFLAG,MASK6F)        
C        
 2240 CBP = CBP - 2        
      IF (CBP .GE. CLR0) GO TO 2270        
C        
C     POSITION TO PREVIOUS BLOCK.        
C        
 2250 NBLOCK = BUFF(4) - 1        
      IF (NBLOCK .EQ. 0) CALL VAXERR (2250)        
C                        FIRST BLOCK        
C     CALL GINOIO (*3300,REPOS,BUFF(4),2250)        
      OPCODE = REPOS        
      IMHERE = 2250        
      ASSIGN 2260 TO GINOIO        
      GO TO 4000        
 2260 CLR = BUFF(8)        
      IF (BUFF(CLR) .NE. LASTCW) CALL VAXERR (2260)        
C                                NOT LAST WORD        
      CBP = CLR - 1        
      GO TO 2240        
C        
 2270 KHR   = -1        
 2280 IBUFF = BUFF(CBP)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 2285 TO DCODE        
      GO TO 3500        
 2285 IF (IBUFF  .NE. 15   ) GO TO 2300        
      IF (IBUFFM .EQ. MASK9) GO TO 2310        
      IF (IBUFFM .NE. MASK2) GO TO 2290        
      IBUFF = BUFF(IBUFFE+CBP+1)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 2287 TO DCODE        
      GO TO 3500        
 2287 IF (IBUFFM .LT. BUFF(4)) GO TO 2250        
      GO TO 2340        
C        
 2290 IF (IBUFFM.NE.MASK8 .AND. IBUFFM.NE.MASKA .AND. IBUFFM.NE.MASKB)  
     1    CALL VAXERR (2290)        
C         BAD CONTROL WORD        
C        
      GO TO 2240        
 2300 IF (IBUFF .EQ. 4) GO TO 2330        
      IF (KHR   .NE.-1) CALL VAXERR (2300)        
C                       BAD CONTROL WORD        
      CBP = CBP + 1        
      GO TO 2280        
C        
 2310 BROW   = BUFF(CBP+1)        
      BNBRAV = IBUFFE        
      BNBWRT = 0        
      BPOINT = LOCFX(BUFF(CBP-ISHFT(1,BSFT))) - LOCFX1        
      IF (BPREC .EQ. 0) GO TO 2320        
      BPOINT = BPOINT/2        
C        
 2320 BPOINT  = BPOINT + 1        
      BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C        
 2330 CBP     = CBP - 1        
 2340 CLR     = CBP        
      BFLAG   = 1        
      BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN 1        
C*****        
C     ENDGTB        
C*****        
 2400 CONTINUE        
      CBP   = BUFF(2)        
      CLR   = BUFF(3)        
      IBUFF = BUFF(CBP)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 2405 TO DCODE        
      GO TO 3500        
 2405 IF (IBUFFM .NE. MASK9) CALL VAXERR (2400)        
C                            TRAILER NOT FOUND        
      BUFF(2) = CBP - 2 - ISHFT(IBUFFE,BSFT)        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     RECTYPE        
C     RETURN INCT(1) = 0, NORMAL GINO LOGICAL RECORD        
C                    = 1, RECORD WRITTEN IN STRING FORMAT        
C*****        
 2500 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (ANDF(RDWRT,1) .NE. 0) CALL VAXERR (2500)        
 2510 IBUFF = BUFF(CLR)        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 2515 TO DCODE        
      GO TO 3500        
 2515 IF (IBUFF .NE. 15) CALL VAXERR (2510)        
      IF (IBUFFM.EQ.MASK1 .OR. IBUFFM.EQ.MASK7) GO TO 2520        
      IF (IBUFFM .EQ. MASK2) GO TO 2530        
      IF (IBUFFM .NE. MASK5) CALL VAXERR (2515)        
C     CALL GINOIO (*3300,RD,BUFF(4),2510)        
      OPCODE = RD        
      IMHERE = 2510        
      ASSIGN 2517 TO GINOIO        
      GO TO 4000        
 2517 CBP = CLR0        
      CLR = CLR0        
      GO TO 2510        
C        
 2520 INCT(1) = 0        
      GO TO 2540        
C        
 2530 INCT(1) = 1        
 2540 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     GETURN        
C*****        
 2600 CONTINUE        
C*****        
C     RDBLK        
C*****        
 2700 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (ANDF(RDWRT,1) .EQ. 1) CALL VAXERR (2700)        
      IF (INCT(1) .NE. 0) GO TO 2710        
C        
C     READ NEXT BLOCK        
C        
C     CALL GINOIO (*3300,RD,BUFF(4),2705)        
      OPCODE = RD        
      IMHERE = 2705        
      ASSIGN 2705 TO GINOIO        
      GO TO 4000        
 2705 CBP = CLR0        
      CLR = CLR0        
C        
C     CHECK IF LAST BLOCK        
C        
 2710 IBUFF = BUFF(BUFF(8))        
C     CALL DCODE (IBUFF,IBUFFE,IBUFFM)        
      ASSIGN 2715 TO DCODE        
      GO TO 3500        
 2715 IF (IBUFF .NE. MASK1F) CALL VAXERR (2710)        
      BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      IF (IBUFFM .NE. MASK7) GO TO 2720        
C        
C     END-OF-FILE, RETURN NUMBER OF UNUSED WORDS IN LAST BUFFER.        
C        
      INCT(2) = NBUFF - BUFF(8)        
      RETURN 1        
 2720 RETURN        
C*****        
C     WRTBLK        
C*****        
 2800 CONTINUE        
      CBP = BUFF(2)        
      CLR = BUFF(3)        
      IF (ANDF(RDWRT,1) .EQ. 0) CALL VAXERR (2800)        
      CLR = BUFF(8)        
      CBP = CLR        
      IF (INCT(1) .NE. 0) GO TO 2810        
C        
C     WRITE BLOCK.        
C        
      BUFF(8) = CLR        
C     CALL GINOIO (*3300,WRT,BUFF(4),2805)        
      OPCODE = WRT        
      IMHERE = 2805        
      ASSIGN 2805 TO GINOIO        
      GO TO 4000        
 2805 BUFF(4) = BUFF(4) + 1        
      CBP = CLR0        
      CLR = CLR0        
C        
 2810 BUFF(2) = CBP        
      BUFF(3) = CLR        
      LSTNAM  = NAME        
      RETURN        
C*****        
C     INTERNAL ROUTINES        
C*****        
C        
C     INTERNAL ROUTINE TO WRITE A BLOCK.        
C        
 2900 BUFF(8) = CLR        
C     CALL GINOIO (*3300,WRT,BUFF(4),2900)        
      OPCODE = WRT        
      IMHERE = 2900        
      ASSIGN 2905 TO GINOIO        
      GO TO 4000        
 2905 BUFF(4) = BUFF(4) + 1        
      CBP = CLR0        
      CLR = CLR0        
      GO TO RET, (330,390,2810,720,800,1430,1710,1720,1850)        
C        
C        
C     INTERNAL ROUTINE TO SKIP TO THE END OF A LOGICAL RECORD.        
C        
 3000 CONTINUE        
      CBP = ANDF(BUFF(CLR),4095) + CLR + 1        
      IF (ISHFT(BUFF(CBP),-28) .EQ. 3) GO TO 3020        
      CLR  = CBP + 1        
      IEND = ISHFT(ISHFT(BUFF(CLR),4),-16)        
      IF (IEND .NE. MASK5) GO TO 3030        
C     CALL GINOIO (*3010,RD,BUFF(4),3010)        
      OPCODE = RD        
      IMHERE = 3010        
      ASSIGN 3010 TO GINOIO        
      GO TO 4000        
 3010 CBP = CLR0        
      CLR = CLR0        
      GO TO 3040        
C3020 CALL GINOIO (*3050,RD,BUFF(4),3020)        
 3020 OPCODE = RD        
      IMHERE = 3020        
      ASSIGN 3025 TO GINOIO        
      GO TO 4000        
 3025 CBP = CLR0        
      CLR = CLR0        
      GO TO 3000        
 3030 CLR = CBP + 1        
      CBP = CLR        
 3040 GO TO RET3, (530,560,750,2020)        
C
C3050 CBP = CLR0        
C     CLR = CLR0        
C     RETURN 1        
C        
C     INTERNAL ROUTINE TO WRITE CONTROL WORD IN BUFFER AT THE END       
C     OF A LOGICAL RECORD.        
C        
 3100 BUFF(CLR) = ORF(CBP-CLR,IHEADR)        
      MASK = MASK10        
      IF (ISHFT(UNITAB(FILEX),-12) .NE. BUFF(4)) MASK = MASK20        
      BUFF(CBP+1) = ORF(UNITAB(FILEX),MASK)        
      CLR = CBP + 2        
      CBP = CLR        
      GO TO RET1, (380,780,1410,1860)        
C        
C     ERROR MESSAGES.  FORCE A SYSTEM TRACEBACK IF MACHINE IN NOT UNIX. 
C        
C        
 3300 WRITE  (NOUT,3310)        
 3310 FORMAT (/,' * GINO FATAL ERROR *')        
C        
C     SET MACHINE TO 6 HERE IF MACHINE IS UNIX        
C     MAC(1) = 6        
C        
      IF (MAC(1) .NE. 5) STOP        
      X = 1.1*ENTRY        
      Y = SIN(X)        
      X = SQRT(Y)        
      I = -33        
      WRITE (I) X        
      RETURN        
C        
C        
C        
C     INTERNAL SUBROUTINE DCODE (IBUFF,IBUFFE,IBUFFM)        
C        
 3500 IBUFFE = ANDF(IBUFF,MASK3F)        
      IBUFF  = ISHFT(IBUFF,  -12)        
      IBUFFM = ANDF(IBUFF,MASK4F)        
      IBUFF  = ISHFT(IBUFF,  -16)        
      GO TO DCODE, (425, 915, 950, 975, 985,1005,1915,1945,1955,2235,   
     1             2285,2287,2405,2515,2715)        
C        
C        
C        
C     INTERNAL SUBROUTINE GINOIO (*,OPCODE,B,IMHERE)        
C        
C     ARRAY B HERE IS BUFF(4 THRU SYSBUF-1) IN GINO.        
C        
C     OPERATIONS ARE AS FOLLOWS (OPCODE):        
C        
C         1 - REWIND FILE        
C         2 - WRITE ONE BLOCK        
C         3 - READ ONE BLOCK        
C         4 - BACKSPACE ONE BLOCK        
C         5 - FORWARD ONE BLOCK        
C         6 - REPOSITION TO REQUESTED BLOCK        
C         7 - CLOSE AND DELETE FILE        
C         8 - RE-READ ONE BLOCK              =4+3        
C         9 - CLOSE (DELETE) AND REWIND FILE =7+1        
C        10 - REWIND AND READ FIRST BLOCK    =1+3        
C        
C        
 4000 IF (F .LE. 1) GO TO 4200        
      IF (IOPEN(F) .EQ. 1) GO TO 4005        
      LREC = NBUFF3*(MOD(LQRO,100)/10)        
C                    MOD(LQRO,100)/10 IS BYTE OR WORD COUNT        
C        
C     SEE VAX FORTRAN PERFORMANCE GUIDE, MAY 1990, SECTION 6.1.1 FOR    
C     SPECIFYING BLOCKSIZE & BUFFERCOUNT        
C     (HOWEVER, THE INCLUSION OF BLOCKSIZE SEEMS TO SLOW DOWN, AND THE  
C     BUFFERCOUNT SEEMS TO MAKE NO DIFFERENT IN SPEED IMPROVEMENT)      
C        
      OPEN (UNIT=F,ACCESS='DIRECT',FORM='UNFORMATTED',STATUS='UNKNOWN', 
     1      RECL=LREC, FILE=DSNAMES(F))        
C    2               ,BLOCKSIZE=LREC,BUFFERCOUNT=2)        
C    2               ,RCDS     =LREC)        
      IOPEN(F) = 1        
 4005 IF (F.EQ.0 .AND. OPCODE.EQ.0 .AND. R(F).EQ.0) CALL VAXBRK        
C        
C     BRANCH TO REQUESTED OPERATION.        
C        
      GO TO (4010,4020,4030,4040,4050,4060,4070,4080,4090,4100), OPCODE 
C        
C     REWIND FILE:        
C     ===========        
C        
 4010 R(F) = 1        
      GO TO 4035        
C        
C     WRITE ONE BLOCK:        
C     ===============        
C        
 4020 RF = R(F)        
      CALL QWRITE (F,RF,BUFF(4),NBUFF3)        
C     WRITE (F,REC=RF) (BUFF(J),J=4,SYSBF1)        
      R(F) = R(F) + 1        
      GO TO 4035        
C        
C     READ ONE BLOCK:        
C     ==============        
C        
 4030 RF = R(F)        
      CALL QREAD (*110,F,RF,BUFF(4),NBUFF3)        
C     READ (F,REC=RF,ERR=110) (BUFF(J),J=4,SYSBF1)        
      R(F) = R(F) + 1        
 4035 IF (KLOSE .NE. 1) GO TO 4200        
      IOPEN(F) = 0        
      CLOSE (UNIT=F,STATUS='KEEP')        
      GO TO 4200        
C        
C     BACKSPACE ONE BLOCK:        
C     ===================        
C        
 4040 IF (R(F) .GT. 1) R(F) = R(F) - 1        
      GO TO 4200        
C        
C     FORWARD ONE BLOCK:        
C     =================        
C        
 4050 R(F) = R(F) + 1        
      GO TO 4200        
C        
C     REPOSITION TO REQUESTED BLOCK:        
C     =============================        
C        
 4060 IF (NBLOCK .EQ. R(F)-1) GO TO 4200        
      CALL QREAD (*110,F,NBLOCK,BUFF(4),NBUFF3)        
C     READ (F,REC=NBLOCK) (BUFF(J),J=4,SYSBF1)        
      R(F) = NBLOCK + 1        
      GO TO 4200        
C        
C     CLOSE AND DELETE FILE:        
C     =====================        
C        
 4070 CLOSE (UNIT=F,STATUS='DELETE')        
      IOPEN(F) = 0        
      GO TO 4200        
C        
C     RE-READ CURRENT BLOCK: (4 + 3)        
C     =====================        
C        
 4080 IF (R(F) .GT. 1) R(F) = R(F) - 1        
      GO TO 4030        
C        
C     CLOSE (& DELETE) FILE AND REWIND: (7 + 1)        
C     ================================        
C        
 4090 CLOSE (UNIT=F,STATUS='DELETE')        
      IOPEN(F) = 0        
      GO TO 4010        
C        
C     REWIND AND READ FIRST BLOCK: (1 + 3)        
C     ===========================        
C        
 4100 CONTINUE        
      CALL QREAD (*4110,F,1,BUFF(4),NBUFF3)        
C     READ (F,REC=1,ERR=4110) (BUFF(J),J=4,SYSBF1)        
      R(F) = 2        
      GO TO 4035        
C        
 4110 WRITE  (NOUT,4120) IMHERE        
 4120 FORMAT (' *** GINO ERROR.  IMHERE=',I5)        
C     RETURN 1        
      GO TO RETN, (3300)        
C        
 4200 GO TO GINOIO, (120, 125, 135, 140, 165, 175, 177, 180, 190, 465,  
     1               545, 725, 940, 945,1035,1040,1050,1640,1925,1995,  
     2              2220,2260,2517,2705,2805,2905,3010,3025)        
      END        
