      SUBROUTINE BTSTRP        
C        
C     IF MACH IS ZERO, THIS SUBROUTINE WILL DETERMINE THE HOST MACHINE, 
C     IBM, VAX, CDC, UNIVAC OR DEC/ULTRIX, CURRENTLY COSMIC/NASTRAN     
C     SUPPORTS.        
C        
C     HOWEVER, IF MACH IS PRESET TO NON-ZERO, THE HOST MACHINE WILL BE  
C     SET TO THAT MACHINE TYPE WITHOUT FURTHER EVALUATION.        
C        
C     AFTER HOST MACHINE IS ESTABLISHED, BTSTRP WILL DEFINE ALL THE     
C     MACHINE-DEPENDENT CONSTANTS NEEDED IN NASTRAN. THESE CONSTANTS    
C     ARE SAVED IN LABEL COMMONS /SYSTEM/, /LHPWX/ AND /MACHIN/        
C        
C     SEE ALSO  PRTPRM, SDCMPS, SDR2E, AND UPCASE WHEN NASTRAN SOURCE   
C     CODE IS PORTED TO OTHER (NEW) MACHINE        
C        
C     LAST REVISED BY C.CHAN/UNISYS  6/92        
C     TO INCLUDE ALL MACHINE CONSTANTS NEEDED IN NASTRAN        
C                ===        
C        
      EXTERNAL        LSHIFT ,RSHIFT ,ANDF   ,COMPLF        
      INTEGER         SYSBUF ,OUTTAP ,TWO    ,COMPLF    ,RSHIFT     ,   
     1                UNITAB ,BUFFAD ,FCB    ,HICORE    ,ORDER      ,   
     2                ABCD   ,AK     ,RECL   ,ANDF      ,MACHNM(22) ,   
     3                SPERLK ,QP     ,M1(132),M2(110)   ,MCONST(242),   
     4                HIGHPW ,VCTR   ,VCTRZ        
      REAL            XX     ,YY        
      COMMON /MACHIN/ MACHX  ,IHALF  ,JHALF  ,LQRO      ,MCHNAM     ,   
     1                VCTRZ        
      COMMON /SEM   / A      ,MASK2  ,MASK3  ,LNKNOS(15)        
      COMMON /LHPWX / LOWPW  ,HIGHPW ,NWPIC  ,NUDFLW    ,MXFL       ,   
     1                KSHIFT ,MTISA        
      COMMON /SYSTEM/ B(100)        
      COMMON /TWO   / TWO(32),MZERO        
      COMMON /XXREAD/ DUM(3) ,IBMCDC        
      COMMON /GINOX / LGINOX ,FILEX  ,EOR    ,OP        ,ENTRY      ,   
     1                LSTNAM ,NXX    ,XYZ(4) ,UNITAB(75),BUFFAD(75) ,   
     2                X(8)   ,FCB(1)        
      EQUIVALENCE     (B( 1),SYSBUF) ,(B(22),LINKNO) ,(B(40),NBPW ) ,   
     1                (B( 2),OUTTAP) ,(B(31),HICORE) ,(B(41),NCPW ) ,   
     2                (B( 4),INTP  ) ,(B(34),IDRUM ) ,(B(55),IPREC) ,   
     3                (B( 9),NLPP  ) ,(B(39),NBPC  ) ,(B(91),LPCH ) ,   
     4                (B(92),LDICT ) ,(B(95),SPERLK) ,(MACHX,MACH ) ,   
     5                (M1(1),MCONST(1))     ,(M2(1)  ,MCONST(133) )     
      DATA    XX    , YY    / 1.2E-38, 0.3E-38       /        
      DATA    MVAX  , ABCD  , KA   /  1H1,   4HABCD  ,4HA           /   
C        
C     MACH   = MACHX = HOST MACHINE        
C              ANY SUBROUTINE, THAT USES 'MACHX' INSTEAD OF 'MACH' IN   
C              LABEL COMMON /MACHIN/, CONTAINES MACHINE CONSTANTS THAT  
C              ARE USED LOCALLY.        
C     NMACH  = NUMBER OF MACHINES        
C     MCONST = ARRAY CONTAINING MACHINE DEPENDENT CONSTANTS        
C        
      DATA    NMACH / 22 /,    M1/        
C        
C     COSMIC/NASTRAN SUPPORTS ONLY MACHINES 1 THRU 6        
C     CONSTANTS BELOW, FOR MACHINES 6 AND HIGHER, MAY NOT BE EXACT      
C     DEC/ULTRIX USING VMS FORTRAN IS MACHINE 5        
C     DEC/ULTRIX WITH RISC IS MAHCINE 6        
C        
C     -MACHINE-    IBM/  UNIVAC   CDC   VAX/    DEC/         IBM/       
C           DUMMY   MVS    FTN   FTN5    VMS  ULTRIX   SUN    AIX    HP 
C     MACH = -1-  ---2-  ---3-  ---4-  ---5-  ---6-  ---7-  ---8-  ---9-
C        
C          SILIC.                                     DATA        
C          GRAPH   MAC   CRAY  CONVEX   NEC  FUJITSU  GENL AMDAHL  PRIME
C          --10-  --11-  --12-  --13-  --14-  --15-  --16-  --17-  --18-
C        
C                         DEC/    RE-        
C          486PC  DUMMY  ALPHA  SERVED        
C          --19-  --20-  --21-  --22-        
C        
C     SYSBUF  =   LENGTH OF NASTRAN I/O BUFFER        
C        
CIBMR1      200,  1604,   871,  1042,  1028,  1028,  1028,  1028,  1028,
     1      200,  4095,   871,  1042,  1028,  1028,  1028,  1028,  1028,
     2     1028,  1028,  2052,  1028,  2052,  2052,  1028,  1028,  1028,
     3     1028,  1028,  1028,  1028,        
C        
C     INTP(X100)  =  FORTRAN UNIT NO. FOR INPUT DATA        
C     OUTTAP      =  FORTRAN UNIT NO. FOR PRINTED OUTPUT        
C        
     4     5 06,  5 06,  5 06,  5 06,  5 06,  5 06,  5 06,  5 06,  5 06,
     5     5 06,  5 06,  5 06,  5 06,  5 06,  5 06,  5 06,  5 06,  5 06,
     6     5 06,  5 06,  5 06,  5 06,        
C        
C        
C     NLPP(X100)  =  NUMBER OF LINES PRINTED PER PAGE        
C     NWPIC       =  NUMBER OF WORDS PER INPUT CARD, USED ONLY IN XGPIBS
C        
     7    50 00, 55 18, 55 18, 42 08, 55 18, 55 18, 55 18, 55 18, 55 18,
     8    55 18, 55 18, 55 09, 55 18, 55 00, 55 00, 55 00, 55 00, 55 00,
     9    55 00, 55 00, 55 18, 55  0,        
C        
C     NBPC(X100)  =  NUMBER OF BITS PER CHARACTER        
C     NBPW        =  NUMBER OF BITS PER WORD        
C        
     O     6 36,  8 32,  9 36,  6 60,  8 32,  8 32,  8 32,  8 32,  8 32,
     1     8 32,  8 32,  8 64,  8 32,  8 64,  8 64,  8 32,  8 32,  8 32,
     2     8 32,  8 32,  8 32,  8 32,        
C        
C     IPREC(X100) =  PRECISION (1 = S.P., 2 = D.P.)        
C     RECL(X10)   =  DIRECT FILE RECORD LENGTH (USED IN FORTRAN OPEN    
C                    STATEMENT) BY WORDS (= 1), OR BYTE (= NCPW)        
C     QP          =  REAL*16 PRECISION FLAG (1 = YES, 0 = NO)        
C        
     3    2 0 0, 2 4 0, 2 1 1, 1 1 0, 2 1 1, 2 4 0, 2 4 0, 2 4 1, 2 4 1,
     4    2 1 1, 2 0 0, 1 8 0, 2 4 1, 1 0 0, 1 0 0, 2 0 0, 2 0 0, 2 0 0,
     5    2 0 0, 2 0 0, 2 1 0, 2 0 0,        
C        
C     HICORE  =   NUMBER RELATED TO CORE        
C        
     6       -1, 50000, 85000,    -1, 50000, 50000, 99999, 50000, 50000,
     7    50000, 50000,500000, 80000, 99999, 99999, 50000, 50000, 50000,
     8    50000, 50000, 50000, 50000/        
C        
      DATA          M2/        
C        
C     LPCH(X100)  =  FORTRAN UNIT NO. FOR PUNCHED OUTPUT        
C     LDICT       =  FORTRAN UNIT NO. FOR RESTART DICTIONARY PUNCH      
C        
     1     7 03,  7 07,  1 03,  7 07, 77 76, 77 76, 77 76, 77 76, 77 76,
     2    77 76, 77 76,  1 03, 77 76,  1 03,  1 03, 77 76, 77 76, 77 76,
     3    77 76, 77 76, 77,76  77 76,        
C        
C     LOWPW, HIGHPW = MACHINE NUMERIC RANGE FOR S. P. REAL NUMBER,      
C     USED ONLY BY RCARD, RCARD2, XRCARD AND YRCARD        
C        
     4       38,    75,    38,   321,    38,    38,    38,    38,    38,
     5       38,    38,  2465,    38,     0,     0,     0,     0,     0,
     6        0,     0,    38,     0,        
C        
C     NUDFLW(X100) =  FLOATING NUMBER UNDERFLOW CONTROL        
C                     (USED ONLY BY FQRW AND FQRWV)        
C     MXFL         =  MAXINUM FILES MAXFIL CAN REQUEST VIA THE NASTRAN  
C                     CARD, USED ONLY IN NASCAR        
C        
     7    16 50, 16 50, 18 49, 14 75,  8 75, 16 75, 16 75, 16 75, 16 75,
     8    16 75, 16 75, 16 75, 16 75, 16 75, 16 75, 16 75, 16 75, 16 75,
     9    16 75, 16 75,  9 75, 16 75,        
C        
C     KSHIFT  =  SHIFT COUNTS USED IN A DIVIDE TO CONVERT A GINO LOC    
C                RETURNED FROM SAVPOS TO GINO BLOCK NUMBER, USED IN EMA 
C        
     O        1,  4096,   4096,262144,  4096,  4096, 4096,  4096,  4096,
     1     4096,  4096,   4096,  4096,     0,     0,    0,     0,     0,
     2        0,     0,   4096,     0,        
C        
C     MANTISSA BITS, USED ONLY IN SDCMPS        
C        
     3     0 00, 24 26, 27 60, 48 96, 23 55, 23 55, 23 52, 23 55, 23 55,
     4    23 55, 23 55, 48 96, 23 52, 48 96, 48 96, 23 55, 23 55, 23 55,
     5     0 00,  0 00, 23 55,  0 00/        
C        
C     VECTORIZATION VERSION        
C     VCTR = 0, NO VECTORIZATION, OR VECTORIZATION DONE BY COMPILER ONLY
C               AND THERE IS NO SOURCE CODE CHANGE FOR VECTORIZATION.   
C          = 1, VECTORIZATION BY LOCAL SOURCE CODE CHANGES, SUCH AS THE 
C               NASTRAN IBM VECTOR VERSION.        
C        
      DATA   VCTR    / 0 /        
C        
C     MACHINE NAME PLATE        
C        
      DATA   MACHNM  /        
     1       4H XXX, 4H IBM, 4HUNVC, 4H CDC, 4H VAX, 4HULTX, 4H SUN,    
     2       4H AIX, 4H HP , 4H SGI, 4H MAC, 4HCRAY, 4HCNVX, 4H NEC,    
     3       4HFUJI, 4HDGEN, 4HAMDL, 4HPRIM, 4H 486, 4H YYY, 4HALFA,    
     4       4H ZZZ  /        
C        
C        
C     THIS ROUTINE IS CALLED ONLY ONCE BY XSEM01 IF WE ARE IN SUPERLINK 
C        
      IF (SPERLK .GT. 1) RETURN        
C        
C     MACHINE TYPE CAN BE PRESET HERE        
C     +++++++++++++++++++++++++++++++        
      MACH = 0        
C        
      IF (MACH .GT. 0) GO TO 100        
      IF (COMPLF(-1) .LE. 2) IF (RSHIFT(COMPLF(0),32)-15) 3678,1108,6600
C        
C     7094        
C        
      MACH = 1        
      GO TO 100        
C        
C     DISTINGUISH BETWEEN 360 AND VAX (OR UNIX)        
C        
 3678 IF (RSHIFT(MVAX,24)-241) 780,360,780        
  360 MACH = 2        
      GO TO 100        
C        
C     DISTINGUISH BETWEEN VAX/VMS AND ULTRIX(RISC)        
C        
  780 MACH = 5        
      XX   = XX/(MACH+1)        
      IF (    XX*2. .GT.  YY) MACH = 6        
C         .2E-38*2.  = .4E-38 IN ULTRIX(RISC)        
C            0.0*2.  = 0.0    IN ULTRIX/VMS        
C     ABOVE METHOD DOES NOT WORK FOR THE NEW 92 ULTRIX(RISC) COMPILER   
C        
      GO TO 100        
C        
C     UNIVAC        
C        
 1108 MACH = 3        
      GO TO 100        
C        
C     DISTINGUISH BETWEEN 60-BIT CDC (TYEP 4) AND 64-BIT (TYPE 12)      
C     MACHINES        
C        
 6600 MACH = 4        
      IF (RSHIFT(RSHIFT(COMPLF(0),30),30) .GE. 8) MACH = 12        
C        
C     DEFINE COMMON /SYSTEM/ MACHINE-DEPENDENT CONSTANTS        
C        
  100 SYSBUF = MCONST(MACH)        
      IBMCDC = 1        
      IF (MACH.EQ.2 .OR. MACH.EQ.4) IBMCDC = 0        
C        
      I  = MACH + NMACH        
      INTP   = MCONST(I)/100        
      OUTTAP = MOD(MCONST(I),100)        
C        
      I  = I + NMACH        
      NLPP   = MCONST(I)/100        
      NWPIC  = MOD(MCONST(I),100)        
C        
      I  = I + NMACH        
      NBPC   = MCONST(I)/100        
      NBPW   = MOD(MCONST(I),100)        
C        
      I  = I + NMACH        
      IPREC  = MCONST(I)/100        
      RECL   = MOD(MCONST(I),100)/10        
      QP     = MOD(MCONST(I),10)        
C        
      I  = I + NMACH        
      HICORE = MCONST(I)        
C        
      I  = I + NMACH        
      LPCH   = MCONST(I)/100        
      LDICT  = MOD(MCONST(I),100)        
C        
C     MACHINE S.P. RANGE        
C        
      I  = I + NMACH        
      HIGHPW = MCONST(I)        
      LOWPW  = 1 - HIGHPW
      IF (MACH .EQ. 2) LOWPW = -78        
      IF (MACH .EQ. 4) LOWPW = -292        
C        
C     FLOATING NUMBER UNDERFLOW CONTROL        
C     MAXINUM FILES FOR MAXFIL CHECK        
C        
      I  = I + NMACH        
      NUDFLW = MCONST(I)/100        
      MXFL   = MOD(MCONST(I),100)        
C        
C     SHIFT COUNTER FOR EMA SUBROUTINE        
C        
      I  = I + NMACH        
      KSHIFT = MCONST(I)        
C        
C     MANTISSA BITS        
C        
      I  = I + NMACH        
      MTISA  = MCONST(I)/100        
      IF (IPREC .EQ. 2) MTISA = MOD(MCONST(I),100)        
C        
C     VECTORIZATION FLAG FOR A PARTICULAR MACHINE = VCTR X MACH        
C        
      VCTRZ = VCTR*MACH        
C        
C     ADD MACHINE NAME PLATE (A4) TO MACHNM, 5TH WORD OF /MACHIN/       
C        
      MCHNAM = MACHNM(MACH)        
C        
C     NUMBER OF BITS PER HALF WORD, USED MAINLY FOR INTEGER PACKING     
C        
C     IHALF = NBPW/2        
C     JHALF = 2**IHALF - 1        
      IHALF = 16        
      JHALF = 65535        
C        
C     NUMBER OF CHARACTERS PER WORD        
C        
      NCPW = NBPW/NBPC        
C        
C     ZERO FIELD KA, AK AND GENERATE A MASK FOR FIRST BYTE        
C        
      AK   = KHRFN1(0,1,KA,4)        
      KA   = KHRFN1(0,1,KA,1)        
      I    = 2**NBPC - 1        
      MASK = LSHIFT(I,NBPW-NBPC)        
C        
C     CHECK BCD WORD (NOT CHARACTER WORD) STORING ORDER.        
C     IF 'ABCD' IS STORED INTERNALLY IN A-B-C-D ORDER, SET ORDER TO 0,  
C     OR  IF IT IS STORED IN REVERSED ORDER, D-C-B-A,  SET ORDER TO 1   
C        
      I = ANDF(ABCD,MASK)        
      ORDER = 0        
      IF (NBPW.LT.60 .AND. I.NE.KA .AND. I.NE.AK) ORDER = 1        
C        
C     CHECK SYSTEM LOC OR %LOC FUNCTION.        
C     IF SYSTEM LOC FUNCTION IS WORD COUNT, SET LOCF TO 1        
C     IF SYSTEM LOC FUNCTION IS BYTE COUNT, SET LOCF TO NCPW        
C        
      LQRO = 1000        
      I    = LOCFX(B(11)) - LOCFX(B(1))        
      LOCF = I/10        
C        
C     MERGE LOCF, QP, RECL, AND ORDER INTO LQRO        
C        
      LQRO = LOCF*1000 + QP*100 + RECL*10 + ORDER        
C        
C        
C        
C     DEBUG PRINT OUT        
C        
C     WRITE (6,120) SYSBUF,INTP,OUTTAP,NLPP,NWPIC,NBPC,NBPW,NCPW,IPREC, 
C    1              RECL,QP,LOCF,ORDER,LQRO,HICORE,LPCH,LDICT,NUDFLW,   
C    2              MXFL,KSHIFT,MTISA,MACH,MCHNAM        
C 120 FORMAT (//2X,'BTSTRP - SYSBUF,INTP,OUTTAP,NLPP,NWPIC,NBPC,NBPW',  
C    1             ',NCPW,IPREC,RECL,QP =', /9X,11I6,        
C    2        /11X,'LOCF,ORDER,LQRO,HICORE,LPCH,LDICT,NUDFLW,MXFL',     
C    3             ',KSHIFT,MTISA =', /9X,10I6,        
C    4        /11X,'MACHINE,MACH-NAME =',I4,2X,A4)        
C        
C     GENERATE MASKS        
C                   7094      360         1108            6600        
C     MASK2  = 777777007777,FFFFFFF0,777777607777,77777760777777777777  
C     MASK3  = 377777777777,7FFFFFFF,377777777777,37777777777777777777  
C     TWO(1) = 020000000000,80000000,020000000000,00000000020000000000  
C        
      MASK2  = COMPLF(LSHIFT(2**NBPC-1, NBPW-4*NBPC))        
      MASK3  = RSHIFT(COMPLF(0),1)        
      MZERO  = LSHIFT(1,NBPW-1)        
      TWO(1) = LSHIFT(1,31)        
C        
C     TWO(1) = LSHIFT(1,31) = 2**31        
C            = +2147483648   IN MACHINES WITH MORE THAN 32-BIT WORD     
C            = -2147483648   IN 32-BIT MACHINES. A NEGATIVE NUMBER!     
C            = -0.000E0      IN SOME  32-BIT MACHINES        
C            = +0.000E0      IN OTHER 32-BIT MACHINES        
C     NOTICE FOR THE 32-BIT MACHINES, IABS(-2147483648) IS FATAL!       
C        
C        
C     INITIALIZE COMMON /ARGOFF/ USED FOR RESOLVING THE 'FORGOTTEN      
C     ARGUMENT' PROBLEM IN MULTIPLE-ENTRY SUBROUTINES        
C        
C     IOFF  = LOCFX(JOFF(1)) - 1        
C        
C     DEFINE COMMONLY USED PHYSICAL CONSTANTS        
C        
      CALL CNSTDD        
C        
C     SET LINK 1 AS THE CURRENT LINK        
C        
      LINKNO = LNKNOS(1)        
C        
      IF (MACH .LT. 3) RETURN        
C        
C     INITIALIZE /GINOX/ FOR NON-IBM MACHINES        
C        
      LGINOX = 245        
      IVAL   = 4096        
      DO 200 I = 1,75        
      FCB(I) = 1        
      UNITAB(I) = IVAL        
  200 BUFFAD(I) = 0        
      IF (MACH .EQ. 4) RETURN        
C        
C     INITIALIZE DRUM POSITION ALLOCATION        
C        
      IDRUM = 1        
      RETURN        
      END        
