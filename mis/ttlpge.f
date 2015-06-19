      SUBROUTINE TTLPGE (TOPT)        
C        
C     THIS TITLE PAGE ROUTINE WAS RE-WRITTEN BY G.CHAN/UNISYS TO SHOW   
C     HIGHER STANDARD OF PROFESSIONALISM.        
C        
      INTEGER         NCHAR(20),IDATE(3),CARD(20),TOPT,TIMEW,HH,SS,     
     1                VN,FTN,FTN5,FOR,FORT,UNIX        
      CHARACTER       COMPUT(20)*7,C7*7,MCHTTL*24,C1(29)*1,XTRA*16      
      COMMON /MACHIN/ MACHX        
      COMMON /SYSTEM/ KSYSTM(100)        
CZZ   COMMON /ZZXSEM/ Z(1)        
      COMMON /ZZZZZZ/ Z(1)        
      EQUIVALENCE     (KSYSTM( 2),NOUT), (KSYSTM(42),IDATE(1)),        
     1                (KSYSTM( 9),NLPP), (KSYSTM(11),   IPAGE),        
     2                (KSYSTM(91),LPCH), (C7,C1,MCHTTL       ),        
     3                (KSYSTM(32),TIMEW)        
      DATA    C1(25), C1(26),C1(27),C1(28),C1(29), XTRA / 6*' '   /,    
     1        MCHTTL/ '        COMPUTER SYSTEMS' /, FORT/ 4HFORT  /     
      DATA    FTN,FTN5,FOR,UNIX  / 4HFTN , 4HFTN5, 4HFOR , 4HUNIX /     
      DATA    COMPUT/        
     1        'DUMMY  ', 'IBM    ', 'UNIVAC ', 'CDC    ', 'VAX/VMS',    
     2        'ULTRIX ', 'SUN    ', 'IBM/AIX', 'HP     ', 'SGI    ',    
     3        'MACINTH', 'CRAY   ', 'CONVEX ', 'NEC    ', 'FUJITSU',    
     4        'DATAGEN', 'AMDAHL ', 'PRIME  ', 'MS-DOS ', 'DUMMY  '/    
      DATA    NCHAR / 5,3,3,6,7,6,3,7,4,7,7,4,6,3,7,7,6,5,6,5      /    
C        
C     SET TOPT DEFAULT TO +2 FOR THE MAIN FRAMES, OR TO -1 FOR UNIX     
C     BASE WORKSTATION        
C        
      IF (TOPT .EQ. -9) TOPT = +2        
      IF (MACHX.GE.6 .AND. MACHX.LE.11) TOPT = -1        
C        
C     PROBLEM START TIME        
C        
      HH = TIMEW/3600        
      MM =(TIMEW - HH*3600)/60        
      SS = TIMEW - HH*3600 - MM*60        
C        
C     UPDATE MCHTTL LINE        
C        
      C7 = COMPUT(MACHX)        
      I  = NCHAR(MACHX) + 2        
      VN = UNIX        
      IF (MACHX.EQ.2 .OR. MACHX.EQ.5) VN = FOR        
      IF (MACHX .EQ. 3) VN = FTN        
      IF (MACHX .EQ. 4) VN = FTN5        
      IF (MACHX .EQ.21) VN = FORT        
      IF (I .GE. 9) GO TO 5        
      K = 9        
      DO 3 J = I,24        
      C1(J) = C1(K)        
    3 K = K + 1        
C        
C     BRANCH ON OPTION        
C        
C     TOPT = 1, PRINT ONE NASTRAN LOGO TITLE PAGE        
C          = 2, PRINT TWO NASTRAN LOGO TITLE PAGES        
C          = 3, PRINT DUMMY MESSAGE AND ONE SHORT TITLE PAGE        
C          = 4, READ AND PRINT ONE LINE USER INPUT CARD AND PRINT ONE   
C               NASTRAN SHORT TITLE PAGE        
C          = 0, OR .GE.5, NO TITLE PAGE PRINTED        
C          = NEGATIVE INTEGER, PRINT ONE NASTRAN SHORT TITLE PAGE       
C        
    5 KORE = KSYSTM(31) + 1        
      IF (MACHX.LT.5 .AND. MACHX.NE.3) KORE = KORSZ(Z(1)) + 999        
      KORE = KORE/1000        
      IF (TOPT.NE.2  .AND. TOPT.NE.1) GO TO 110        
C        
C     TOPT = 1, OR 2        
C        
      DO 100 I = 1,TOPT        
      IF (IPAGE.LE.0 .OR. I.EQ.2) WRITE (NOUT,10)        
      IF (NLPP  .GT. 48) WRITE (NOUT,20)        
      WRITE (NOUT,30) MCHTTL        
      WRITE (NOUT,40) VN,KORE        
      WRITE (NOUT,50)        
C     WRITE (NOUT,60) IDATE        
      WRITE (NOUT,60) IDATE(2),IDATE(3)        
      WRITE (NOUT,70) XTRA        
      WRITE (NOUT,75)        
      WRITE (NOUT,80)        
      WRITE (NOUT,85)        
      WRITE (NOUT,90)        
      WRITE (NOUT,95) HH,MM,SS        
 10   FORMAT (1H1)        
 20   FORMAT (///)        
 30   FORMAT (34X,17(1HM),        
     2       /28X,29(1HM),        
     3       /25X,35(1HM),        
     4       /22X,20(1HM),1X,20(1HM),22X,1H/,8X,A24,        
     5       /20X,45(1HM),18X,2H//)        
 40   FORMAT (1H+,93X,A4,10H VERSION -,I5,1HK)        
 50   FORMAT (18X,16(1HM),2X,31(1HM),14X,3H///,        
     7       /16X,53(1HM),10X,4(1H/),        
     8       /14X,13(1HM),9X,35(1HM),6X,5(1H/))        
 60   FORMAT (13X,12(1HM),2X, 9(1HM),2X,34(1HM),3X, 6(1H/),9X,        
     *       3X,18HSYSTEM RELEASE  - , A3,A2,4H ED.)        
C    *       19HSYSTEM RELEASE  -  , A4, A3, A2)        
 70   FORMAT (12X,12(1HM),1X,13(1HM),3X,15(1HM),2X,15(1HM),6(1H/),      
     *       28X,A16,        
     1       /11X,12(1HM),1X,17(1HM),2X,28(1HM),6(1H/),        
     2       /10X,13(1HM),1X,19(1HM),2X,24(1HM),6(1H/),        
     3       /9X,5(1HM),2X,7(1HM),1X,13(1HM),1X,7(1HM),2X,19(1HM),8(1H/)
     *,      2HMM,        
     4       /9X,14(1HM),1X,23(1HM),2X,14(1HM),8(1H/),1H-,4(1HM),       
     *       43X,1H*,1X,1H*,1X,1H*,        
     5       /8X,16(1HM),1X,24(1HM),1X,9(1HM),9(1H/),2H--,7(1HM),       
     *       41X,1H*,5X,1H*)        
 75   FORMAT (8X,16(1HM),1X,25(1HM),2X,4(1HM),10(1H/),2H--,9(1HM),      
     *       41X,1H*,2X,1HR,2X,1H*,        
     7       /8X,16(1HM),1X,27(1HM),1X,1HM,8(1H/),4HMM--,11(1HM),       
     *       41X,1H*,5X,1H*,        
     8       /7X,8(1HM),4X,6(1HM),4X,5(1HM),5X,10(1HM),8X,4H//MM,11X,   
     *       2HMM,3X,6(1HM),7X,5(1HM),8X,4(1HM),6X,4(1HM),2X,1H*,1X,1H*,
     *       1X,1H*,        
     9       /7X,9(1HM),4X,6(1HM),2X,7(1HM),4X,6(1HM),14H///   /// M  M,
     *       25HM- MMM   MMM MMM  M   MMM,7X,4(1HM),9X,4(1HM),6X,2HMM)  
 80   FORMAT (7X,9(1HM),5X,5(1HM),2X,6(1HM),3H  M,3X,8(1H/),3X,4(1HM),  
     *       5H MM--,5(1HM),3X,7(1HM),21H  M    MMM     MM MMM,8X,      
     *       5(1HM),5X,2HMM,        
     1       /7X,9(1HM),2X,1HM,4X,5HMMM  ,4(1HM),6H// ///,3X,5(1H/),    
     *       13HMMM   MMMM-- ,6(1HM),3X,7(1HM),41H  M    MMM     M   MMM
     *       MM MMMM   MM,        
     2       /7X,9(1HM),2X,2HMM,4X,2HMM,3X,4(1H/),2X,3H///,4X,8(1HM),   
     *       4X,4H--M ,7(1HM),3X,7(1HM),2X,1HM,3X,3HMMM,5X,2HMM,3X,     
     *       4(1HM),6X,2HMM,2X,4(1HM),2X,2HMM)        
 85   FORMAT (7X,9(1HM),2X,4(1HM),6X,11H/ /// ///MM,4X,8(1HM),4H---M,   
     *       4X,6(1HM),3X,7(1HM),2X,6(1HM),6X,1HM,5X,4(1HM),5X,2HMM,    
     *       4X,6(1HM),        
     4       /7X,9(1HM),2X,5(1H/),5X,4H// M,11X,6(1HM),3H---,4(1HM),4X, 
     *       5(1HM),3X,7(1HM),7H  M MMM,6X,11(1HM),5X,2HMM,5X,5(1HM),   
     5       /7X,2HMM,7(1H/),2X,6(1HM),4X,3HMMM,2X,7(1HM),4X,7HMMM----, 
     *       4HMMMM,4X,6HM MMMM,3X,7(1HM),8H  M  MMM,4X,2HMM,7X,        
     *       4(1HM),4X,2HMM,6X,4(1HM),        
     6       /5X,4(1H/),6(1HM),4X,7(1HM),2X,2HMM,4X,5(1HM),5X,5H----M,  
     *       9X,6HMM MMM,5X,5(1HM),3X,2HMM,3X,2HMM,2X,4(1HM),5X,        
     *       6(1HM),2X,4(1HM),7X,2HMM)        
 90   FORMAT (3X,2H//,3X,26(1HM),1X,6(1HM),4(1H-),16(1HM),1X,15(1HM),   
     *       6X,3HMMM,        
     8       /8X, 27(1HM),7H MM----,19(1HM),1X,15(1HM),        
     9       /8X, 27(1HM),3H---,23(1HM),1X,15(1HM),        
     O       /9X, 24(1HM),7H---MM  ,22(1HM),1X,13(1HM),        
     1       /9X, 22(1HM),2H--,6(1HM),4X,19(1HM),1X,5(1HM),2X,6(1HM),   
     2       /10X,19(1HM),3H---,7(1HM),4X,19(1HM),1X,12(1HM),        
     3       /11X, 9(1HM),1X,6(1HM),2H--,  33(1HM), 1X, 11(1HM),        
     4       /12X,13(1HM),3H---,33(1HM),1X,11(1HM))        
 95   FORMAT (13X,11(1HM),2H--, 22(1HM),2X, 9(1HM), 2X, 11(1HM),        
     6       /14X, 8(1HM),2H--,26(1HM), 9X,12(1HM),        
     7       /16X, 5(1HM),2H--,46(1HM),24X,14HDISTRIBUTED BY,        
     8       /18X, 4HMM--,13(1HM),2X,30(1HM),        
     9       /19X, 1H-,   45(1HM),5X,        
     *       51HCOMPUTER SOFTWARE MANAGEMENT AND INFORMATION CENTER,    
     *       9H (COSMIC),        
     O       /18X,1H-,3X,41(1HM),26X,22HUNIVERSITY OF  GEORGIA,        
     1       /17X,1H-,7X,35(1HM),29X,22HATHENS, GEORGIA  30602,        
     2       /28X,29(1HM),        
     3       /1X,'TIME:',I3,':',I2,':',I2,        
     4       19X,17(1HM),32X,33HPHONE  (706) 542-3265   FAX -4807)      
 100  CONTINUE        
      GO TO 240        
C        
 110  IF (TOPT  ) 160,240,120        
 120  IF (TOPT-4) 130,210,240        
C        
C     TOPT = 3        
C        
 130  WRITE  (NOUT, 10)        
      WRITE  (NOUT,140)        
 140  FORMAT (' THIS COMMENT CAN BE USED TO IDENTIFY LOCAL FIXES - ',   
     1        'TO CHANGE, UPDATE DECK TTLPGE.')        
      GO TO 160        
C        
C     TOPT = NEGATIVE (AND 3, AND 4)        
C        
 160  IF (IPAGE .LE. 0) CALL PAGE1        
      WRITE  (NOUT,170) MCHTTL,IDATE(2),IDATE(3)        
      WRITE  (NOUT,180) VN,KORE        
      WRITE  (NOUT,190)        
 170  FORMAT (//////34X,4H****, /32X,1H*,6X,1H*, /31X,1H*,8X,1H*,       
     1       /31X,16H*  N A S T R A N,        
     2       /31X,1H*,8X,1H*, /32X,1H*,6X,1H*, /34X,4H****,        
     3       ///11X,A24,10X,17HSYSTEM RELEASE - ,A3,A2, 4H ED.)        
 180  FORMAT (11X,A4,8H VERSION,22X,'OPEN CORE',I7,'K WORDS')        
 190  FORMAT (/33X,'DISTRIBUTED BY', //11X,'COMPUTER SOFTWARE MANAGE',  
     1       'MENT AND INFORMATION CENTER (COSMIC)', /11X,'UNIVERSITY ',
     2       'OF GEORGIA, ATHENS, GEORGIA 30602   (706)542-3265', /52X, 
     3       'FAX   (706)542-4807')        
      WRITE  (NOUT,200) HH,MM,SS        
 200  FORMAT (//11X,'START TIME:',I3,':',I2,':',I2)        
      GO TO 240        
C        
C     TOPT = 4        
C        
 210  WRITE  (NOUT,10)        
      CALL XREAD (*240,CARD)        
      WRITE  (NOUT,220) CARD        
 220  FORMAT (1X,20A4)        
      WRITE  (NOUT,200) HH,MM,SS        
      GO TO 160        
C        
C     CALL NSINFO TO PRINTOUT INSTALLATION-CENTER-TO-USER MESSAGES,     
C     FROM THE THIRD SECTION OF THE NASINFO FILE        
C        
 240  CALL NSINFO (3)        
C        
      RETURN        
      END        
