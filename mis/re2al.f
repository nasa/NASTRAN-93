      SUBROUTINE RE2AL (RE,ALPH)        
C        
C    THIS ROUTINE CONVERTS RE, A REAL SINGLE PRECISION NUMBER TO ALPHA, 
C    THE ALPHA-NUMERIC EQUIVALENT ROUNDING THE RESULT UPWARDS.        
C    INPUT/OUTPUT.        
C      RE - REAL SINGLE PRECISION INPUT        
C      ALPH-TWO BCD WORDS IN 2A4 FORMAT - OUTPUT.        
C    OUTPUT FORMAT.        
C      1. LEADING CHARACTER IS BLANK OR MINUS SIGN.        
C      2. F-FORMAT USED IF ABS(RE) .GT. .0099995 AND .LT. 1.0+5        
C          E.G.   * .012345*  *-543.210*  * 234567.*        
C      3. E-FORMAT USED OTHERWISE.  MAXIMUM EXPONENT LENGTH IS 3 DIGITS.
C          E.G.   * 2.456+8*  *-2.45+78*  * 2.4+178*        
C        
C    NOTE THAT 9.995+99 WILL BE OUTPUT AS 1.0+100        
C        
C    COMMENTS FROM G.CHAN/SPERRY    9/84        
C    =================================================================  
C        
C    THIS ROUTINE CAUSES FATAL ERROR IN THE FOLLOWING 3 CASES        
C        (1) RE .LT. -9994.999        
C        (2) RE .LT. 0. AND RE .GT. -0.001        
C        (3) LSHIFT AND RSHIFT FUNCTIONS DO NOT WORK IN VAX MACHINE     
C        
C     PRESENTLY, THIS ROUTINE IS REPLACED BY FP2A8 (AN ENTRY POINT IN   
C     INT2A8).  REACTIVATE THIS ROUTINE WHEN THE KNOWN BUGS ARE FIXED.  
C        
C     INTEGER  ALPH(2),IDIGIT(10),C(8),ISIMP(2),RSHIFT,ORF        
C        
C     COMMON /SYSTEM/ SKP(38),NBPC,NBPW,NCPW        
C        
C     DATA IDIGIT/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/        
C     DATA IBLANK,IPLUS,IMINUS,IPOINT/1H ,1H+,1H-,1H./        
C     DATA ISIMP/4H 0.0,4H    /        
C        
C     INITALIZE        
C        
C     ABSRE = ABS(RE)        
C     IF (ABSRE) 400,400,10        
C10   IP = ALOG10(ABSRE)        
C     ISIGNF = 0        
C     IF (ABSRE.LT.1 .AND. 10.**IP.NE.ABSRE) IP = IP-1        
C        
C     SET LEADING CHARACTER TO BLANK OR MINUS.  SET FORMAT TO E OR F.   
C        
C20   C(1) = IBLANK        
C     IF (RE.LT.0.) C(1) = IMINUS        
C     IF (ABSRE.GE..9999995 .AND. ABSRE.LT.1.00001) GO TO 700        
C     IF (ABSRE.GE..0099995 .AND. ABSRE.LT.99995.0) GO TO 100        
C        
C     E FORMAT        
C        
C     C(3)= IPOINT        
C     RRE = RE        
C22   IIP = IABS(IP)        
C25   IXP = ALOG10(FLOAT(IIP))+ 1.        
C        
C     NO.DIGITS + SIGN IN EXP = IXP+1.   NO.DIGITS  PRINTED=8-2-(IXP+1) 
C        
C     NDP   = 5 -IXP        
C     RNDRE = RRE + .05*10.**(IXP-3+IP)        
C     RNDRE = RE  + .05*10.**(IXP-3+IP)        
C     IP1   = ALOG10(RNDRE)        
C     IF (RNDRE.LT.1 .AND. 10.**IP1.NE.RNDRE) IP1 = IP1-1        
C        
C     HAS ROUNDING INCREASED THE EXPONENT        
C        
C     IF (IP .NE. IP1) GO TO 500        
C     IR = RNDRE * 10.** (NDP-1-IP)        
C     IC = IR/10**(NDP-1)        
C     C(2) = IDIGIT(IC+1)        
C     NDPM1 = NDP-1        
C     IR = IR - IC *10**(NDP-1)        
C     DO 40 I=1,NDPM1        
C     IC = IR/10**(NDP-1-I)        
C     C(I+3)= IDIGIT(IC+1)        
C     IR = IR-IC*10**(NDP-1-I)        
C40   CONTINUE        
C        
C     EXPONENT        
C        
C     C(8-IXP) = IPLUS        
C     IF (IP .LT. 0) C(8-IXP) = IMINUS        
C     DO 50 I=1,IXP        
C     IC = IIP/10**(IXP-I)        
C     ISUB = 8-IXP+I        
C     C(ISUB) = IDIGIT(IC+1)        
C     IIP = IIP - IC*10**(IXP-I)        
C50   CONTINUE        
C     GO TO 200        
C        
C     F-FORMAT        
C        
C100  CONTINUE        
C     NDP = 6        
C     IF (IP .LT. -1) NDP=NDP+1+IP        
C     IRE = ABSRE * 10.**(NDP-1-IP) +.5        
C     FRE = FLOAT(IRE) * 10.**(1-NDP+IP)        
C     IP1 = ALOG10(FRE)        
C     IF (FRE.LT.1 .AND. 10.**IP1.NE.FRE) IP1 = IP1 - 1        
C        
C     HAS ROUNDING CREATED A HIGH ORDER DIGIT        
C        
C     IF (IP1 .NE. IP) GO TO 600        
C     IPTIN = 0        
C     IPP = IP        
C     DO 120 I=1,6        
C     IF (IPP.GT.-1 .OR. IPTIN.EQ.1) GO TO 110        
C     C(I+1) = IPOINT        
C     IPTIN  = 1        
C110  IC  = IRE/10**(6-I)        
C     IRE = IRE-IC*10**(6-I)        
C     ISUB= I+IPTIN+1        
C     C(ISUB) = IDIGIT(IC+1)        
C     IF (IC.NE.0 .OR. IPTIN.EQ.1) ISIGNF = 1        
C     IF (ISIGNF.EQ.0 .AND. IC.EQ.0) C(ISUB) = IBLANK        
C     IPP = IPP-1        
C120  CONTINUE        
C     IF (IPTIN) 140,140,150        
C140  C(8) = IPOINT        
C150  CONTINUE        
C        
C     PACK FROM 8A1  TO 2A4        
C        
C200  LOCS = 1        
C     DO 230  I=1,2        
C     K = 0        
C     DO  220  J=1,4        
C     IIP  = LSHIFT (RSHIFT(C(LOCS),NBPC*(NCPW-1)),(NCPW-J)*NBPC)       
C     LOCS = LOCS+1        
C     K = ORF(K,IIP)        
C220  CONTINUE        
C230  ALPH(I) = K        
C290  RETURN        
C        
C     RE =0    SIMPLIFIED OUTPUT        
C        
C400  ALPH(1) = ISIMP(1)        
C     ALPH(2) = ISIMP(2)        
C     RETURN        
C        
C     ROUNDING HAS INCREASED NUMBER CHARS IN EXPONENT. ADJUST AND CONTIN
C        
C500  RRE = RNDRE        
C     IP  = IP1        
C     GO TO 22        
C600  IP  = IP1        
C     ABSRE = FRE        
C     GO  TO 100        
C700  C(2) = IDIGIT(2)        
C     C(3) = IPOINT        
C     C(4) = IDIGIT(1)        
C     C(5) = IDIGIT(1)        
C     C(6) = IDIGIT(1)        
C     C(7) = IDIGIT(1)        
C     C(8) = IDIGIT(1)        
C     GO TO 200        
C        
C    ================= END OF ORIGINAL RE2AL ===========================
C        
      EXTERNAL        LSHIFT        
      INTEGER         ALPH(2)        
      COMMON /SYSTEM/ IBUF,NOUT,DUMMY(37),NBPW        
C        
      CALL FP2A8 (*40,RE,ALPH)        
      IF (NBPW-60) 30,10,20        
C        
C     FOR 60- OR 64- BIT MACHINES, SAVE THE SECOND HALF OF REAL NUMBER  
C     IN THE SECOND ALPH WORD. THAT IS -        
C     THE FULL REAL NUMBER IS IN ALPH(1), ALL 8 BYTES, OR        
C     FIRST 4 BYTES IN ALPH(1), AND LAST 4 BYTES IN ALPH(2)        
C        
 10   ALPH(2) = LSHIFT(ALPH(1),24)        
      GO TO 30        
 20   ALPH(2) = LSHIFT(ALPH(1),32)        
 30   RETURN        
C        
 40   WRITE  (NOUT,50)        
 50   FORMAT (99X,'(IN FP2A8, CALLED FROM RE2AL)')        
      CALL MESAGE (-61,0,0)        
      GO TO 30        
      END        
