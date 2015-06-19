      SUBROUTINE XSEM07        
C        
C     THE PURPOSE OF THIS ROUTINE IS TO GET THE NEXT MODULE TO BE       
C     EXECUTED FROM THE OSCAR FILE, INITIALIZE THE MODULE AND CALL IT   
C     IF IT IS IN THIS LINK OR CALL THE LINK IN WHICH THE MODULE        
C     RESIDES.        
C        
      IMPLICIT INTEGER (A-Z)        
      EXTERNAL        LSHIFT,RSHIFT,ANDF        
      DIMENSION       EQUIV(2), PURGE(2),SUBNAM(2),        
     1                SCRTCH(3),WORDB(4),WORDE(2),NUMBR(10)        
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25        
      COMMON /XMSSG / UFM,UWM,UIM,SFM        
      COMMON /SEM   / MASK,MASK2,MASK3,PROGMI(15)        
     1       /SYSTEM/ SYSBUF,NOUT,XX(72),DBG75        
     2       /XLINK / LXLINK,MAXLNK,MXLINK(1)        
     3       /XFIST / FIST(2)        
     4       /XPFIST/ FSTRST        
     5       /OSCENT/ INOSCR(200)        
CZZ  6       /ZZXSEM/ DATABF(1)        
     6       /ZZZZZZ/ DATABF(1)        
     7       /BLANK / PARAM(60)        
     8       /XVPS  / VPS(1)        
     9       /MSGX  / NMSG        
      DATA    REW   / 1     /, NOREW/ 0     /, POOL / 4HPOOL  /,        
     1        SCRTCH/ 4HSCRA, 4HTCH0, 4HTCH0/,        
     2        NUMBR / 1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H0 /,        
     3        WORDE / 4HBEGN, 4HEND /,        
     4        IBLNK / 4H    /,EXIT  / 4HEXIT/        
      DATA    THISLK/ 7     /,SUBNAM/ 4HXSEM, 2H07/        
      DATA    EQUIV , PURGE / 4HEQUI, 4HV   , 4HPURG, 4HE   /        
      DATA    XEQU  , XPUR  / 4HXEQU, 4HXPUR/        
      DATA    XSAV  , YCHK  / 4HXSAV, 4HXCHK/        
C        
C     INITIALIZE MACHINE DEPENDENT CONSTANTS        
C        
      CALL BTSTRP        
C        
C     CALL BGNSYS TO INITIATE PROCESSING        
C        
      CALL BGNSYS        
      IBUF1 = KORSZ(DATABF) - SYSBUF        
      GO TO 30        
C        
C     RETURN HERE AFTER MODULE HAS EXECUTED        
C        
   10 IF (INOSCR(4).EQ.XSAV .OR. INOSCR(4).EQ.YCHK) GO TO 20        
      WORDB(4) = WORDE(2)        
      CALL CONMSG (WORDB,4,0)        
   20 IF (NMSG .GT. 0) CALL MSGWRT        
      CALL OPEN (*270,POOL,DATABF(IBUF1),2)        
C        
C     READ THE OSCAR ENTRY        
C        
   30 CALL READ (*280,*40,POOL,INOSCR,200,1,ERRFLG)        
      GO TO 290        
   40 IF (INOSCR(6)) 50,30,30        
C        
C     TRY AGAIN IF EXECUTE FLAG IS OFF        
C        
   50 CALL CLOSE (POOL,2)        
      TYPECD = ANDF(INOSCR(3),MASK)        
C        
C     NOW DETERMINE TYPE OF OSCAR FORMAT        
C        
      IF (TYPECD .GT. 2) GO TO 200        
C        
C        
C     NOW PROCESSING TYPE O AND F        
C        
   60 MODNO  = INOSCR(2)        
      FIST(2)= FSTRST        
      OPNTR  = 7        
      ASSIGN 110 TO MM        
      FISTNM = 101        
C        
C     PROCESS FILES IN OSCAR ENTRY.        
C        
   70 J = INOSCR(OPNTR)        
      OPNTR = OPNTR + 1        
      IF (J .EQ. 0) GO TO 100        
      DO 90 I = 1,J        
      CALL GNFIST (INOSCR(OPNTR),FISTNM,MODNO)        
      IF (MODNO) 60,260,80        
   80 OPNTR  = OPNTR  + 3        
   90 FISTNM = FISTNM + 1        
  100 GO TO MM, (110,120)        
C        
C     SETUP TO PROCESS OUTPUT FILES        
C        
  110 IF (TYPECD .EQ. 2) GO TO 120        
      ASSIGN 120 TO MM        
      FISTNM = 201        
      GO TO 70        
C        
C     PROCESS SCRATCH FILES        
C        
  120 J1 = INOSCR(OPNTR)        
      IF (J1 .EQ. 0) GO TO 140        
      FISTNM = 301        
      SCRTCH(2) = SCRTCH(3)        
      LL = 1        
      L  = 0        
      DO 130 J = 1,J1        
      L  = L + 1        
      IF (L .EQ. 10) SCRTCH(2) = KHRFN1(SCRTCH(2),3,NUMBR(LL),1)        
      SCRTCH(2) = KHRFN1(SCRTCH(2),4,NUMBR(L),1)        
      CALL GNFIST (SCRTCH,FISTNM,MODNO)        
      IF (L .NE. 10) GO TO 125        
      L  = 0        
      LL = LL + 1        
  125 IF (MODNO) 60,260,130        
  130 FISTNM = FISTNM + 1        
  140 OPNTR  = OPNTR  + 1        
C        
C     NOW PROCESS PARAMETER LIST IN OSCAR        
C     PARMN = NO. OF PARAMETERS TO PROCESS        
C        
      PARMN = INOSCR(OPNTR)        
      IF (PARMN .EQ. 0) GO TO 200        
      II = 1        
      OPNTR = OPNTR + 1        
      DO 190 J2 = 1,PARMN        
      IF (INOSCR(OPNTR)) 170,150,150        
C        
C     NOW PROCESS CONSTANT PARAMETER        
C        
  150 PARML = INOSCR(OPNTR)        
      OPNTR = OPNTR + 1        
      DO 160 J3 = 1,PARML        
      PARAM(II) = INOSCR(OPNTR)        
      II = II + 1        
  160 OPNTR = OPNTR + 1        
      GO TO 190        
C        
C     MOVE VARIABLE INTO COMMON VIA VPS TABLE        
C        
  170 VPSX  = ANDF(INOSCR(OPNTR),MASK3)        
      OPNTR = OPNTR + 1        
      VPARML= VPS(VPSX-1)        
      DO 180 J5 = 1,VPARML        
      PARAM(II) = VPS(VPSX)        
      II = II + 1        
  180 VPSX = VPSX + 1        
  190 CONTINUE        
      IF (II .GT. 51) STOP 'XSEM07@190'        
C        
C     USE LINK SPECIFICATION TABLE (MXLINK) TO DETERMINE IF MODULE      
C     RESIDES IN THIS LINK.        
C     MODX = INDEX INTO MXLINK        
C        
  200 MODX = RSHIFT(INOSCR(3),16)        
      IF (ANDF(MXLINK(MODX),LSHIFT(1,THISLK-1)) .NE. 0) GO TO 230       
C        
C     MODULE IS NOT IN THIS LINK - DETERMINE CLOSEST LINK WHERE IT      
C     RESIDES.        
C        
      J = 2*MAXLNK + 1        
      DO 210 I = 1,MAXLNK        
      IF (ANDF(MXLINK(MODX),LSHIFT(1,I-1)) .EQ. 0) GO TO 210        
C        
C     MODULE IS IN LINK I - SEE IF LINK I IS CLOSER THAN LINK J.        
C        
      IF (MIN0(IABS(THISLK-J),IABS(THISLK-I)) .EQ. IABS(THISLK-J))      
     1    GO TO 220        
C        
C     LINK I IS CLOSER THAN J - MAKE LINK J CLOSEST LINK FOUND SO FAR.  
C        
      J = I        
  210 CONTINUE        
      I = 2*MAXLNK        
  220 IF (J .GT. MAXLNK) GO TO 950        
C        
C     CALL ENDSYS TO GET LINK J        
C        
      IF (THISLK-J .EQ. I-THISLK) J = I        
      NAME = PROGMI(J)        
      IF (THISLK .EQ. J) GO TO 960        
      CALL ENDSYS (NAME,PROGMI(7))        
      RETURN        
C        
C     MODULE IS IN THIS LINK        
C     PRINT TIME MODULE BEGAN EXECUTION IF FUNCTIONAL MODULE        
C        
  230 WORDB(2) = INOSCR(4)        
      WORDB(3) = INOSCR(5)        
      IF (INOSCR(4).NE.XEQU .AND. INOSCR(4).NE.XPUR) GO TO 250        
      IF (INOSCR(4) .NE. XEQU) GO TO 240        
      WORDB(2) = EQUIV(1)        
      WORDB(3) = EQUIV(2)        
      GO TO 250        
  240 WORDB(2) = PURGE(1)        
      WORDB(3) = PURGE(2)        
  250 CALL TMTOGO (KTIME)        
      IF (KTIME.LE.0 .AND. WORDB(2).NE.EXIT)        
     1    CALL MESAGE (-50,0,WORDB(2))        
      IF (INOSCR(4).EQ.XSAV .OR. INOSCR(4).EQ.YCHK) GO TO 1000        
      WORDB(1) = IBLNK        
      WORDB(4) = WORDE(1)        
C        
C     EXTRACT DMAP SEQUENCE NUMBER        
C        
      IDIN = ANDF(INOSCR(6),MASK)        
      J    = IDIN        
      PARAM(60) = IDIN        
      DO 251 I = 1,4        
      ICHR = IDIN - (IDIN/10)*10        
      IF (ICHR .EQ. 0) ICHR = 10        
      L = 5 - I        
      WORDB(1) = KHRFN1(WORDB(1),L,NUMBR(ICHR),1)        
      IDIN = IDIN/10        
      IF (IDIN .EQ. 0) GO TO 252        
  251 CONTINUE        
  252 CONTINUE        
      CALL CONMSG (WORDB,4,0)        
      IF (DBG75 .EQ. 2) WRITE (NOUT,255) THISLK,MODX,J,INOSCR(4),       
     1                                   INOSCR(5)        
  255 FORMAT (/,' <LINK',I2,1H.,I4,'.   DMAP SEQ. NO.',I4,2X,2A4,1H>)   
      GO TO 1000        
C        
C        
C     ERROR MESSAGES -        
C        
C     MODULE REQUIREMENTS EXCEED AVAILABLE FILES        
  260 INOSCR(6) = ANDF(INOSCR(6),MASK)        
      CALL MESAGE (-18,INOSCR(6),INOSCR(4))        
C        
C     UNEXPECTED ALTERNATE RETURN TAKEN WHILE ATTEMPTING TO OPEN POOL   
C     TAPE.        
C        
  270 CONTINUE        
      KODE = 270        
      GO TO 990        
C        
C     OSCAR FILE POSITIONED INCORRECTLY - HIT EOF.        
C        
  280 CONTINUE        
      KODE = 280        
      GO TO 990        
C        
C     OSCAR RECORD TOO LARGE FOR /OSCENT/        
C        
  290 CONTINUE        
      KODE = 290        
      GO TO 990        
C        
C     LINK SPECIFICATIONS INCORRECT FOR THIS MODULE.        
C        
  940 CONTINUE        
      WRITE  (NOUT,945) WORDB,MODX        
  945 FORMAT (/1X,4A4,I9)        
      KODE = 940        
      GO TO 990        
C        
C     LOGIC ERROR AT 220        
C        
  950 CONTINUE        
      KODE = 950        
      GO TO 990        
C        
C     LOGIC ERROR AT 230        
C        
  960 CONTINUE        
      KODE = 960        
      GO TO 990        
C        
C     LOGIC ERROR AT 1000        
C        
  970 CONTINUE        
      KODE = 970        
      GO TO 990        
C        
  990 CONTINUE        
      WRITE  (NOUT,991) SFM,KODE        
  991 FORMAT (A25,' 1006, LINK DRIVER LOGIC ERROR- CODE =',I4)        
      CALL MESAGE (-37,0,SUBNAM)        
C        
C        
C     EXECUTE MODULE        
C        
  995 GO TO (2003, 940,2005,2006,2007,2008,2009,2010,2011,2012, 940,    
     1       2014,2015), I        
 1000 IF (MODX .GT. 15) GO TO 1010        
      I = MODX - 2        
      IF (I) 940,940,995        
 1005 GO TO (2021,2022), I        
 1010 IF (MODX .GT. 22) GO TO 1020        
      I = MODX - 20        
      IF (I) 940,940,1005        
 1015 GO TO (2030, 940, 940,2033), I        
 1020 IF (MODX .GT. 33) GO TO 1030        
      I = MODX - 29        
      IF (I) 940,940,1015        
 1025 GO TO (2037,2038,2039,2040), I        
 1030 IF (MODX .GT. 40) GO TO 1040        
      I = MODX - 36        
      IF (I) 940,940,1025        
 1040 IF (MODX -  46) 940,2046,1050        
 1050 IF (MODX -  57) 940,2057,1060        
 1055 GO TO (2067, 940,2069,2070, 940, 940,2073, 940,2075, 940,2077,    
     1       2078,2079), I        
 1060 IF (MODX .GT. 79) GO TO 1070        
      IF (MODX .EQ. 59) GO TO 2059        
      I = MODX - 66        
      IF (I) 940,940,1055        
 1065 GO TO (2090,2091,2092,2093)I        
 1070 IF (MODX .GT. 93) GO TO 1080        
      I = MODX - 89        
      IF (I) 940,940,1065        
 1080 IF (MODX - 124) 940,2124,1090        
 1085 GO TO (2130,2131), I        
 1090 IF (MODX .GT.131) GO TO 1100        
      I = MODX - 129        
      IF (I) 940,940,1085        
 1095 GO TO (2139, 940,2141,2142), I        
 1100 IF (MODX .GT. 142) GO TO 1110        
      I = MODX - 138        
      IF (I) 940,940,1095        
 1105 GO TO (2148,2149,2150, 940,2152), I        
 1110 IF (MODX .GT. 152) GO TO 1120        
      I = MODX - 147        
      IF (I) 940,940,1105        
 1115 GO TO (2173,2174,2175,2176), I        
 1120 IF (MODX .GT. 176) GO TO 1130        
      I = MODX - 172        
      IF (I) 940,940,1115        
 1130 IF (MODX .GT. 206) GO TO 1140        
      I = MODX - 204        
      GO TO (2205,2206), I        
 1140 IF (MODX .GT. 218) GO TO 1150        
      I = MODX - 213        
      GO TO (2214,2215,940,940,2218), I        
 1150 IF (MODX-LXLINK) 940,940,970        
 2003 CALL XCHK        
      GO TO 10        
 2005 CALL XCEI        
      GO TO 10        
 2006 CALL XCEI        
      GO TO 10        
 2007 CALL XCEI        
      GO TO 10        
 2008 CALL XSAVE        
      GO TO 10        
 2009 CALL XPURGE        
      GO TO 10        
 2010 CALL XEQUIV        
      GO TO 10        
 2011 CALL XCEI        
      GO TO 10        
 2012 CALL XCEI        
      GO TO 10        
 2014 CALL DADD        
      GO TO 10        
 2015 CALL DADD5        
      GO TO 10        
 2021 CALL CYCT1        
      GO TO 10        
 2022 CALL CYCT2        
      GO TO 10        
 2030 CALL DDCOMP        
      GO TO 10        
 2033 CALL DSCHK        
      GO TO 10        
 2037 CALL DUMOD1        
      GO TO 10        
 2038 CALL DUMOD2        
      GO TO 10        
 2039 CALL DUMOD3        
      GO TO 10        
 2040 CALL DUMOD4        
      GO TO 10        
 2046 CALL DFBS        
      GO TO 10        
 2057 CALL GPCYC        
      GO TO 10        
 2059 CALL DUMOD5        
      GO TO 10        
 2067 CALL MATGEN        
      GO TO 10        
 2069 CALL MATPRN        
      GO TO 10        
 2070 CALL PRTINT        
      GO TO 10        
 2073 CALL MERGE1        
      GO TO 10        
 2075 CALL MODA        
      GO TO 10        
 2077 CALL MODB        
      GO TO 10        
 2078 CALL MODC        
      GO TO 10        
 2079 CALL DMPYAD        
      GO TO 10        
 2090 CALL QPARAM        
      GO TO 10        
 2091 CALL PARAML        
      GO TO 10        
 2092 CALL QPARMR        
      GO TO 10        
 2093 CALL PARTN1        
      GO TO 10        
 2124 CALL SETVAL        
      GO TO 10        
 2130 CALL SMPYAD        
      GO TO 10        
 2131 CALL SOLVE        
      GO TO 10        
 2139 CALL TABPCH        
      GO TO 10        
 2141 CALL TABFMT        
      GO TO 10        
 2142 CALL TABPT        
      GO TO 10        
 2148 CALL DTRANP        
      GO TO 10        
 2149 CALL DUMERG        
      GO TO 10        
 2150 CALL DUPART        
      GO TO 10        
 2152 CALL VEC        
      GO TO 10        
 2173 CALL COPY        
      GO TO 10        
 2174 CALL SWITCH        
      GO TO 10        
 2175 CALL MPY3        
      GO TO 10        
 2176 CALL DDCMPS        
      GO TO 10        
 2205 CALL FVRST1        
      GO TO 10        
 2206 CALL FVRST2        
      GO TO 10        
 2214 CALL QPARMD        
      GO TO 10        
 2215 CALL GINOFL        
      GO TO 10        
 2218 CALL GRBVEC        
      GO TO 10        
      END        
