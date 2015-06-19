      SUBROUTINE XSEM00                                                 00000100
C **********************************************************************00000200
C THE PURPOSE OF THIS ROUTINE IS TO EXECUTE THE PREFACE AND THEN TO     00000300
C EXECUTE MODULES ACCORDING TO THE DMAP.  THE DMAP IS READ FROM THE     00000400
C OSCAR.  FOR EACH MODULE TO BE EXECUTED, THE FIST AND XVPS ARE SETUP.  00000500
C                                                                       00000700
      INTEGER ANDF ,DATABF,ERRFLG,FIST  ,FISTNM,FSTRST,OPNTR ,ORF       00000800
     1       ,PARML,PARAM ,PARMN ,POOL  ,REW   ,RSHIFT,SCRCHM,SCRTCH    00000900
     2       ,VPS  ,VPARML,TYPECD,VPSX  ,WORDB ,WORDE ,PROGMI           00001000
     3       ,PLOTF,THISLK,EXIT  ,SYSBUF,SUBNAM(2)                      00001100
      INTEGER EQUIV(2), PURGE(2), XEQU, XPUR, XSAV, YCHK                00001200
C                                                                       00001400
      LOGICAL LVAX                                                      00001500
C                                                                       00001600
      DIMENSION SCRTCH(3),WORDB(4),WORDE(2),NUMBR(10)                   00001712
C                                                                       00001800
      COMMON/MACHIN/MACH                                                00001900
      COMMON/SEM   /MASK  ,MASK2 ,MASK3 ,LINKNM(15)                     00002006
     1                                                                  00002100
     F      /SYSTEM/SYSBUF,XX(20),LINKNO,XXX(16),NBPC,NBPW,NCPW,XXXX(53)00002217
     F             ,ISPERLNK                                            00002316
     G                                                                  00002400
     H      /XLINK /LXLINK,MAXLNK,MXLINK(1)                             00002500
     1                                                                  00002600
     2      /XFIST /FIST(2)                                             00002700
     3                                                                  00002800
     4      /XPFIST/FSTRST                                              00002900
     5                                                                  00003000
     6      /OSCENT/INOSCR(200)                                         00003100
     7                                                                  00003200
     8      /ZZZZZZ/DATABF(1)                                           00003318
     9                                                                  00003400
     A      /BLANK /PARAM(100)                                          00003500
     B                                                                  00003600
     C      /XVPS  /VPS(1)                                              00003700
     D                                                                  00003800
     E      /MSGX  /NMSG                                                00003900
C                                                                       00004000
      EQUIVALENCE (XX(1),NOUT)                                          00004100
      EQUIVALENCE (XX(19),PLOTF)                                        00004200
C                                                                       00004300
      DATA REW     /     1/,NOREW/     0/,POOL /4HPOOL/                 00004400
     3,    SCRTCH  /4HSCRA,4HTCH0,4HTCH0/                               00004512
     4,    NUMBR   /1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H0 /           00004614
     5,    WORDB   /4HSEM1,4HBEGN,4H    ,4H    /                        00004700
     5,    WORDE   /4HBEGN,4HEND /                                      00004800
     6,    IBLNK   /4H    /                                             00004900
     6,    MODX    /   215/                                             00005000
     7,    EXIT    /4HEXIT/                                             00005100
      DATA THISLK  /     2/                                             00005200
      DATA SUBNAM  /4HXSEM,2H00/                                        00005300
      DATA BCDZRO  /1H0   /                                             00005400
      DATA EQUIV, PURGE /4HEQUI, 4HV   , 4HPURG, 4HE   /                00005500
      DATA XEQU , XPUR  /4HXEQU, 4HXPUR/                                00005600
      DATA XSAV , YCHK  /4HXSAV, 4HXCHK/                                00005700
C*****                                                                  00005800
C INITIALIZE MACHINE DEPENDENT CONSTANTS                                00005900
      CALL BTSTRP                                                       00006000
      LVAX = MACH.EQ.5                                                  00006100
C*****                                                                  00006200
C EXECUTE PREFACE                                                       00006300
C*****                                                                  00006400
      KSCR= LSHIFT(1,NBPW-4*NBPC)                                       00006500
      CALL TDATE(XX(14))                                                00006600
      CALL CONMSG(WORDB,2,1)                                            00006708
      CALL SEMINT ( 0 )                                                 00006815
      ISPERLNK = 1                                                      00006916
      WORDB(2) = WORDE(2)                                               00007000
      CALL CONMSG ( WORDB,2,1)                                          00007100
      IPLOT = PLOTF                                                     00007200
      IF (PLOTF .LT. 0) PLOTF=1                                         00007300
      IBUF1 = KORSZ(DATABF)-SYSBUF                                      00007400
      GO TO 20                                                          00007500
C*****                                                                  00007600
C RETURN HERE AFTER MODULE HAS EXECUTED                                 00007700
C*****                                                                  00007800
   10 IF (INOSCR(4).EQ.XSAV.OR.INOSCR(4).EQ.YCHK) GO TO 20              00007900
      WORDB(4) = WORDE(2)                                               00008000
      CALL CONMSG(WORDB,4,0)                                            00008100
   20 IF(NMSG .GT. 0) CALL MSGWRT                                       00008200
      CALL OPEN(*270,POOL,DATABF(IBUF1),2)                              00008300
C*****                                                                  00008400
C READ THE OSCAR ENTRY                                                  00008500
C*****                                                                  00008600
   30 CALL READ(*280,*40,POOL,INOSCR,200,1,ERRFLG)                      00008700
      GO TO 290                                                         00008800
   40 IF (INOSCR(6))50,30,30                                            00008900
C*****                                                                  00009000
C TRY AGAIN IF EXECUTE FLAG IS OFF                                      00009100
C*****                                                                  00009200
   50 CALL CLOSE(POOL,2)                                                00009300
      TYPECD= ANDF(INOSCR(3),MASK)                                      00009400
C*****                                                                  00009500
C NOW DETERMINE TYPE OF OSCAR FORMAT                                    00009600
C*****                                                                  00009700
      IF(TYPECD .GT. 2)  GO TO 200                                      00009800
C*****                                                                  00009900
C*****                                                                  00010000
C NOW PROCESSING TYPE O AND F                                           00010100
C*****                                                                  00010200
   60 MODNO= INOSCR(2)                                                  00010300
      FIST(2)= FSTRST                                                   00010400
      OPNTR = 7                                                         00010500
      ASSIGN 110 TO MM                                                  00010600
      FISTNM=101                                                        00010700
C*****                                                                  00010800
C PROCESS FILES IN OSCAR ENTRY.                                         00010900
C*****                                                                  00011000
   70 J=INOSCR(OPNTR)                                                   00011100
      OPNTR=OPNTR+1                                                     00011200
      IF(J.EQ.0) GO TO 100                                              00011300
      DO 90 I=1,J                                                       00011400
      CALL GNFIST(INOSCR(OPNTR),FISTNM,MODNO)                           00011500
      IF(MODNO) 60,260,80                                               00011600
   80 OPNTR= OPNTR+ 3                                                   00011700
   90 FISTNM=FISTNM+1                                                   00011800
  100 GO TO MM,(110,120)                                                00011900
C*****                                                                  00012000
C SETUP TO PROCESS OUTPUT FILES                                         00012100
C*****                                                                  00012200
  110 IF(TYPECD.EQ.2) GO TO 120                                         00012300
      ASSIGN 120 TO MM                                                  00012400
      FISTNM=201                                                        00012500
      GO TO 70                                                          00012600
C*****                                                                  00012700
C PROCESS SCRATCH FILES                                                 00012800
C*****                                                                  00012900
  120 J1= INOSCR(OPNTR)                                                 00013000
      IF(J1.EQ.0) GO TO 140                                             00013100
      FISTNM= 301                                                       00013200
      SCRTCH(2) = SCRTCH(3)                                             00013312
      LL = 1                                                            00013412
      L  = 0                                                            00013512
      DO 130 J=1,J1                                                     00013600
      L = L + 1                                                         00013712
      IF ( L .EQ. 10 ) SCRTCH(2) = KHRFN1(SCRTCH(2),3,NUMBR(LL),1)      00013812
      SCRTCH(2) = KHRFN1(SCRTCH(2),4,NUMBR(L),1)                        00013912
      CALL GNFIST(SCRTCH,FISTNM,MODNO)                                  00014100
      IF ( L .NE. 10 ) GO TO 125                                        00014212
      L  = 0                                                            00014312
      LL = LL + 1                                                       00014412
  125 IF(MODNO) 60,260,130                                              00014512
  130 FISTNM=FISTNM+1                                                   00014600
  140 OPNTR=OPNTR+1                                                     00014700
C*****                                                                  00014800
C NOW PROCESS PARAMETER LIST IN OSCAR                                   00014900
C  PARMN = NO. OF PARAMETERS TO PROCESS                                 00015000
C*****                                                                  00015100
      PARMN=INOSCR(OPNTR)                                               00015200
      IF(PARMN .EQ. 0)  GO TO 200                                       00015300
      II=1                                                              00015400
      OPNTR= OPNTR+ 1                                                   00015500
      DO 190 J2=1,PARMN                                                 00015600
      IF(INOSCR(OPNTR))170,150,150                                      00015700
C*****                                                                  00015800
C NOW PROCESS CONSTANT PARAMETER                                        00015900
C*****                                                                  00016000
  150 PARML=INOSCR(OPNTR)                                               00016100
      OPNTR=OPNTR+1                                                     00016200
      DO 160 J3=1,PARML                                                 00016300
      PARAM(II)=INOSCR(OPNTR)                                           00016400
      II=II+1                                                           00016500
  160 OPNTR=OPNTR+1                                                     00016600
      GO TO 190                                                         00016700
C*****                                                                  00016800
C MOVE VARIABLE INTO COMMON VIA VPS TABLE                               00016900
C*****                                                                  00017000
  170 VPSX= ANDF(INOSCR(OPNTR),MASK3)                                   00017100
      OPNTR=OPNTR+1                                                     00017200
      VPARML=VPS(VPSX-1)                                                00017300
      DO 180 J5=1,VPARML                                                00017400
      PARAM(II)=VPS(VPSX)                                               00017500
      II=II+1                                                           00017600
  180 VPSX=VPSX+1                                                       00017700
  190 CONTINUE                                                          00017800
  200 MODX = RSHIFT(INOSCR(3),16)                                       00017900
C*****                                                                  00020200
C MODULE IS IN THIS LINK                                                00020300
C PRINT TIME MODULE BEGAN EXECUTION IF FUNCTIONAL MODULE                00020400
C*****                                                                  00020500
  245 WORDB(2) = INOSCR(4)                                              00020600
      WORDB(3) = INOSCR(5)                                              00020700
      IF (INOSCR(4).NE.XEQU.AND.INOSCR(4).NE.XPUR) GO TO 250            00020800
      IF (INOSCR(4).NE.XEQU) GO TO 248                                  00020900
      WORDB(2) = EQUIV(1)                                               00021000
      WORDB(3) = EQUIV(2)                                               00021100
      GO TO 250                                                         00021200
  248 WORDB(2) = PURGE(1)                                               00021300
      WORDB(3) = PURGE(2)                                               00021400
  250 CALL TMTOGO (KTIME)                                               00021500
      IF (KTIME.LE.0.AND.WORDB(2).NE.EXIT)                              00021600
     *   CALL MESAGE (-50, 0, WORDB(2))                                 00021700
      IF (INOSCR(4).EQ.XSAV.OR.INOSCR(4).EQ.YCHK) GO TO 1000            00021800
      WORDB(1) = IBLNK                                                  00021900
      WORDB(4) = WORDE(1)                                               00022000
C                                                                       00022100
C     EXTRACT DMAP SEQUENCE NUMBER                                      00022200
C                                                                       00022300
      IDIN  = ANDF(INOSCR(6),MASK)                                      00022400
      DO 251  I =1,4                                                    00022500
      ICHR  = IDIN -(IDIN/10)*10 +1                                     00022600
      L = NBPW-NBPC                                                     00022700
      IF (.NOT.LVAX)  WORDB(1) =                                        00022800
     *    ORF(RSHIFT(WORDB(1),NBPC),LSHIFT(RSHIFT(NUMBR(ICHR),L),L))    00022900
      IF (LVAX)  WORDB(1)=KHRFN1(WORDB(1),5-I,NUMBR(ICHR),1)            00023000
      IDIN = IDIN/10                                                    00023100
      IF(IDIN .EQ. 0)  GO TO 252                                        00023200
  251 CONTINUE                                                          00023300
  252 CONTINUE                                                          00023400
      CALL CONMSG(WORDB,4,0)                                            00023500
      GO TO 1000                                                        00023600
C*****                                                                  00023700
C                   E R R O R   M E S S A G E S                         00023800
C*****                                                                  00023900
C MODULE REQUIREMENTS EXCEED AVAILABLE FILES                            00024000
  260 INOSCR(6) = ANDF(INOSCR(6),MASK)                                  00024100
      CALL MESAGE(-18,INOSCR(6),INOSCR(4))                              00024300
C                                                                       00024400
C UNEXPECTED ALTERNATE RETURN TAKEN WHILE ATTEMPTING TO OPEN POOL TAPE. 00024500
  270 CONTINUE                                                          00024600
      KODE = 270                                                        00024700
      GO TO 990                                                         00024800
C                                                                       00024900
C OSCAR FILE POSITIONED INCORRECTLY - HIT EOF.                          00025000
  280 CONTINUE                                                          00025100
      KODE = 280                                                        00025200
      GO TO 990                                                         00025300
C                                                                       00025400
C OSCAR RECORD TOO LARGE FOR /OSCENT/                                   00025500
  290 CONTINUE                                                          00025600
      KODE = 290                                                        00025700
      GO TO 990                                                         00025800
C                                                                       00025900
C LINK SPECIFICATIONS INCORRECT FOR THIS MODULE.                        00026000
  940 CONTINUE                                                          00026100
      WRITE (NOUT,945) WORDB,MODX                                       00026200
  945 FORMAT (/1X,4A4,I9)                                               00026302
      KODE = 940                                                        00026400
      GO TO 990                                                         00026500
C                                                                       00026600
C                                                                       00028000
  990 CONTINUE                                                          00028100
      WRITE(NOUT,991) KODE                                              00028200
  991 FORMAT(64H0*** SYSTEM FATAL MESSAGE 1006, LINK DRIVER LOGIC ERROR 00028300
     *- CODE =,I4)                                                      00028400
      CALL MESAGE(-37,0,SUBNAM)                                         00028500
C********************************************************************** 00028600
C EXECUTE MODULE                                                        00054010
 1000 CALL SSWTCH ( 2, LDIAG )                                          00054110
C     IF ( LDIAG .NE. 0 .AND. MODX .GT. 14 ) CALL DBMDIA                00054210
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00054309
     1( 940,  940, 2003,  940, 2005, 2006, 2007, 2008, 2009, 2010),MODX 00054404
      MODX = MODX - 10                                                  00054503
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00054603
     1(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),MODX 00054700
      MODX = MODX - 10                                                  00054803
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00054903
     1(2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030),MODX 00055000
      MODX = MODX - 10                                                  00055103
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00055203
     1(2031, 2032, 2033, 2034, 2035, 2036, 2037, 2038, 2039, 2040),MODX 00055300
      MODX = MODX - 10                                                  00055403
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00055503
     1(2041, 2042, 2043, 2044, 2045, 2046, 2047, 2048, 2049, 2050),MODX 00055600
      MODX = MODX - 10                                                  00055703
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00055803
     1(2051, 2052, 2053, 2054, 2055, 2056, 2057, 2058, 2059, 2060),MODX 00055900
      MODX = MODX - 10                                                  00056003
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00056103
     1(2061, 2062, 2063, 2064, 2065, 2066, 2067, 2068, 2069, 2070),MODX 00056200
      MODX = MODX - 10                                                  00056303
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00056403
     1(2071, 2072, 2073, 2074, 2075, 2076, 2077, 2078, 2079, 2080),MODX 00056500
      MODX = MODX - 10                                                  00056603
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00056703
     1(2081, 2082, 2083, 2084, 2085, 2086, 2087, 2088, 2089, 2090),MODX 00056800
      MODX = MODX - 10                                                  00056903
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00057003
     1(2091, 2092, 2093, 2094, 2095, 2096, 2097, 2098, 2099, 2100),MODX 00057100
      MODX = MODX - 10                                                  00057203
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00057303
     1(2101, 2102, 2103, 2104, 2105, 2106, 2107, 2108, 2109, 2110),MODX 00057400
      MODX = MODX - 10                                                  00057503
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00057603
     1(2111, 2112, 2113, 2114, 2115, 2116, 2117, 2118, 2119, 2120),MODX 00057700
      MODX = MODX - 10                                                  00057803
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00057903
     1(2121, 2122, 2123, 2124, 2125, 2126, 2127, 2128, 2129, 2130),MODX 00058000
      MODX = MODX - 10                                                  00058103
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00058203
     1(2131, 2132, 2133, 2134, 2135, 2136, 2137, 2138, 2139, 2140),MODX 00058300
      MODX = MODX - 10                                                  00058403
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00058503
     1(2141, 2142, 2143, 2144, 2145, 2146, 2147, 2148, 2149, 2150),MODX 00058600
      MODX = MODX - 10                                                  00058703
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00058803
     1(2151, 2152, 2153, 2154, 2155, 2156, 2157, 2158, 2159, 2160),MODX 00058900
      MODX = MODX - 10                                                  00059003
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00059103
     1(2161, 2162, 2163, 2164, 2165, 2166, 2167, 2168, 2169, 2170),MODX 00059200
      MODX = MODX - 10                                                  00059303
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00059403
     1(2171, 2172, 2173, 2174, 2175, 2176, 2177, 2178, 2179, 2180),MODX 00059500
      MODX = MODX - 10                                                  00059603
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00059703
     1(2181, 2182, 2183, 2184, 2185, 2186, 2187, 2188, 2189, 2190),MODX 00059800
      MODX = MODX - 10                                                  00059903
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00060003
     1(2191, 2192, 2193, 2194, 2195, 2196, 2197, 2198, 2199, 2200),MODX 00060100
      MODX = MODX - 10                                                  00060203
      IF ( MODX .GE.  1  .AND. MODX .LE. 10 ) GO TO                     00060303
     1(2201, 2202, 2203, 2204, 2205, 2206, 2207, 2208, 2209, 2210),MODX 00060400
      MODX = MODX - 10                                                  00060503
      IF ( MODX .GE.  1  .AND. MODX .LE.  7 ) GO TO                     00060611
     1(2211, 2212, 2213, 2214, 2215, 2216, 2217), MODX                  00060711
      GO TO 940                                                         00060800
 2003 CALL XCHK                                                         00060900
      GO TO 10                                                          00061000
 2005 CALL XCEI                                                         00061100
      GO TO 10                                                          00061200
 2006 CALL XCEI                                                         00061300
      GO TO 10                                                          00061400
 2007 CALL XCEI                                                         00061500
      GO TO 10                                                          00061600
 2008 CALL XSAVE                                                        00061704
      GO TO 10                                                          00061804
 2009 CALL XPURGE                                                       00061900
      GO TO 10                                                          00062000
 2010 CALL XEQUIV                                                       00062100
      GO TO 10                                                          00062200
 2011 CALL XCEI                                                         00062300
      GO TO 10                                                          00062400
 2012 CALL XCEI                                                         00062500
      GO TO 10                                                          00062600
 2013 CALL XCEI                                                         00062700
      GO TO 10                                                          00062800
 2014 CALL DADD                                                         00062900
      GO TO 10                                                          00063000
 2015 CALL DADD5                                                        00063100
      GO TO 10                                                          00063200
 2016 CALL AMG                                                          00063300
      GO TO 10                                                          00063400
 2017 CALL AMP                                                          00063500
      GO TO 10                                                          00063600
 2018 CALL APD                                                          00063700
      GO TO 10                                                          00063800
 2019 CALL BMG                                                          00063900
      GO TO 10                                                          00064000
 2020 CALL CASE                                                         00064100
      GO TO 10                                                          00064200
 2021 CALL CYCT1                                                        00064300
      GO TO 10                                                          00064400
 2022 CALL CYCT2                                                        00064500
      GO TO 10                                                          00064600
 2023 CALL CEAD                                                         00064700
      GO TO 10                                                          00064800
 2024 CALL CURV                                                         00064900
      GO TO 10                                                          00065000
 2025 CONTINUE                                                          00065100
      GO TO 10                                                          00065200
 2026 CALL DDR                                                          00065300
      GO TO 10                                                          00065400
 2027 CALL DDR1                                                         00065500
      GO TO 10                                                          00065600
 2028 CALL DDR2                                                         00065700
      GO TO 10                                                          00065800
 2029 CALL DDRMM                                                        00065900
      GO TO 10                                                          00066000
 2030 CALL DDCOMP                                                       00066100
      GO TO 10                                                          00066200
 2031 CALL DIAGON                                                       00066300
      GO TO 10                                                          00066400
 2032 CALL DPD                                                          00066500
      GO TO 10                                                          00066600
 2033 CALL DSCHK                                                        00066700
      GO TO 10                                                          00066800
 2034 CALL DSMG1                                                        00066900
      GO TO 10                                                          00067000
 2035 CALL DSMG2                                                        00067100
      GO TO 10                                                          00067200
 2036 CONTINUE                                                          00067300
      GO TO 10                                                          00067400
 2037 CALL DUMOD1                                                       00067500
      GO TO 10                                                          00067600
 2038 CALL DUMOD2                                                       00067700
      GO TO 10                                                          00067800
 2039 CALL DUMOD3                                                       00067900
      GO TO 10                                                          00068000
 2040 CALL DUMOD4                                                       00068100
      GO TO 10                                                          00068200
 2041 CONTINUE                                                          00068300
      GO TO 10                                                          00068400
 2042 CALL EMA1                                                         00068500
      GO TO 10                                                          00068600
C         SET LINKNO TO FLAG SUBROUTINE SMA1B TO CALL EMG1B             00068706
 2043 LINKNO = LINKNM(8)                                                00068807
      CALL EMG                                                          00068906
      LINKNO = LINKNM(1)                                                00069007
      GO TO 10                                                          00069100
 2044 CALL FA1                                                          00069200
      GO TO 10                                                          00069300
 2045 CALL FA2                                                          00069400
      GO TO 10                                                          00069500
 2046 CALL DFBS                                                         00069600
      GO TO 10                                                          00069700
 2047 CALL FRLG                                                         00069800
      GO TO 10                                                          00069900
 2048 CALL FRRD                                                         00070000
      GO TO 10                                                          00070100
 2049 CONTINUE                                                          00070200
      GO TO 10                                                          00070300
 2050 CALL GI                                                           00070400
      GO TO 10                                                          00070500
 2051 CALL GKAD                                                         00070600
      GO TO 10                                                          00070700
 2052 CALL GKAM                                                         00070800
      GO TO 10                                                          00070900
 2053 CALL GP1                                                          00071000
      GO TO 10                                                          00071100
 2054 CALL GP2                                                          00071200
      GO TO 10                                                          00071300
 2055 CALL GP3                                                          00071400
      GO TO 10                                                          00071500
 2056 CALL GP4                                                          00071600
      GO TO 10                                                          00071700
 2057 CALL GPCYC                                                        00071800
      GO TO 10                                                          00071900
 2058 CALL GPFDR                                                        00072000
      GO TO 10                                                          00072100
 2059 CALL DUMOD5                                                       00072200
      GO TO 10                                                          00072300
 2060 CALL GPWG                                                         00072400
      GO TO 10                                                          00072500
 2061 CONTINUE                                                          00072600
      GO TO 10                                                          00072700
 2062 CALL INPUT                                                        00072800
      GO TO 10                                                          00072900
 2063 CALL INPTT1                                                       00073000
      GO TO 10                                                          00073100
 2064 CALL INPTT2                                                       00073200
      GO TO 10                                                          00073300
 2065 CALL INPTT3                                                       00073400
      GO TO 10                                                          00073500
 2066 CALL INPTT4                                                       00073600
      GO TO 10                                                          00073700
 2067 CALL MATGEN                                                       00073800
      GO TO 10                                                          00073900
 2068 CALL MATGPR                                                       00074000
      GO TO 10                                                          00074100
 2069 CALL MATPRN                                                       00074200
      GO TO 10                                                          00074300
 2070 CALL PRTINT                                                       00075000
      GO TO 10                                                          00076000
 2071 CALL MCE1                                                         00077000
      GO TO 10                                                          00078000
 2072 CALL MCE2                                                         00079000
      GO TO 10                                                          00079100
 2073 CALL MERGE1                                                       00079200
      GO TO 10                                                          00079300
 2074 CONTINUE                                                          00079400
      GO TO 10                                                          00079500
 2075 CALL MODA                                                         00079600
      GO TO 10                                                          00079700
 2076 CALL MODACC                                                       00079800
      GO TO 10                                                          00079900
 2077 CALL MODB                                                         00080000
      GO TO 10                                                          00081000
 2078 CALL MODC                                                         00082000
      GO TO 10                                                          00083000
 2079 CALL DMPYAD                                                       00084000
      GO TO 10                                                          00085000
 2080 CALL MTRXIN                                                       00086000
      GO TO 10                                                          00087000
 2081 CALL OFP                                                          00088000
      GO TO 10                                                          00089000
 2082 CALL OPTPR1                                                       00089100
      GO TO 10                                                          00089200
 2083 CALL OPTPR2                                                       00089300
      GO TO 10                                                          00089400
 2084 CONTINUE                                                          00089500
      GO TO 10                                                          00089600
 2085 CALL OUTPT                                                        00089700
      GO TO 10                                                          00089800
 2086 CALL OUTPT1                                                       00089900
      GO TO 10                                                          00090000
 2087 CALL OUTPT2                                                       00091000
      GO TO 10                                                          00092000
 2088 CALL OUTPT3                                                       00093000
      GO TO 10                                                          00094000
 2089 CALL OUTPT4                                                       00095000
      GO TO 10                                                          00096000
 2090 CALL QPARAM                                                       00097000
      GO TO 10                                                          00098000
 2091 CALL PARAML                                                       00099000
      GO TO 10                                                          00099100
 2092 CALL QPARMR                                                       00099200
      GO TO 10                                                          00099300
 2093 CALL PARTN1                                                       00099400
      GO TO 10                                                          00099500
 2094 CONTINUE                                                          00099600
      GO TO 10                                                          00099700
 2095 CALL MRED1                                                        00099800
      GO TO 10                                                          00099900
 2096 CALL MRED2                                                        00100000
      GO TO 10                                                          00101000
 2097 CALL CMRD2                                                        00102000
      GO TO 10                                                          00103000
 2098 CALL PLA1                                                         00104000
      GO TO 10                                                          00105000
 2099 CALL PLA2                                                         00106000
      GO TO 10                                                          00107000
 2100 CALL PLA3                                                         00108000
      GO TO 10                                                          00109000
 2101 CALL PLA4                                                         00109100
      GO TO 10                                                          00109200
 2102 CONTINUE                                                          00109300
      GO TO 10                                                          00109400
 2103 CALL DPLOT                                                        00109500
      GO TO 10                                                          00109600
 2104 CALL DPLTST                                                       00109700
      GO TO 10                                                          00109800
 2105 CALL PLTTRA                                                       00109900
      GO TO 10                                                          00110000
 2106 CALL PRTMSG                                                       00111000
      GO TO 10                                                          00112000
 2107 CALL PRTPRM                                                       00113000
      GO TO 10                                                          00114000
 2108 CALL RANDOM                                                       00115000
      GO TO 10                                                          00116000
 2109 CALL RBMG1                                                        00117000
      GO TO 10                                                          00118000
 2110 CALL RBMG2                                                        00119000
      GO TO 10                                                          00119100
 2111 CALL RBMG3                                                        00119200
      GO TO 10                                                          00119300
 2112 CALL RBMG4                                                        00119400
      GO TO 10                                                          00119500
 2113 CONTINUE                                                          00119600
      GO TO 10                                                          00119700
 2114 CALL REIG                                                         00119800
      GO TO 10                                                          00119900
 2115 CALL RMG                                                          00120000
      GO TO 10                                                          00121000
 2116 CALL SCALAR                                                       00122000
      GO TO 10                                                          00123000
 2117 CALL SCE1                                                         00124000
      GO TO 10                                                          00125000
 2118 CALL SDR1                                                         00126000
      GO TO 10                                                          00127000
 2119 CALL SDR2                                                         00128000
      GO TO 10                                                          00129000
 2120 CALL SDR3                                                         00129100
      GO TO 10                                                          00129200
 2121 CALL SDRHT                                                        00129300
      GO TO 10                                                          00129400
 2122 CALL SEEMAT                                                       00129500
      GO TO 10                                                          00129600
 2123 CONTINUE                                                          00129700
      GO TO 10                                                          00129800
 2124 CALL SETVAL                                                       00129900
      GO TO 10                                                          00130000
 2125 CALL SMA1                                                         00131000
      GO TO 10                                                          00132000
 2126 CALL SMA2                                                         00133000
      GO TO 10                                                          00134000
 2127 CALL SMA3                                                         00135000
      GO TO 10                                                          00136000
 2128 CALL SMP1                                                         00137000
      GO TO 10                                                          00138000
 2129 CALL SMP2                                                         00139000
      GO TO 10                                                          00139100
 2130 CALL SMPYAD                                                       00139200
      GO TO 10                                                          00139300
 2131 CALL SOLVE                                                        00139400
      GO TO 10                                                          00139500
 2132 CONTINUE                                                          00139600
      GO TO 10                                                          00139700
 2133 CALL SSG1                                                         00139800
      GO TO 10                                                          00139900
 2134 CALL SSG2                                                         00140000
      GO TO 10                                                          00141000
 2135 CALL SSG3                                                         00142000
      GO TO 10                                                          00143000
 2136 CALL SSG4                                                         00144000
      GO TO 10                                                          00145000
 2137 CALL SSGHT                                                        00146000
      GO TO 10                                                          00147000
 2138 CALL TA1                                                          00148000
      GO TO 10                                                          00149000
 2139 CALL TABPCH                                                       00149100
      GO TO 10                                                          00149200
 2140 CONTINUE                                                          00149300
      GO TO 10                                                          00149400
 2141 CALL TABFMT                                                       00149500
      GO TO 10                                                          00149600
 2142 CALL TABPT                                                        00149700
      GO TO 10                                                          00149800
 2143 CONTINUE                                                          00149900
      GO TO 10                                                          00150000
 2144 CALL TIMTST                                                       00151000
      GO TO 10                                                          00152000
 2145 CALL TRD                                                          00153000
      GO TO 10                                                          00154000
 2146 CALL TRHT                                                         00155000
      GO TO 10                                                          00156000
 2147 CALL TRLG                                                         00157000
      GO TO 10                                                          00158000
 2148 CALL DTRANP                                                       00159000
      GO TO 10                                                          00159100
 2149 CALL DUMERG                                                       00159200
      GO TO 10                                                          00159300
 2150 CALL DUPART                                                       00159400
      GO TO 10                                                          00159500
 2151 CALL VDR                                                          00159600
      GO TO 10                                                          00159700
 2152 CALL VEC                                                          00159800
      GO TO 10                                                          00159900
 2153 CONTINUE                                                          00160000
      GO TO 10                                                          00161000
 2154 CALL XYPLOT                                                       00162000
      GO TO 10                                                          00163000
 2155 CALL XYPRPT                                                       00164000
      GO TO 10                                                          00165000
 2156 CALL XYTRAN                                                       00166000
      GO TO 10                                                          00167000
 2157 CONTINUE                                                          00168000
      GO TO 10                                                          00169000
 2158 CALL COMB1                                                        00169100
      GO TO 10                                                          00169200
 2159 CALL COMB2                                                        00169300
      GO TO 10                                                          00169400
 2160 CALL EXIO                                                         00169500
      GO TO 10                                                          00169600
 2161 CALL RCOVR                                                        00169700
      GO TO 10                                                          00169800
 2162 CALL EMFLD                                                        00169900
      GO TO 10                                                          00170000
 2163 CONTINUE                                                          00171000
      GO TO 10                                                          00172000
 2164 CALL RCOVR3                                                       00173000
      GO TO 10                                                          00174000
 2165 CALL REDUCE                                                       00175000
      GO TO 10                                                          00176000
 2166 CALL SGEN                                                         00177000
      GO TO 10                                                          00178000
 2167 CALL SOFI                                                         00179000
      GO TO 10                                                          00179100
 2168 CALL SOFO                                                         00179200
      GO TO 10                                                          00179300
 2169 CALL SOFUT                                                        00179400
      GO TO 10                                                          00179500
 2170 CALL SUBPH1                                                       00179600
      GO TO 10                                                          00179700
 2171 CALL PLTMRG                                                       00179800
      GO TO 10                                                          00179900
 2172 CONTINUE                                                          00180000
      GO TO 10                                                          00181000
 2173 CALL COPY                                                         00182000
      GO TO 10                                                          00183000
 2174 CALL SWITCH                                                       00184000
      GO TO 10                                                          00185000
 2175 CALL MPY3                                                         00186000
      GO TO 10                                                          00187000
 2176 CALL DDCMPS                                                       00188000
      GO TO 10                                                          00189000
 2177 CALL LODAPP                                                       00189100
      GO TO 10                                                          00189200
 2178 CALL GPSTGN                                                       00189300
      GO TO 10                                                          00189400
 2179 CALL EQMCK                                                        00189500
      GO TO 10                                                          00189600
 2180 CALL ADR                                                          00189700
      GO TO 10                                                          00189800
 2181 CALL FRRD2                                                        00189900
      GO TO 10                                                          00190000
 2182 CALL GUST                                                         00191000
      GO TO 10                                                          00192000
 2183 CALL IFT                                                          00193000
      GO TO 10                                                          00194000
 2184 CALL LAMX                                                         00195000
      GO TO 10                                                          00196000
 2185 CALL EMA                                                          00197000
      GO TO 10                                                          00198000
 2186 CALL ANISOP                                                       00199000
      GO TO 10                                                          00199100
 2187 CONTINUE                                                          00199200
      GO TO 10                                                          00199300
 2188 CALL GENCOS                                                       00199400
      GO TO 10                                                          00199500
 2189 CALL DDAMAT                                                       00199600
      GO TO 10                                                          00199700
 2190 CALL DDAMPG                                                       00199800
      GO TO 10                                                          00199900
 2191 CALL NRLSUM                                                       00200000
      GO TO 10                                                          00201000
 2192 CALL GENPAR                                                       00202000
      GO TO 10                                                          00203000
 2193 CALL CASEGE                                                       00204000
      GO TO 10                                                          00205000
 2194 CALL DESVEL                                                       00206000
      GO TO 10                                                          00207000
 2195 CALL PROLAT                                                       00208000
      GO TO 10                                                          00209000
 2196 CALL MAGBDY                                                       00209100
      GO TO 10                                                          00209200
 2197 CALL COMUGV                                                       00209300
      GO TO 10                                                          00209400
 2198 CALL FLBMG                                                        00209500
      GO TO 10                                                          00209600
 2199 CALL GFSMA                                                        00209700
      GO TO 10                                                          00209800
 2200 CALL TRAIL                                                        00209900
      GO TO 10                                                          00210000
 2201 CALL SCAN                                                         00211000
      GO TO 10                                                          00212000
 2202 CONTINUE                                                          00213000
      GO TO 10                                                          00214000
 2203 CALL PTHBDY                                                       00215000
      GO TO 10                                                          00216000
 2204 CALL VARIAN                                                       00217000
      GO TO 10                                                          00218000
 2205 CALL FVRST1                                                       00219000
      GO TO 10                                                          00219100
 2206 CALL FVRST2                                                       00219200
      GO TO 10                                                          00219300
 2207 CALL ALG                                                          00219400
      GO TO 10                                                          00219500
 2208 CALL APDB                                                         00219600
      GO TO 10                                                          00219700
 2209 CALL PROMPT                                                       00219800
      GO TO 10                                                          00219900
 2210 CALL OLPLOT                                                       00220000
      GO TO 10                                                          00221000
 2211 CALL INPTT5                                                       00222000
      GO TO 10                                                          00223000
 2212 CALL OUTPT5                                                       00224000
      GO TO 10                                                          00225000
 2213 CONTINUE                                                          00226000
      GO TO 10                                                          00227000
 2214 CALL QPARMD                                                       00228000
      GO TO 10                                                          00229000
 2215 CALL GINOFL                                                       00229111
      GO TO 10                                                          00229200
 2216 CALL DBASE                                                        00229311
      GO TO 10                                                          00229411
 2217 CALL NORMAL                                                       00229511
      GO TO 10                                                          00229611
      END                                                               00230000
