      BLOCK DATA ZZCORE        
CZZCORE        
C        
C     ALL ZZ-OPEN CORE LABEL COMMONS IN NASTRAN        
C        
C     THIS BLOCK DATA IS NEEDED FOR        
C        . DECstation/ULTRIX (RISC CPU)        
C        . SilconGraphics        
C        ? IBM (optional)        
C        . GRAY (UNICOS)        
C     AND IS NOT NEEDED FOR        
C        . VAX/VMS main frame AND microVAX        
C        . VAXstaion/ULTRIX        
C        . CDC (NOS AND NOS/BE)        
C        . UNIVAC (EXEC 8)        
C        
C     MAKE SURE THAT LABEL COMMON /XNSTRN/ MUST BE STORED IN THE LOWEST
C     AND /ZZZZZZ/ MUST BE IN THE HIGHEST MEMORY ADDRESSES. AFTER
C     COMPILATION, USE nm -n -d zzcore.o TO CHECK THAT IS THE CASE.
C
C     NOTE - SOME COMPILER MAY SAVE THE LABEL COMMONS IN ASCENDING AND
C     SOME IN DECENDING MEMORY ADDRESS ORDER. (NOT JUST IN ALPHA-NUMERIC
C     ORDER.) INTERCHANGE THE POSITIONS OF /XNSTRN/ AND /ZZZZZZ/ IF
C     NECCESSARY. THIS BLOCK DATA ROUTINE IS POSITION SENSITIVE.
C        
C     NASTRAN MAXIMUM OPEN CORE DIMENSION IS ALLOCATED HERE IN /ZZZZZZ/.
C     IT IS ALSO DEFINED IN DEFCOR.MDS        
C        
C     IN FUTURE PROGRAM DEVELOPMENT, ANY NEW OPEN CORE LABEL COMMON     
C     MUST ALSO SPECIFY HERE IN THIS BLOCK DATA ROUTINE.        
C        
C     WRITTEN BY G.CHAN/UNISYS, ON THE FIRST DAY OF WINTER, 1989        
C        
C     LAST REVISED 5/91 BY G.C.        
C     DUE TO DEC/ULTRIX LIMITATION, THE TOTAL NO. OF LABEL COMMONS ARE  
C     REDUCED TO BELOW 240.  THOSE REMOVED COMMONS ARE MARKED BY C* AND 
C     FOLLOWED BY THE NAMES OF SUBROUTINES THAT USE THE COMMON.        
C     (SUBROUTINE ZZKORE.MDS IS NO LONGER USED)        
C        
C        
      IMPLICIT INTEGER (A-Z)                                            
C        
      COMMON /XNSTRN/ CORE(1)        
      COMMON /SOFPTR/ PTRX(1)        
C        
      COMMON /ZZADRX/ ADRX(1)                                           
      COMMON /ZZALGX/ ALGX(1)                                           
      COMMON /ZZAMB1/ AMB1(1)                                           
      COMMON /ZZAMB2/ AMB2(1)                                           
      COMMON /ZZAMGX/ AMGX(1)                                           
C*    COMMON /ZZAMPA/ AMPA(1)    AMPA.MIS                               
      COMMON /ZZAMPC/ AMPC(1)                                           
C*    COMMON /ZZAMPD/ AMPD(1)    AMPD.MIS                               
C*    COMMON /ZZAMPE/ AMPE(1)    AMPE.MIS                               
C*    COMMON /ZZAMPF/ AMPF(1)    AMPF.MIS                               
C*    COMMON /ZZANIS/ ANIS(1)    ANISOP.MIS                             
      COMMON /ZZAPDB/ APDB(1)                                           
      COMMON /ZZAPDX/ APDX(1)                                           
      COMMON /ZZASDX/ ASDX(1)                                           
C        
      COMMON /ZZBAND/ BAND(1)                                           
      COMMON /ZZBMGX/ BMGX(1)                                           
C        
      COMMON /ZZCASE/ CASE(1)                                           
      COMMON /ZZCASG/ CASG(1)                                           
C        
      COMMON /ZZCDET/ CDET(1)                                           
      COMMON /ZZCEA1/ CEA1(1)                                           
      COMMON /ZZCEAD/ CEAD(1)                                           
      COMMON /ZZCFAC/ CFAC(1)                                           
      COMMON /ZZCFBS/ CFBS(1)                                           
      COMMON /ZZCFCN/ CFCN(1)                                           
      COMMON /ZZCFR1/ CFR1(1)                                           
      COMMON /ZZCFR2/ CFR2(1)                                           
      COMMON /ZZCFR3/ CFR3(1)                                           
      COMMON /ZZCFR4/ CFR4(1)                                           
      COMMON /ZZCINV/ CINV(1)                                           
      COMMON /ZZCMB2/ CMB2(1)                                           
      COMMON /ZZCMRD/ CMRD(1)                                           
      COMMON /ZZCNV1/ CNV1(1)                                           
      COMMON /ZZCNV2/ CNV2(1)                                           
      COMMON /ZZCNV3/ CNV3(1)                                           
      COMMON /ZZCOMB/ COMB(1)                                           
      COMMON /ZZCOMU/ COMU(1)                                           
C*    COMMON /ZZCOPY/ COPY(1)    COPY.MIS                               
      COMMON /ZZCURV/ CURV(1)                                           
C*    COMMON /ZZCYC1/ CYC1(1)    CYCT1.MIS                              
C*    COMMON /ZZCYC2/ CYC2(1)    CTCT2.MIS                              
C        
      COMMON /ZZDADD/ DADD(1)                                           
      COMMON /ZZDAMB/ DAMB(1)                                           
      COMMON /ZZDAMG/ DAMG(1)                                           
C*    COMMON /ZZDBAS/ DBAS(1)    DBASE.MIS                              
      COMMON /ZZDDC1/ DDC1(1)                                           
      COMMON /ZZDDC2/ DDC2(1)                                           
      COMMON /ZZDDC3/ DDC3(1)                                           
      COMMON /ZZDDC4/ DDC4(1)                                           
      COMMON /ZZDDC5/ DDC5(1)                                           
      COMMON /ZZDDMG/ DDMG(1)                                           
      COMMON /ZZDDMT/ DDMT(1)                                           
      COMMON /ZZDDR1/ DDR1(1)                                           
C*    COMMON /ZZDDRA/ DDRA(1)    DDR1A.MIS                              
C*    COMMON /ZZDDRB/ DDRB(1)    DDR1B.MIS                              
      COMMON /ZZDDRM/ DDRM(1)                                           
C*    COMMON /ZZDESV/ DESV(1)    DESVEL.MIS                             
      COMMON /ZZDETX/ DETX(1)                                           
C*    COMMON /ZZDFB1/ DFB1(1)    DFBS.MIS                               
C*    COMMON /ZZDFB2/ DFB2(1)    DFBS.MIS                               
      COMMON /ZZDIAG/ DIAG(1)                                           
      COMMON /ZZDLPT/ DLPT(1)                                           
      COMMON /ZZDMP1/ DMP1(1)                                           
      COMMON /ZZDMP2/ DMP2(1)                                           
      COMMON /ZZDMP3/ DMP3(1)                                           
      COMMON /ZZDPDX/ DPDX(1)                                           
      COMMON /ZZDS1A/ DS1A(1)                                           
      COMMON /ZZDS1X/ DS1X(1)                                           
      COMMON /ZZDSCH/ DSCH(1)                                           
      COMMON /ZZDSMG/ DSMG(1)                                           
      COMMON /ZZDTRA/ DTRA(1)                                           
C     COMMON /ZZDUM1/ DUM1(1)                                           
C*    COMMON /ZZDUM2/ DUM2(1)    DUMOD2.MIS                             
C*    COMMON /ZZDUM3/ DUM3(1)    DUMOD3.MIS                             
C*    COMMON /ZZDUM4/ DUM4(1)    DUMOD4.MIS                             
      COMMON /ZZDUM5/ DUM5(1)                                           
C        
      COMMON /ZZELIM/ ELIM(1)                                           
      COMMON /ZZEMA1/ EMA1(1)                                           
      COMMON /ZZEMAX/ EMAX(1)                                           
      COMMON /ZZEMFL/ EMFL(1)                                           
      COMMON /ZZEMGX/ EMGX(1)                                           
C     COMMON /ZZEM01/ EM01(1)        
C                     " 02,03,04,05,06,07,08,09,10        
C                     " 11,12,13,14,15,16,17,18,19,20        
C                     " 21,22,23,24,25,26,27,28,29,30        
C                     " 31,32,33,34,35,36,37,38,39,40        
C                     " 41,42,43,44,45,46,47        
C     COMMON /ZZEM48/ EM48(1)        
      COMMON /ZZENDS/ ENDS(1)                                           
      COMMON /ZZEQMA/ EQMA(1)                                           
      COMMON /ZZEQMS/ EQMS(1)                                           
      COMMON /ZZEXO1/ EXO1(1)                                           
      COMMON /ZZEXO2/ EXO2(1)                                           
C        
      COMMON /ZZFA1K/ FA1K(1)                                           
      COMMON /ZZFA1P/ FA1P(1)                                           
      COMMON /ZZFA1X/ FA1X(1)                                           
      COMMON /ZZFA2X/ FA2X(1)                                           
      COMMON /ZZFACT/ FACT(1)                                           
      COMMON /ZZFCTR/ FCTR(1)                                           
      COMMON /ZZFER1/ FER1(1)                                           
      COMMON /ZZFER2/ FER2(1)                                           
      COMMON /ZZFER3/ FER3(1)                                           
      COMMON /ZZFER4/ FER4(1)                                           
      COMMON /ZZFLB1/ FLB1(1)                                           
      COMMON /ZZFLB2/ FLB2(1)                                           
      COMMON /ZZFR2A/ FR2A(1)                                           
      COMMON /ZZFR2B/ FR2B(1)                                           
      COMMON /ZZFR2C/ FR2C(1)                                           
      COMMON /ZZFR2D/ FR2D(1)                                           
      COMMON /ZZFR2E/ FR2E(1)                                           
      COMMON /ZZFR2X/ FR2X(1)                                           
      COMMON /ZZFRA1/ FRA1(1)                                           
      COMMON /ZZFRB1/ FRB1(1)                                           
C*    COMMON /ZZFRC1/ FRC1(1)    FRRD1C.MIS                             
C*    COMMON /ZZFRC2/ FRC2(1)    FRRD1C.MIS                             
C*    COMMON /ZZFRC3/ FRC3(1)    FRRD1C.MIS                             
      COMMON /ZZFRD1/ FRD1(1)                                           
      COMMON /ZZFRD2/ FRD2(1)                                           
      COMMON /ZZFRF1/ FRF1(1)                                           
C*    COMMON /ZZFVR1/ FVR1(1)    FVRST1.MIS                             
C*    COMMON /ZZFVR2/ FVR2(1)    FVRST2.MIS                             
C        
      COMMON /ZZGENC/ GENC(1)                                           
      COMMON /ZZGENP/ GENP(1)                                           
C*    COMMON /ZZGFSM/ GFSM(1)    GFSMRG.MIS                             
C*    COMMON /ZZGFSP/ GFSP(1)    GFSPTN.MIS                             
      COMMON /ZZGIGG/ GIGG(1)                                           
      COMMON /ZZGIPS/ GIPS(1)                                           
      COMMON /ZZGKAD/ GKAD(1)                                           
      COMMON /ZZGKAM/ GKAM(1)                                           
C*    COMMON /ZZGNFL/ GNFL(1)    GINOFL.MIS                             
      COMMON /ZZGP1X/ GP1X(1)                                           
      COMMON /ZZGP2X/ GP2X(1)                                           
      COMMON /ZZGP3X/ GP3X(1)                                           
      COMMON /ZZGP4X/ GP4X(1)                                           
      COMMON /ZZGPCY/ GPCY(1)                                           
      COMMON /ZZGPFD/ GPFD(1)                                           
      COMMON /ZZGPST/ GPST(1)                                           
      COMMON /ZZGPWA/ GPWA(1)                                           
      COMMON /ZZGPWB/ GPWB(1)                                           
      COMMON /ZZGUS2/ GUS2(1)                                           
      COMMON /ZZGUST/ GUST(1)                                           
C        
      COMMON /ZZHES1/ HES1(1)                                           
      COMMON /ZZHES2/ HES2(1)                                           
C        
      COMMON /ZZIFP1/ IFP1(1)                                           
      COMMON /ZZIFP3/ IFP3(1)                                           
      COMMON /ZZIFP4/ IFP4(1)                                           
      COMMON /ZZIFP5/ IFP5(1)                                           
      COMMON /ZZIFPX/ IFPX(1)                                           
      COMMON /ZZIFTX/ IFTX(1)                                           
      COMMON /ZZINIT/ INIT(1)                                           
      COMMON /ZZINP1/ INP1(1)                                           
      COMMON /ZZINP2/ INP2(1)                                           
      COMMON /ZZINP3/ INP3(1)                                           
      COMMON /ZZINP4/ INP4(1)                                           
      COMMON /ZZINP5/ INP5(1)                                           
      COMMON /ZZINPT/ INPT(1)                                           
      COMMON /ZZINV1/ INV1(1)                                           
      COMMON /ZZINV2/ INV2(1)                                           
      COMMON /ZZINV3/ INV3(1)                                           
      COMMON /ZZINVP/ INVP(1)                                           
      COMMON /ZZINVT/ INVT(1)                                           
      COMMON /ZZINVV/ INVV(1)                                           
C        
      COMMON /ZZLAMX/ LAMX(1)                                           
      COMMON /ZZLODA/ LODA(1)                                           
C        
      COMMON /ZZMAGB/ MAGB(1)                                           
      COMMON /ZZMBAM/ MBAM(1)                                           
C*    COMMON /ZZMCEA/ MCEA(1)    MCE1A.MIS                              
C*    COMMON /ZZMCEB/ MCEB(1)    MCE1B.MIS                              
C*    COMMON /ZZMCEC/ MCEC(1)    MCE1C.MIS                              
C*    COMMON /ZZMCED/ MCED(1)    MCE1D.MIS                              
      COMMON /ZZMGB1/ MGB1(1)                                           
      COMMON /ZZMGB2/ MGB2(1)                                           
      COMMON /ZZMGEN/ MGEN(1)                                           
      COMMON /ZZMGPR/ MGPR(1)                                           
      COMMON /ZZMGT1/ MGT1(1)                                           
      COMMON /ZZMGT2/ MGT2(1)                                           
      COMMON /ZZMODA/ MODA(1)                                           
      COMMON /ZZMPY3/ MPY3(1)                                           
      COMMON /ZZMRD1/ MRD1(1)                                           
      COMMON /ZZMRD2/ MRD2(1)                                           
      COMMON /ZZMRGE/ MRGE(1)                                           
      COMMON /ZZMTRX/ MTRX(1)                                           
C        
      COMMON /ZZNRLS/ NRLS(1)                                           
      COMMON /ZZNRML/ NRML(1)                                           
C        
      COMMON /ZZOFPX/ OFPX(1)                                           
      COMMON /ZZOPT1/ OPT1(1)                                           
      COMMON /ZZOPT2/ OPT2(1)                                           
C*    COMMON /ZZOUT1/ OUT1(1)    OUTPT1.MIS                             
C*    COMMON /ZZOUT2/ OUT2(1)    OUTPT2.MIS                             
C*    COMMON /ZZOUT3/ OUT3(1)    OUTPT3.MIS                             
      COMMON /ZZOUT5/ OUT5(1)                                           
C        
      COMMON /ZZPARM/ PARM(1)                                           
C*    COMMON /ZZPL31/ PL31(1)    PLA31.MIS                              
C*    COMMON /ZZPL32/ PL32(1)    PLA32.MIS                              
C*    COMMON /ZZPL41/ PL41(1)    PLA41.MIS                              
C*    COMMON /ZZPL42/ PL42(1)    PLA42.MIS                              
      COMMON /ZZPLA2/ PLA2(1)                                           
      COMMON /ZZPLOT/ PLOT(1)                                           
      COMMON /ZZPLTM/ PLTM(1)                                           
      COMMON /ZZPLTR/ PLTR(1)                                           
      COMMON /ZZPMSG/ PMSG(1)                                           
      COMMON /ZZPROL/ PROL(1)                                           
      COMMON /ZZPRTI/ PRTI(1)                                           
      COMMON /ZZPSET/ PSET(1)                                           
      COMMON /ZZPSTA/ PSTA(1)                                           
      COMMON /ZZPTHB/ PTHB(1)                                           
      COMMON /ZZPTMG/ PTMG(1)                                           
C        
      COMMON /ZZRAND/ RAND(1)                                           
      COMMON /ZZRCAX/ RCAX(1)                                           
      COMMON /ZZRCBX/ RCBX(1)                                           
      COMMON /ZZRCCX/ RCCX(1)                                           
      COMMON /ZZRCEX/ RCEX(1)                                           
      COMMON /ZZRCOX/ RCOX(1)                                           
      COMMON /ZZRCV3/ RCV3(1)                                           
      COMMON /ZZREA1/ REA1(1)                                           
      COMMON /ZZREA2/ REA2(1)                                           
      COMMON /ZZREA3/ REA3(1)                                           
      COMMON /ZZREA6/ REA6(1)                                           
      COMMON /ZZREDU/ REDU(1)                                           
      COMMON /ZZREIG/ REIG(1)                                           
      COMMON /ZZRMGX/ RMGX(1)                                           
C        
      COMMON /ZZSCAL/ SCAL(1)                                           
      COMMON /ZZSCAN/ SCAN(1)                                           
      COMMON /ZZSDA1/ SDA1(1)                                           
      COMMON /ZZSDA2/ SDA2(1)                                           
      COMMON /ZZSDB1/ SDB1(1)                                           
      COMMON /ZZSDC2/ SDC2(1)                                           
      COMMON /ZZSDR2/ SDR2(1)                                           
      COMMON /ZZSDR3/ SDR3(1)                                           
      COMMON /ZZSDRH/ SDRH(1)                                           
      COMMON /ZZSEEM/ SEEM(1)                                           
      COMMON /ZZSGEN/ SGEN(1)                                           
C*    COMMON /ZZSLV1/ SLV1(1)    SOLVE.MIS                              
C*    COMMON /ZZSLV2/ SLV2(1)    SOLVE.MIS                              
C*    COMMON /ZZSLV3/ SLV3(1)    SOLVE.MIS                              
C*    COMMON /ZZSLV4/ SLV4(1)    SOLVE.MIS                              
C*    COMMON /ZZSLV5/ SLV5(1)    SOLVE.MIS                              
      COMMON /ZZSLVR/ SLVR(1)                                           
      COMMON /ZZSM3B/ SM3B(1)                                           
      COMMON /ZZSM3C/ SM3C(1)                                           
      COMMON /ZZSMA1/ SMA1(1)                                           
      COMMON /ZZSMA2/ SMA2(1)                                           
      COMMON /ZZSMA3/ SMA3(1)                                           
      COMMON /ZZSOFI/ SOFI(1)                                           
      COMMON /ZZSOFO/ SOFO(1)                                           
      COMMON /ZZSOFU/ SOFU(1)                                           
      COMMON /ZZSSA1/ SSA1(1)                                           
      COMMON /ZZSSA2/ SSA2(1)                                           
      COMMON /ZZSSA3/ SSA3(1)                                           
      COMMON /ZZSSB1/ SSB1(1)                                           
      COMMON /ZZSSB2/ SSB2(1)                                           
C                                                                       
      COMMON /ZZSSC2/ SSC2(1)                                           
C                                                                       
      COMMON /ZZSSG2/ SSG2(1)                                           
      COMMON /ZZSSGH/ SSGH(1)                                           
      COMMON /ZZSTPD/ STPD(1)                                           
      COMMON /ZZSUBP/ SUBP(1)                                           
C        
      COMMON /ZZTAA1/ TAA1(1)                                           
      COMMON /ZZTAA2/ TAA2(1)                                           
      COMMON /ZZTAC1/ TAC1(1)                                           
      COMMON /ZZTBFT/ TBFT(1)                                           
      COMMON /ZZTBPH/ TBPH(1)                                           
      COMMON /ZZTBPR/ TBPR(1)                                           
C*    COMMON /ZZTIM1/ TIM1(1)    TIMTS1.MIS                             
C*    COMMON /ZZTIM2/ TIM2(1)    TIMTS2.MIS                             
C*    COMMON /ZZTMIO/ TMIO(1)    TMTSIO.MIS                             
C*    COMMON /ZZTMLP/ TMLP(1)    TMTSLP.MIS                             
      COMMON /ZZTRAN/ TRAN(1)                                           
      COMMON /ZZTRDA/ TRDA(1)                                           
      COMMON /ZZTRDC/ TRDC(1)                                           
      COMMON /ZZTRDE/ TRDE(1)                                           
      COMMON /ZZTRDX/ TRDX(1)                                           
      COMMON /ZZTRLG/ TRLG(1)                                           
C        
      COMMON /ZZUPRT/ UPRT(1)                                           
C        
      COMMON /ZZVARI/ VARI(1)                                           
      COMMON /ZZVDRX/ VDRX(1)                                           
      COMMON /ZZVECX/ VECX(1)                                           
      COMMON /ZZVLVC/ VLVC(1)                                           
C        
      COMMON /ZZXCSA/ XCSA(1)                                           
      COMMON /ZZXGPI/ XGPI(1)                                           
      COMMON /ZZXSEM/ XSEM(1)                                           
      COMMON /ZZXSRT/ XSRT(1)                                           
      COMMON /ZZXST2/ XST2(1)                                           
      COMMON /ZZXYPL/ XYPL(1)                                           
      COMMON /ZZXYTR/ XYTR(1)                                           
C        
      COMMON /ZZZZZZ/ ZZZZ(400000)        
C        
C        
      DATA    ZZZZ(1)/ 400000/        
      DATA    CORE(1)/     0 /        
      DATA    PTRX(1)/     0 /        
C        
      DATA    ADRX(1),ALGX(1),AMB1(1),AMB2(1),AMGX(1),        
     1                AMPC(1),        
     2                APDB(1),APDX(1),ASDX(1)/   9*0 /        
C        
      DATA    BAND(1),BMGX(1)/   2*0 /        
C        
      DATA    CASE(1),CASG(1),CDET(1),CEA1(1),CEAD(1),        
     1        CFAC(1),CFBS(1),CFCN(1),CFR1(1),CFR2(1),        
     2        CFR3(1),CFR4(1),CINV(1),CMB2(1),CMRD(1),        
     3        CNV1(1),CNV2(1),CNV3(1),COMB(1),COMU(1),        
     4                CURV(1)                /  21*0 /        
C        
      DATA    DADD(1),DAMB(1),DAMG(1),        DDC1(1),        
     1        DDC2(1),DDC3(1),DDC4(1),DDC5(1),DDMG(1),        
     2        DDMT(1),DDR1(1),                DDRM(1),        
     3                DETX(1),                DIAG(1),        
     4        DLPT(1),DMP1(1),DMP2(1),DMP3(1),DPDX(1),        
     5        DS1A(1),DS1X(1),DSCH(1),DSMG(1),DTRA(1),        
     6                                        DUM5(1)/        
     7          25*0 /        
C        
      DATA    ELIM(1),EMA1(1),EMAX(1),EMFL(1),EMGX(1)/        
     1           5*0 /        
C     DATA    EM01(1)        
C    1           " 02,03,04,05,06,07,08,09,10        
C    2        " 11,12,13,14,15,16,17,18,19,20        
C    3        " 21,22,23,24,25,26,27,28,29,30        
C    4        " 31,32,33,34,35,36,37,38,39,40        
C    5        " 41,42,43,44,45,46,47        
C    6        EM48(1)/  48* 0 /        
      DATA    ENDS(1),EQMA(1),EQMS(1),EXO1(1),EXO2(1)/        
     1           5*0 /        
C        
      DATA    FA1K(1),FA1P(1),FA1X(1),FA2X(1),FACT(1),        
     1        FCTR(1),FER1(1),FER2(1),FER3(1),FER4(1),        
     2        FLB1(1),FLB2(1),FR2A(1),FR2B(1),FR2C(1),        
     3        FR2D(1),FR2E(1),FR2X(1),FRA1(1),FRB1(1),        
     4                                FRD1(1),FRD2(1),        
     5        FRF1(1)                /  23*0 /        
C        
      DATA    GENC(1),GENP(1),                GIGG(1),        
     1        GIPS(1),GKAD(1),GKAM(1),        GP1X(1),        
     2        GP2X(1),GP3X(1),GP4X(1),GPCY(1),GPFD(1),        
     3        GPST(1),GPWA(1),GPWB(1),GUS2(1),GUST(1)/        
     4          17*0 /        
C        
      DATA    HES1(1),HES2(1)/   2*0 /        
C        
      DATA    IFP1(1),IFP3(1),IFP4(1),IFP5(1),IFPX(1),        
     1        IFTX(1),INIT(1),INP1(1),INP2(1),INP3(1),        
     2        INP4(1),INP5(1),INPT(1),INV1(1),INV2(1),        
     3        INV3(1),INVP(1),INVT(1),INVV(1)/  19*0 /        
C        
      DATA    LAMX(1),LODA(1)/   2*0 /        
C        
      DATA    MAGB(1),MBAM(1),        
     1                MGB1(1),MGB2(1),MGEN(1),MGPR(1),        
     2        MGT1(1),MGT2(1),MODA(1),MPY3(1),MRD1(1),        
     3        MRD2(1),MRGE(1),MTRX(1)/  14*0 /        
C        
      DATA    NRLS(1),NRML(1)/   2*0 /        
C        
      DATA    OFPX(1),OPT1(1),OPT2(1),        
     1                OUT5(1)/   4*0 /        
C        
      DATA    PARM(1),        
     1        PLA2(1),PLOT(1),PLTM(1),PLTR(1),PMSG(1),        
     2        PROL(1),PRTI(1),PSET(1),PSTA(1),PTHB(1),        
     3        PTMG(1)/  12*0 /        
C        
      DATA    RAND(1),RCAX(1),RCBX(1),RCCX(1),RCEX(1),        
     1        RCOX(1),RCV3(1),REA1(1),REA2(1),REA3(1),        
     2        REA6(1),REDU(1),REIG(1),RMGX(1)/  14*0 /        
C        
      DATA    SCAL(1),SCAN(1),SDA1(1),SDA2(1),SDB1(1),        
     1        SDC2(1),SDR2(1),SDR3(1),SDRH(1),SEEM(1),        
     2        SGEN(1),        
     3                SLVR(1),SM3B(1),SM3C(1),SMA1(1),        
     4        SMA2(1),SMA3(1),SOFI(1),SOFO(1),SOFU(1),        
     5        SSA1(1),SSA2(1),SSA3(1),SSB1(1),SSB2(1),        
     6        SSC2(1),SSG2(1),SSGH(1),STPD(1),SUBP(1)/        
     7          30*0 /        
C        
      DATA    TAA1(1),TAA2(1),TAC1(1),TBFT(1),TBPH(1),        
     1        TBPR(1),        
     2        TRAN(1),TRDA(1),TRDC(1),TRDE(1),TRDX(1),        
     3        TRLG(1)/  12*0 /        
C        
      DATA    UPRT(1)/     0 /        
C        
      DATA    VARI(1),VDRX(1),VECX(1),VLVC(1)/   4*0 /        
C        
      DATA    XCSA(1),XGPI(1),XSEM(1),XSRT(1),XST2(1),        
     1        XYPL(1),XYTR(1)/   7*0 /        
C        
      END        
