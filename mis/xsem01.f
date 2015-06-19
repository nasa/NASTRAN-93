      SUBROUTINE XSEM01        
C        
C     FOR LINK1 DEBUG PURPOSE, PRINT OUT GOES TO UNIT 6, NOT NOUT       
C        
      IMPLICIT INTEGER (A-Z)        
      EXTERNAL        LSHIFT,RSHIFT,ANDF        
      CHARACTER*6     SUBR(11)        
      DIMENSION       WORDB(3),WORDE(5),WORDF(4),SUBNAM(2),XTRA(1)      
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25        
      COMMON /XMSSG / UFM,UWM,UIM,SFM        
      COMMON /SEM   / MASK,MASK2,MASK3,PROGMI(15)        
     1       /SYSTEM/ SYSTM(100)        
     2       /XLINK / LXLINK,MAXLNK,MXLINK(1)        
     3       /XFIST / FIST(1)        
     4       /XPFIST/ FSTRST        
     5       /OSCENT/ INOSCR(200)        
CZZ  6       /ZZXSEM/ DATABF(1)        
     6       /ZZZZZZ/ DATABF(1)        
     7       /BLANK / PARAM(60)        
     8       /XVPS  / VPS(1)        
     9       /MSGX  / NMSG        
     O       /MACHIN/ MACH        
     1       /OUTPUT/ HDG(1)        
      EQUIVALENCE     (SYSTM(1),SYSBUF),(SYSTM(2),NOUT),        
     1                (DEBUG,SYSTM(75)),(XTRA(1),HDG(1))        
      DATA   REW    / 1     /, NOREW /  0     /, POOL  /  4HPOOL /,     
     1       WORDB  / 4HSEM1,  4HBEGN , 4H    /, YCHK  /  4HXCHK /,     
     2       WORDE  / 4HBEGN,  4HEND  , 3*1H  /, EXIT  /  4HEXIT /      
     3       WORDF  / 4HK T ,  4HGPIN , 4HELID , 4H2<  /        
      DATA   THISLK / 1     /, SUBNAM/  4HXSEM,  2H01  /        
      DATA   SUBR   / 'CPUTIM','WALTIM','BTSTRP','TDATE ','DUMMY ',     
     1                'MAPFNS','CONMSG','SEMINT','POOL  ','MODX  ',     
     2                'ENDSYS'/        
C        
C     THE NEW CHANGES HERE IN NEXT FEW LINES REQUIRE        
C        
C     1. DEBUG IS SET BY SEMDBD, NASTRN, OR NAST01. DEBUG = -1,0, 1,2   
C     2. XSEM01 CALLS CPUTIME, WALTIME AND TDATE TO INITIALIZE CPU TIME,
C        WALL-CLOCK TIME, AND CURRENT DATE.        
C     3. XSEM01 CALLS MACHCK.MIS TO CHECK CERTAIN SYSTEM ROUTINES OR    
C        FUCTIONS. UNSATISFIED EXTERNALS, SUCH AS ETIME, IAND, IDATE,   
C        %LOC ETC., MAY APPEAR IN BUILDING LINK 1 EXECUTABLE        
C     4. XSEM01 CALLS MAPFNS.MDS WITH ONE ARGUMENT. MAPFNS SHOULD JUST  
C        RETURN TO XSEM01. (BY CALLING MAPFNS HERE, ALL THE MAPFNS'     
C        FUNCTION ROUTINES WILL BE LOADED INTO NASTRAN EXECUTABLES      
C        WITHOUT SPECIAL HANDLING IN BUILDING ALL THE LINKS. THIS IS    
C        PARTICLUARLY TRUE FOR IBM SINCE MAPFNS IS IN IBM 'SUPER' LINK. 
C        MAPFNS.MDS IS NOT CALLED IN XSEM02 THRU XSEM15)        
C     5. DUMMY.MDS IS CALLED TO VERIFY THAT THE CORRECT DUMMY IS IN THE 
C        NASTRAN LIBRARY        
C     6. IF THE DEBUG FLAG IS 0 OR -1, XSEM01 WILL RUN SUCCESSFULLY WITH
C        OR WITHOUT ANY UNSATISFIED EXTERNAL.        
C        
C     SAVE STARTING CPU TIME, WALL CLOCK TIME, AND DATE IN /SYSTEM/     
C        
      IF (DEBUG .GT. 0) WRITE (6,60) SUBR(1)        
   60 FORMAT (/,' -LINK1 DEBUG- XSEM01 CALLING ',A6,' NEXT')        
      CALL KLOCK  (SYSTM(18))        
      IF (DEBUG .GT. 0) WRITE (6,60) SUBR(2)        
      CALL WALTIM (SYSTM(32))        
      IF (DEBUG .GT. 0) WRITE (6,60) SUBR(3)        
      CALL BTSTRP        
      IF (DEBUG .GT. 0) WRITE (6,60) SUBR(4)        
      CALL TDATE  (SYSTM(15))        
      IF (DEBUG .LE. 0) GO TO 70        
      CALL MACHCK (*70)        
      WRITE (6,60) SUBR(5)        
   70 CALL DUMMY        
      LK = THISLK + 5        
C        
C     MAPFNS CHECK, ONLY IF MACHINE IS VAX AND UNIX        
C     (MAPFNS ON OTHER MACHINE IS IN MACHINE ASSEMBLY LANGUAGE. REMOVE  
C     THE CHECK IF MAPFNS BECOMES UNSATISFIED EXTERNAL)        
C        
      IF (DEBUG .GT. 0) WRITE (6,60) SUBR(6)        
      IF (MACH .GE. 5) J = MAPFNS(I)        
C        
C     TDATE CALL HAS BEEN MOVED TO NASTRN AND NAST01        
C        
C     CALL SEMINT TO EXECUTE PREFACE        
C        
      IF (DEBUG .GT. 0) WRITE (6,60) SUBR(7)        
      CALL CONMSG (WORDB,2,1)        
      IF (DEBUG .GT. 0) WRITE (6,60) SUBR(8)        
      CALL SEMINT (DEBUG)        
      WORDB(2) = WORDE(2)        
      WORDF(3) = KHRFN1(WORDF(3),4,WORDF(1),3)        
      WORDE(3) = KHRFN1(WORDF(2),2,WORDF(3),2)        
      CALL CONMSG (WORDB,2,1)        
      IBUF1 = KORSZ(DATABF) - SYSBUF        
      GO TO 90        
C        
C     RETURN HERE AFTER MODULE HAS EXECUTED        
C        
   80 IF (INOSCR(4) .EQ. YCHK) GO TO 90        
      WORDB(3) = WORDE(2)        
      CALL CONMSG (WORDB,3,0)        
   90 IF (NMSG  .GT. 0) CALL MSGWRT        
      IF (DEBUG .GT. 0) WRITE (6,60) SUBR(9)        
      CALL OPEN (*270,POOL,DATABF(IBUF1),2)        
C        
C     READ THE OSCAR ENTRY        
C        
  100 CALL READ (*280,*110,POOL,INOSCR,200,1,ERRFLG)        
      GO TO 290        
  110 IF (INOSCR(6)) 120,100,100        
C        
C     TRY AGAIN IF EXECUTE FLAG IS OFF        
C        
  120 CALL CLOSE (POOL,2)        
C        
C     USE LINK SPECIFICATION TABLE (MXLINK) TO DETERMINE IF MODULE      
C     RESIDES IN THIS LINK.        
C     MODX = INDEX INTO LINK SPEC TABLE        
C        
      MODX = RSHIFT(INOSCR(3),16)        
      M0DX = MODX        
      IF (PARAM(LK*8)) 130,130,150        
  130 MODX = PARAM(LK*8) + 5        
      XTRA(LK-5) =-THISLK        
      XTRA(LK-4) = XTRA(1)        
      XTRA(LK-3) = KHRFN1(WORDE(3),1,WORDF(4),2)        
      XTRA(LK-2) = KHRFN1(WORDF(1),3,WORDF(4),1)        
      XTRA(LK+3) = SYSTM(75)        
      INOSCR(LK-2) = KHRFN1(WORDF(3),2,YCHK,1)        
      INOSCR(LK+1) = INOSCR(LK-2)        
      DO 140 I = MODX,8        
  140 XTRA(I) = PARAM(I+52)        
  150 IF (DEBUG .GT. 0) WRITE (6,60) SUBR(10)        
      IF (ANDF(MXLINK(MODX),LSHIFT(1,THISLK-1)) .NE. 0) GO TO 250       
C        
C     MODULE IS NOT IN THIS LINK - DETERMINE CLOSEST LINK WHERE IT      
C     RESIDES.        
C        
      J = 2*MAXLNK + 1        
      DO 200 I = 1,MAXLNK        
      IF (ANDF(MXLINK(MODX),LSHIFT(1,I-1)) .EQ. 0) GO TO 200        
C        
C     MODULE IS IN LINK I - SEE IF LINK I IS CLOSER THAN LINK J.        
C        
      IF (MIN0(IABS(THISLK-J),IABS(THISLK-I)) .EQ. IABS(THISLK-J))      
     1    GO TO 210        
C        
C     LINK I IS CLOSER THAN J - MAKE LINK J CLOSEST LINK FOUND SO FAR.  
C        
      J = I        
  200 CONTINUE        
      I = 2*MAXLNK        
  210 IF (J .GT. MAXLNK) GO TO 950        
C        
C     CALL ENDSYS TO GET LINK J        
C        
      IF (THISLK-J .EQ. I-THISLK) J = I        
      NAME = PROGMI(J)        
      IF (DEBUG  .GT. 0) WRITE (6,60) SUBR(11)        
      IF (THISLK .EQ. J) GO TO 960        
      CALL ENDSYS (NAME,PROGMI(1))        
      RETURN        
C        
C     MODULE IS IN THIS LINK        
C     PRINT TIME MODULE BEGAN EXECUTION IF FUNCTIONAL MODULE        
C        
  250 CALL TMTOGO (KTIME)        
      IF (KTIME.LE.0 .AND. INOSCR(4).NE.EXIT)        
     1    CALL MESAGE (-50,0,INOSCR(4))        
      IF (INOSCR(4) .EQ. YCHK) GO TO 1000        
      WORDB(1) = INOSCR(4)        
      WORDB(2) = INOSCR(5)        
      WORDB(3) = WORDE(1)        
      CALL CONMSG (WORDB,3,0)        
      IDIN     = ANDF(INOSCR(6),MASK)        
      PARAM(60)= IDIN        
      IF (DEBUG .EQ. 2) WRITE (NOUT,255) THISLK,M0DX,IDIN,INOSCR(4),    
     1                                   INOSCR(5)        
  255 FORMAT (/,' <LINK',I2,1H.,I4,'.   DMAP SEQ. NO.',I4,2X,2A4,1H>)   
      GO TO 1000        
C        
C        
C     ERROR MESSAGES        
C        
C     UNEXPECTED ALTERNATE RETURN TAKEN WHILE ATTEMPTING TO OPEN POOL   
C     TAPE        
C        
  270 KODE = 270        
      GO TO  980        
C        
C     OSCAR FILE POSITIONED INCORRECTLY - HIT EOF        
C        
  280 KODE = 280        
      GO TO  980        
C        
C     OSCAR RECORD TOO LARGE FOR /OSCENT/        
C        
  290 KODE = 290        
      GO TO  980        
C        
C     LINK SPECIFICATIONS INCORRECT FOR THIS MODULE        
C        
  940 KODE = 940        
      WRITE  (6,945) WORDB,M0DX        
  945 FORMAT (/1X,3A4,I9)        
      GO TO 980        
C        
C     LOGIC ERROR AT 210        
C        
  950 KODE = 950        
      WRITE  (6,955) M0DX,MAXLNK,I,J,THISLK        
  955 FORMAT (/5X,  'MODX,MAXLNK,I,J,THISLK =',5I7)        
      GO TO  980        
C        
C     LOGIC ERROR AT 220        
C        
  960 KODE = 960        
      GO TO  980        
C        
C     LOGIC ERROR AT 1010        
C        
  970 KODE = 970        
      GO TO  980        
C        
  980 WRITE  (6,985) SFM,KODE        
  985 FORMAT (A25,' 1006, LINK DRIVER LOGIC ERROR - CODE =',I5)        
      CALL MESAGE (-37,0,SUBNAM)        
C        
C        
C     EXECUTE MODULES        
C        
  990 GO TO (2003,940,2005,2006,2007,940,2009,2010,2011,2012), I        
 1000 IF (MODX .GT. 12) GO TO 1010        
      I = MODX - 2        
      IF (I) 940,940,990        
 1010 IF (MODX-LXLINK) 940,940,970        
 2003 CALL XCHK        
      GO TO 80        
 2005 CALL XCEI        
      GO TO 80        
 2006 CALL XCEI        
      GO TO 80        
 2007 CALL XCEI        
      GO TO 80        
 2009 CALL XPURGE        
      GO TO 80        
 2010 CALL XEQUIV        
      GO TO 80        
 2011 CALL XCEI        
      GO TO 80        
 2012 CALL XCEI        
      GO TO 80        
      END        
