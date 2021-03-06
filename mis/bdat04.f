      SUBROUTINE BDAT04        
C        
C     THIS SUBROUTINE PROCESSES THE RELES BULK DATA.        
C        
      EXTERNAL        RSHIFT,ANDF        
      LOGICAL         NAME,TDAT,PRINT,PAGER        
      INTEGER         SCR2,BUF2,BUF1,RELES(2),FLAG,GEOM4,ID(2),AAA(2),  
     1                CONSET,IP(6),ICC(6),ANDF,SCBDAT,RSHIFT,IHD(96),   
     2                OUTT,IBAS(2)        
      DIMENSION       IBITS(32),JBITS(32),KBITS(32)        
      CHARACTER       UFM*23        
      COMMON /XMSSG / UFM        
CZZ   COMMON /ZZCOMB/ Z(1)        
      COMMON /ZZZZZZ/ Z(1)        
      COMMON /CMB001/ SCR1,SCR2,SCBDAT,SCSFIL,SCCONN,SCMCON,        
     1                SCTOC,GEOM4,CASECC        
      COMMON /CMB002/ BUF1,BUF2,BUF3,BUF4,BUF5,SCORE,LCORE,INPT,OUTT    
      COMMON /CMB003/ COMBO(7,5),CONSET,IAUTO,TOLER,NPSUB,CONECT,TRAN,  
     1                MCON,RESTCT(7,7),ISORT,ORIGIN(7,3),IPRINT        
      COMMON /CMB004/ TDAT(6)        
      COMMON /CMBFND/ INAM(2),IERR        
      COMMON /OUTPUT/ ITITL(96),IHEAD(96)        
      COMMON /SYSTEM/ XXX,IOT,JUNK(6),IPAGE,LINE,ITLINE,MAXLIN,IDAT(3)  
      COMMON /BLANK / STEP,IDRY        
      DATA    IHD   / 11*4H    ,4H  SU,4HMMAR,4HY OF,4H PRO,4HCESS,     
     1                4HED R,4HELES,4H BUL,4HK DA,4HTA  ,18*4H    ,     
     2                4H   B,4HASIC,2*4H    ,4H GRI,4HD   ,4H     ,     
     3                4HREQU,4HESTE,4HD   ,4H  IN,4HTERN,  4HAL   ,     
     4                4H   C,4HURRE,4HNT  ,4H  DO,4HF TO,  4H BE  ,     
     5                13*4H    ,4HSUBS,4HTRUC,4HTURE,4H   P,4HOINT,     
     6                4H ID ,4H    ,4H REL,4HEASE,4H    ,4H  PO,4HINT , 
     7                4HNO. ,4H    ,4H DOF,4H    ,4H   R,4HELEA,4HSED , 
     8                6*4H       /        
      DATA    RELES / 410,4      / , AAA / 4HBDAT,4H04    /        
C        
      DO 10 I = 1,96        
      IHEAD(I) = IHD(I)        
   10 CONTINUE        
      PAGER = .TRUE.        
      PRINT = .FALSE.        
      IF (ANDF(RSHIFT(IPRINT,8),1) .EQ. 1) PRINT = .TRUE.        
      IFILE = SCBDAT        
      CALL OPEN (*200,SCBDAT,Z(BUF2),0)        
      CALL SKPFIL (SCBDAT,3)        
      CALL CLOSE  (SCBDAT,2)        
      CALL OPEN (*200,SCBDAT,Z(BUF2),3)        
      IFILE = SCR2        
      CALL LOCATE (*170,Z(BUF1),RELES,FLAG)        
      IFILE = GEOM4        
   20 CALL READ (*210,*160,GEOM4,ID,1,0,N)        
      IF (ID(1) .EQ. CONSET) GO TO 40        
   30 CALL READ (*210,*220,GEOM4,ID,2,0,N)        
      IF (ID(1)+ID(2) .NE. -2) GO TO 30        
      GO TO 20        
   40 NAME = .TRUE.        
      IF (PAGER .AND. PRINT) CALL PAGE        
      PAGER = .FALSE.        
      TDAT(4) = .TRUE.        
   50 CALL READ (*210,*220,GEOM4,ID,2,0,N)        
      IF (ID(1)+ID(2) .NE. -2) GO TO 60        
      CALL WRITE (SCBDAT,ID,0,1)        
      GO TO 20        
   60 IF (.NOT.NAME) GO TO 100        
      CALL FINDER (ID,IS,IC)        
      IBAS(1) = ID(1)        
      IBAS(2) = ID(2)        
      IF (IERR .NE. 1) GO TO 90        
      WRITE  (OUTT,70) UFM,(ID(K),K=1,2)        
   70 FORMAT (A23,' 6517, THE BASIC SUBSTRUCTURE  ',2A4, /30X,        
     1       'REFERED TO BY A RELES  BULK DATA CARD CAN NOT BE FOUND ', 
     2       'IN THE PROBLEM TABLE OF CONTENTS.')        
      IDRY = -2        
   80 CALL READ (*210,*220,GEOM4,ID,2,0,N)        
      IF (ID(1)+ID(2) .NE. -2) GO TO 80        
      GO TO 20        
   90 CONTINUE        
      CALL WRITE (SCBDAT,IS,1,0)        
      NAME = .NOT.NAME        
      GO TO 50        
  100 CALL FNDGRD (IS,IC,ID(1),IP,ICC,N)        
      IF (IERR .NE. 1) GO TO 120        
      WRITE  (OUTT,110) UFM,ID(1),INAM        
  110 FORMAT (A23,' 6515, GRID POINT',I10,' BASIC SUBSTRUCTURE ',2A4,   
     1       ' DOES NOT EXIST.')        
      IDRY = -2        
      GO TO 50        
  120 CALL ENCODE (ID(2))        
      CALL BITPAT (ID(2),IBITS)        
      DO 150 I = 1,N        
      ICCC = ANDF(ID(2),ICC(I))        
      CALL BITPAT (ICCC,JBITS)        
      ICC(I) = ANDF(ICC(I),63)        
      CALL BITPAT (ICC(I),KBITS)        
      IF (ICCC .EQ. 0) GO TO 150        
      IF (.NOT.PRINT ) GO TO 140        
      WRITE (OUTT,130) IBAS,ID(1),IBITS(1),IBITS(2),IP(I),KBITS(1),     
     1                 KBITS(2),JBITS(1),JBITS(2)        
  130 FORMAT (35X,2A4,5X,I8,7X,A4,A2,6X,I8,6X,A4,A2,6X,A4,A2)        
  140 CONTINUE        
      CALL WRITE (SCBDAT,IP(I),1,0)        
      CALL WRITE (SCBDAT,ICCC, 1,0)        
  150 CONTINUE        
      GO TO 50        
  160 CONTINUE        
  170 CALL CLOSE (SCBDAT,1)        
      RETURN        
C        
  200 IMSG = -1        
      GO TO 230        
  210 IMSG = -2        
      GO TO 230        
  220 IMSG = -3        
  230 CALL MESAGE (IMSG,IFILE,AAA)        
      RETURN        
      END        
