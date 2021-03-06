      SUBROUTINE XRGDTB (LU)        
C        
C     XRGDTB PROCESSES THE CARD AND FILE NAME RESTART TABLES        
C     THIS SUBROUTINE IS CALLED ONLY BY XRGDFM        
C        
C     WRITTEN BY  RPK CORPORATION; DECEMBER, 1983        
C        
C     INPUT        
C       LU            FORTRAN UNIT NUMBER FOR THE RIGID FORMAT FILE     
C     /SYSTEM/        
C       OPTAPE        OUTPUT UNIT NUMBER FOR THE PRINT FILE        
C     /XRGDXX/        
C       ICHAR         ARRAY IN 80A1 FORMAT CONTAINING CARD IMAGE        
C       ISCR          FILE NUMBER ON WHICH TABLES ARE WRITTEN        
C       ITYPE         TYPE OF TABLE BEING PROCESSED-('CARD'OR'FILE')    
C       LIMIT         LOWER/UPPER LIMITS FOR VALUES IN THE TABLE        
C       RECORD        CARD IMAGE IN 20A4 FORMAT        
C        
C     OUTPUT        
C     /XRGDXX/        
C       ICOL          COLUMN WITHIN CARD BEING PROCESSED        
C       ICOUNT        NUMBER OF ALPHA CHARACTERS WITHIN A NAME        
C       IERROR        ERROR FLAG - NON-ZERO IF ERROR OCCURRED        
C       NAME          NAME OF THE SUBROUTINE        
C       NUMBER        VALUE OF NUMBER RETURNED BY XRGNUM        
C        
C     LOCAL VARIABLES        
C       ASTRSK          CONTAINS THE VALUE 1H*        
C       BLANK           CONTAINS THE VALUE 1H        
C       COMENT          CONTAINS THE VALUE OF 4H$$$$        
C       DOLLAR          CONTAINS THE VALUE OF 1H$        
C       ICOLUM          COLUMN NUMBER OF THE NEXT CHARACTER WITHIN      
C                       A NAME        
C        
C     FUNCTIONS        
C       1. CALLS READ AND XDCODE FOR EACH CARD WITHIN THE TABLE.        
C       2. CALLS XRGNUM TO PROCESS ALL NUMBERS        
C       3. CALLS XECODE TO PROCESS ALL NAMES        
C       4. ALL ENTRIES READ ARE EXPECTED TO BE IN THE FOLLOWING        
C          FORMAT:    NNNN    NAME  NAME  NAME  NAME  NAME ...        
C          WHERE NNNN IS ANY NUMBER.        
C        
C     SUBROUTINES CALLED - XRGNUM,XECODE,READ,WRITE        
C        
C     ERRORS  MESSAGES 8028,8034,8029,8036 MAY BE ISSUED        
C        
      INTEGER         RECORD, BLANK, DOLLAR, ASTRSK, OPTAPE, COMENT     
      CHARACTER       UFM*23        
      COMMON /XMSSG / UFM        
      COMMON /SYSTEM/ KSYSTM(100)        
      COMMON /XRGDXX/ IRESTR, NSUBST, IPHASE, ICOL  , NUMBER, ITYPE ,   
     1                ISTATE, IERROR, NUM(2), IND   , NUMENT        ,   
     2                RECORD(20)    , ICHAR(80)     , LIMIT(2)      ,   
     3                ICOUNT, IDMAP , ISCR  , NAME(2), MEMBER(2)    ,   
     4                IGNORE        
      EQUIVALENCE     (KSYSTM( 2),OPTAPE), (KSYSTM(39), NBPC),        
     1                (KSYSTM(40), NBPW ), (KSYSTM(41), NCPW)        
      DATA    BLANK / 1H /,  DOLLAR / 1H$ /, ASTRSK / 1H* /        
      DATA    COMENT/ 4H$$$$ /        
C        
 100  NUMBER  = 0        
      NAME(1) = 0        
C     CALL DSXREA (*710,RECORD)        
      READ (LU,150,ERR=710,END=710) RECORD        
 150  FORMAT (20A4)        
      CALL XDCODE        
      IF (RECORD(1) .EQ. COMENT) GO TO 100        
      IF (ICHAR(1).EQ.DOLLAR .AND. ICHAR(2).EQ.ASTRSK) GO TO 800        
      ICOL = 1        
 200  IF (ICHAR(ICOL).EQ.BLANK .OR. ICOL.GT.80) GO TO 500        
      IF (NUMBER .NE. 0) GO TO 300        
      CALL XRGNUM        
      IF (NUMBER .EQ. 0) GO TO 720        
      IF (NUMBER.GE.LIMIT(1) .AND. NUMBER.LE.LIMIT(2)) GO TO 200        
      GO TO 730        
 300  ICOUNT = 1        
 350  ICOLUM = ICOL + ICOUNT        
      IF (ICHAR(ICOLUM).EQ.BLANK .OR. ICOLUM.GT.80) GO TO 400        
      ICOUNT = ICOUNT + 1        
      IF (ICOUNT .LE. 8) GO TO 350        
      GO TO 740        
 400  IF (ICOUNT .EQ. 0) GO TO 350        
      CALL XECODE        
      CALL WRITE (ISCR,NAME,2,0)        
      CALL WRITE (ISCR,NUMBER,1,0)        
      ICOL = ICOL + ICOUNT        
      GO TO 200        
 500  IF (ICOL .GE. 80) GO TO 600        
      ICOL = ICOL + 1        
      GO TO 200        
 600  IF (NUMBER.EQ.0 .OR. NAME(1).EQ.0) GO TO 750        
      GO TO 100        
C        
C     ERRORS        
C        
 710  WRITE  (OPTAPE,715) UFM,MEMBER        
 715  FORMAT (A23,' 8027, UNEXPECTED EOF ENCOUNTERED ON FILE ',2A4,     
     1        ' IN SUBROUTINE XRGDTB.')        
      GO TO 770        
 720  WRITE  (OPTAPE,725) UFM,RECORD        
 725  FORMAT (A23,' 8028, EXPECTED TO FIND AN INTEGER IN THE FIRST ',   
     1        'FIELD OF THE FOLLOWING CARD', //20X,20A4)        
      GO TO 760        
 730  WRITE  (OPTAPE,735) UFM,NUMBER,RECORD,LIMIT,ITYPE        
 735  FORMAT (A23,' 8029, THE VALUE',I4,' GIVEN IN THE FIRST FIELD OF', 
     1        ' THE FOLLOWING CARD', //20X,20A4, //5X,'IS OUTSIDE THE ',
     2        'RANGE OF',I5,1H-,I4,6H FOR ',A4,8H' CARDS.)        
      GO TO 760        
 740  WRITE  (OPTAPE,745) UFM,RECORD        
 745  FORMAT (A23,' 8029, THE FOLLOWING CARD CONTAINS NAMES THAT ARE' , 
     2        'COMPRISED OF MORE THAN 8 CHARACTERS', //20X,20A4)        
      GO TO 760        
 750  WRITE  (OPTAPE,755) UFM,RECORD        
 755  FORMAT (A23,' 8036, MISSING FIELDS ON THE FOLLOWING CARD', /20X,  
     1        20A4)        
 760  IERROR = 1        
      GO TO 100        
 770  IERROR = 1        
 800  CALL WRITE (ISCR,0,0,1)        
      RETURN        
      END        
