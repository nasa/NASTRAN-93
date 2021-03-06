      SUBROUTINE UNPSCR (IN,OUT,Z,BUF1,BUF2,MAXZ,TYSIGN,FLAG)        
C        
C     THIS ROUTINE UNPACKS A MATRIX (IN), AND TRANSFER THE DATA FROM    
C     FIRST TO LAST NON-ZERO TERMS TO A SCRATCH FILE (OUT) IN VERY LARGE
C     RECORD(S), PRECEEDED BY THE FIRST AND LAST NON-ZERO TERM POINTERS.
C        
C     INPPUT  - IN, + 7 TRAILER WORDS (WORDS 4,5,6, AND 7 WILL BE       
C               OVERWRITTEN)        
C               Z, BUF1, BUF2, MAXZ, TYSIGN, AND FLAG        
C     OUTPUT  - OUT, NO TRAILER WORD WRITTEN        
C               IN(4) = 10*(NO. OF RECONDS WRITTEN, HEADER RECORD       
C                       EXCLUDED) + FLAG        
C               IN(5) = DATA WORD TYPE UNPACKED (= 1,2,OR 4)        
C               IN(6) = TOTAL NO. OF S.P. WORDS USED FOR INPUT MATRIX   
C                       IN FORWARD UNPACK PASS        
C               IN(7) = OUTPUT GINO NUMBER        
C        
C     FLAG = 1, THE MATRIX IS UNPACKED ONCE, IN FORWARD DIRECTION, THIS 
C               MATRIX CAN BE IN GENERAL FORM; NEEDS NOT BE TRIANGULAR. 
C     FLAG = 2, THE MATRIX IS UNPACKED FORWARD AND BACKWARD        
C     FLAG = 3, THE MATRIX IS ADVANCED TO THE END AND UNPACKED BACKWARD 
C               ONCE AND THEN FORWARD        
C     MAXZ = n, WHERE n IS THE UPER LIMIT OF THE RECORD SIZE TO BE      
C               WRITTEN (5000 MINIMUM).        
C          = 0  OR LESS, OUTPUT WILL BE WRITTEN OUT IN EITHER ONE OR TWO
C               LONG RECORDS (ONE EACH FOR FORWARD AND BACKWARD UNPACK) 
C     Z    =    WORKING SPACE, MINIMUM SIZE = ROW + 2 WORDS        
C     TYSIGN =  (-4,-3,...,+4), IS TYPE AND SIGN FOR INPUT MATRIX UNPACK
C               NO TYPE AND SIGN CHANGE IF TYSIGN = 0.        
C     BUF1, BUF2 = TWO GINO BUFFERS        
C     SUBROUTINE DEBUG CAN BE ACTIVATED BY DIAG 11 OR 16        
C        
C     ASSUME MATRIX IN(5x5) =  a  0  0  0  0        
C                              b  e  0  0  0        
C                              c  f  g  0  0        
C                              d  0  h  j  0        
C                              0  0  i  k  l        
C        
C     OUTPUT FILE OUT WILL HAVE THE FOLLOWING DATA (PRECEEDED BY HEADER 
C     RECORD)        
C        
C     FLAG 1 -  1 4 a b c d 2 3 e f 3 5 g h i 4 5 j k 5 5 l <EOF>       
C     FLAG 2 -  1 4 a b c d 2 3 e f 3 5 g h i 4 5 j k 5 5 l <EOR>       
C               5 5 l 4 5 j k 3 5 g h i 2 4 e f 1 4 a b d c <EOF>       
C     FLAG 3 -  5 5 l 4 5 j k 3 5 g h i 2 3 e f 1 4 a b c d <EOR>       
C               1 4 a b c d 2 3 e f 3 5 g h i 4 5 j k 5 5 l <EOF>       
C        
C     WHERE a thru l MAY BE SP, DP, CSP, OR CDP DATA        
C        
C     IF INPUT MATRIX IS VERY LARGE, THERE WILL BE SEVERAL LONG RECORDS 
C     FOR EACH UNPACK PASS, AND EACH RECORD WILL NOT EXCEED MAXZ IN     
C     LENGTH. MINIMUM OF MAXZ IS 5000. IF MAXZ IS NOT GIVEN, EACH UNPACK
C     PASS WILL GO TO ONE VERY VERY LONG RECORD. IN THIS CASE, MAXZ IS  
C     SET TO 2**31        
C        
C     THE PURPOSE OF THIS ROUTINE IS TO AVOID UNPACKING A MATRIX TOO    
C     MANY TIMES, WHILE THE MATRIX IS BEING USED REPEATEDLY.        
C     SEE FBSII (REPEATEDLY CALLED BY FBS), FRBK2 (REPEATEDLY CALLED    
C     BY FNXTVC), AND FRMLTD (REPEATED CALLED BY FRBK2 AND FNXTVC) IN   
C     USING THIS NEW DATA FORMAT.        
C        
C     WRITTEN BY G.CHAN/UNISYS   11/1991        
C        
C     COMMENTS FROM G.C.  3/93        
C     THE PRESENT UNPSCR ASSUMES THE MATRIX IS QUIT DENSE, SUCH AS THE  
C     LOWER OR UPPER TRIANGULAR FACTORS. IF MATRIX IS SPARSE, SAY 33    
C     PERCENT OF LESS, WE COULD WRITE THE MATRIX OUT ANOTHER WAY AND    
C     SAVE LOTS OF DISC SPACE. WE COULD WRITE THE FIRST TO LAST NON-ZERO
C     TERMS IN STRING FORMS SIMILAR TO OUTPUT4 MODULE. THIS IMPROVEMENT 
C     WILL BE LEFT FOR NEXT PROJECT.        
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL         FLAG23,DEBUG        
      INTEGER         IN(7),Z(3),NAM(2),TYIIJJ(4),SAVE(4)        
      CHARACTER*8     FBWD,FORWD,BACKWD        
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25        
      COMMON /XMSSG / UFM,UWM,UIM,SFM        
      COMMON /SYSTEM/ SYSBUF,NOUT        
      COMMON /UNPAKX/ TYPE,II,JJ,INCR        
      COMMON /NAMES / RD,RDREW,WRT,WRTREW,REW        
      COMMON /TYPE  / RC(2),WORDS(4)        
      EQUIVALENCE     (TYPE,TYIIJJ(1))        
      DATA    FORWD , BACKWD / 'FORWARD','BACKWARD'/        
      DATA    NAM   / 4HUNPS , 2HCR  /        
C        
      IF (FLAG.LT.1 .OR. FLAG.GT.3 .OR. IN(1).EQ.OUT) GO TO 300        
      CALL SSWTCH (11,I)        
      CALL SSWTCH (16,J)        
      DEBUG = .FALSE.        
      IF (I+J .GE. 1) DEBUG = .TRUE.        
      MAX = MAXZ        
      IF (MAX .LE. 0) MAX = 1073741824        
      IF (DEBUG) WRITE (NOUT,5) UIM        
    5 FORMAT (A29,', UNPSCR DEBUG, ACTIVATED BY DIAG 11 AND/OR 16')     
      IF (MAX .LT. 5000) GO TO 280        
      FLAG23 = FLAG.EQ.2 .OR. FLAG.EQ.3        
      DO 10 I = 1,4        
   10 SAVE(I) = TYIIJJ(I)        
      TYPE = IN(5)        
      NL   = IN(2)        
      IF (TYSIGN.NE.0 .AND. IABS(TYSIGN).LE.4) TYPE = TYSIGN        
      NWDS = WORDS(IABS(TYPE))        
      IF (DEBUG) WRITE (NOUT,15) IN(1),OUT,MAXZ,MAX,FLAG,NL,TYPE,NWDS   
   15 FORMAT (5X,'UNPSCR/@15  IN,OUT,MAXZ,MAX,FLAG,NL,TYPE,NWDS = ',    
     1        2I5,2I12,I4,I7,2I4)        
      INCR = 1        
      FORM = IN(4)        
      IF (FLAG23 .AND. FORM.NE.4 .AND. FORM.NE.5) GO TO 260        
C                          LOWER  AND      UPPER  TRIANGULAR FACTORS    
C        
      FILE = OUT        
      CALL GOPEN (OUT,Z(BUF2),WRTREW)        
      FILE = IN(1)        
      CALL OPEN (*200,IN,Z(BUF1),RDREW)        
      NREC = 0        
      IF (FLAG .EQ. 3) GO TO 90        
   20 CALL FWDREC (*210,IN)        
C        
C     UNPACK FORWARD        
C        
      FBWD = FORWD        
      TOT  = 0        
      SUM  = 0        
      DO 80 I = 1,NL        
      II   = 0        
      CALL UNPACK (*60,IN,Z(3))        
      IF (FLAG23 .AND. II.NE.I) GO TO 220        
   30 Z(1) = II        
      Z(2) = JJ        
      LL   = (JJ-II+1)*NWDS + 2        
      TOT  = TOT + LL        
      SUM  = SUM + LL        
      IF (SUM .LE. MAX) GO TO 50        
      NREC = NREC + 1        
      CALL WRITE (OUT,0,0,1)        
      SUM  = SUM - LL        
      IF (DEBUG) WRITE (NOUT,40) NREC,SUM,FBWD        
   40 FORMAT (5X,'UNPSCR WROTE RECORD',I5,',  NO. OF WORDS =',I9,2X,A8) 
      SUM  = LL        
   50 CALL WRITE (OUT,Z(1),LL,0)        
      GO TO 80        
   60 IF (FLAG23) GO TO 240        
      II   = I        
      JJ   = I        
      DO 70 K = 3,6        
   70 Z(K) = 0        
      GO TO 30        
   80 CONTINUE        
      NREC = NREC + 1        
      CALL WRITE (OUT,0,0,1)        
      IF (DEBUG) WRITE (NOUT,40) NREC,SUM,FBWD        
      IF (FLAG .NE. 2) GO TO 150        
      CALL BCKREC (IN)        
      GO TO 100        
C        
   90 CALL SKPREC (IN,NL)        
C        
C     UNPACK BACKWARD        
C        
  100 FBWD = BACKWD        
      SUM  = 0        
      I    = NL        
      DO 120 J = 1,NL        
      II   = 0        
      CALL UNPACK (*240,IN,Z(3))        
      IF (II .NE. I) GO TO 220        
      Z(1) = II        
      Z(2) = JJ        
      LL   = (JJ-II+1)*NWDS + 2        
      SUM  = SUM + LL        
      IF (SUM .LE. MAX) GO TO 110        
      NREC = NREC + 1        
      CALL WRITE (OUT,0,0,1)        
      SUM  = SUM - LL        
      IF (DEBUG) WRITE (NOUT,40) NREC,SUM,FBWD        
      SUM  = LL        
  110 CALL WRITE (OUT,Z(1),LL,0)        
      CALL BCKREC (IN)        
      CALL BCKREC (IN)        
  120 I    = I - 1        
      NREC = NREC + 1        
      CALL WRITE (OUT,0,0,1)        
      IF (DEBUG) WRITE (NOUT,40) NREC,SUM,FBWD        
      IF (FLAG .EQ. 3) GO TO 20        
C        
C     END OF UNPACKING        
C        
C     CHANGE LAST 4 WORDS OF THE INPUT MATRIX TRAILER. PARTICULARY, SET 
C     THE 7TH WORD TO NEGATIVE. NOTE, IF FLAG IS 2 OR 3, IN(4) AND IN(6)
C     TRAILER WORDS HOLD HALF OF THE ACTUAL VALUES.        
C     NOTE - SINCE WRTTRL IS NOT CALLED TO REGISTER THESE TRAILER WORD  
C     CHANGES, THE TRAILER WORDS ARE INTENDED FOR THE ROUTINE TO BE     
C     EXECUTE NEXT.  ALSO NOTE THAT OUTPUT FILE HAS NO TRAILER.        
C     LASTLY, WE NEED TO RESTORE ORIGINAL WORDS IN /UNPAKX/ PREVIOUSLY  
C     SAVED.        
C        
  150 CALL CLOSE (IN, REW)        
      CALL CLOSE (OUT,REW)        
      IN(7) =-OUT        
      IN(6) = TOT        
      IN(5) = NWDS        
      I     = NREC        
      IF (.NOT.FLAG23) GO TO 160        
      I     = NREC/2        
      TOT   = TOT*2        
  160 IN(4) = 10*I + FLAG        
      DO 170 I = 1,4        
  170 TYIIJJ(I) = SAVE(I)        
      IF (.NOT.DEBUG) GO TO 350        
      WRITE  (NOUT,180) UIM,TOT,NREC,NL,IN(3)        
  180 FORMAT (A29,1H,,I10,' S.P. WORDS MOVED TO SCRATCH FILE BY UNPSCR',
     1        /5X,'IN',I5,' RECORDS.', 5X,'INPUT MATRIX =',I8,3H BY,I7) 
      GO TO 350        
C        
  200 J = -1        
      GO TO 330        
  210 J = -2        
      GO TO 330        
  220 WRITE  (NOUT,230) SFM,I,II,JJ,FBWD,FLAG        
  230 FORMAT (A25,',  I & II MISMATCH ',3I6,3H  /,A8,I9)        
      GO TO 320        
  240 WRITE  (NOUT,250) I,FBWD,FLAG        
  250 FORMAT ('0*** NULL COLUMN ENCOUNTERED IN TRIANGULAR FACTOR.  ',   
     1        'COLUMN',I7,3X,A8,I9)        
      GO TO 320        
  260 CALL FNAME (IN(1),IN(2))        
      WRITE  (NOUT,270) IN(2),IN(3),FORM,FLAG        
  270 FORMAT ('0*** INPUT MATRTIX ',2A4,' IS NOT A TRIANGULAR FACTOR.', 
     1        '   FORM,FLAG =',2I4)        
      CALL ERRTRC ('UNPSCR  ',270)        
  280 WRITE  (NOUT,290) MAXZ        
  290 FORMAT ('0*** MAXZ ERROR ',I9,'  (TOO SMALL)')        
      CALL ERRTRC ('UNPSCR  ',290)        
      GO TO 320        
  300 WRITE  (NOUT,310) SFM,FLAG,IN(1),OUT        
  310 FORMAT (A25,',  FLAG,IN(1),OUT =',3I5)        
  320 J = -37        
  330 CALL MESAGE (J,FILE,NAM)        
C        
  350 IF (DEBUG) WRITE (NOUT,360)        
  360 FORMAT (' ... UNPSCR DEBUG ENDS',/)        
      RETURN        
      END        
