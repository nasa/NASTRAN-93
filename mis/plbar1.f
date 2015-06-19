      SUBROUTINE PLBAR1 (IDO,LCORE)        
C        
C     THIS SUBROUTINE SETS UP THE DATA NEEDED TO CALL PLOAD1        
C     TO GET THE APPLIED CONCENTRATED, UNIFORMLY OR LINEARLY DISTRIBUTED
C     LOADS, ON A BAR ELEMENT FROM A PLOAD1 CARD        
C     AND INSERTS THE VECTOR INO PV        
C        
      INTEGER         BAR,NAM(2),OLDID,EST,IZ(1),ISLT(7)        
      DIMENSION       TA(9),TB(9),PA(6),PB(6),PG(42)        
      COMMON /SYSTEM/ IBUFF,NOUT        
      COMMON /SSGA1X/ PV(1)        
      COMMON /LOADX / LC,SLT,D1(5),EST,D2(11),ILID        
      COMMON /MATIN / MATID,INFLAG,TEMP        
      EQUIVALENCE     (PG(1),IZ(1))        
      DATA    NAM   / 4HPLBA,4HR1  /, N,OLDID,ISLT / 9*0 /        
      DATA    IECT  , IEPT,IBG,NWDS,BAR / 1,16,34,42,34  /        
C        
C     INITIALIZE AND OPEN EST        
C        
      IF (N .NE. 0) GO TO 30        
      CALL GOPEN (EST,IZ(LCORE),0)        
   10 CALL READ (*110,*20,EST,I,1,0,FLAG)        
   20 IF (I .EQ. BAR) GO TO 30        
      CALL FWDREC (*110,EST)        
      GO TO 10        
C        
C     READ SLT THEN FIND BAR ELEMENT        
C        
   30 CALL READ (*100,*100,SLT,ISLT,7,0,FLAG)        
      IF (ISLT(1) .EQ. OLDID) GO TO 60        
   40 CALL READ (*110,*110,EST,IZ(IECT),NWDS,0,FLAG)        
      OLDID = IZ(IECT)        
      IF (IZ(IECT)-ISLT(1)) 40,50,110        
C        
C     CONVERT COORD. SYSTEMS        
C        
   50 IF (IZ(IECT+6) .NE. 0) CALL GLBBAS (PG(IECT+ 3),PG(IECT+ 3),      
     1                                    PG(IBG + 1),IZ(IECT+ 6))      
      IF (IZ(IBG   ) .NE. 0) CALL GLBBAS (PG(IECT+ 9),PG(IECT+ 9),      
     1                                    PG(IBG + 1),IZ(IBG    ))      
      IF (IZ(IBG +4) .NE. 0) CALL GLBBAS (PG(IECT+12),PG(IECT+12),      
     1                                    PG(IBG + 5),IZ(IBG + 4))      
      CALL GBTRAN (IZ(IBG  ),IZ(IBG+1),TA)        
      CALL GBTRAN (IZ(IBG+4),IZ(IBG+5),TB)        
C        
C     DATA READY        
C        
      INFLAG = 1        
      TEMP   = PG(IBG+8)        
      MATID  = IZ(IEPT)        
      CALL MAT (OLDID)        
   60 CALL PLOAD1 (1,ISLT,PG(IECT+3),PG(IECT+9),PG(IECT+12),PG(IBG+1),  
     1             PG(IBG+5),PA,PB,TA,TB,ISLT,IZ(IECT))        
C        
C     INSERT INTO PV        
C        
      IPG = IZ(IECT+1) - 1        
      DO 70 I = 1,6        
   70 PV(IPG+I) = PV(IPG+I) + PA(I)        
      IPG = IZ(IECT+2) - 1        
      DO 80 I = 1,6        
   80 PV(IPG+I) = PV(IPG+I) + PB(I)        
      N = N + 1        
      IF (N .NE. IDO) GO TO 150        
      N = 0        
      OLDID = 0        
      CALL CLOSE (EST,1)        
      GO TO 150        
C        
C     ERROR        
C        
  100 CALL MESAGE (-1,SLT,NAM)        
  110 WRITE  (NOUT,120) ISLT(1),ILID        
  120 FORMAT ('0*** USER FATAL MESSAGE 2286, CBAR ELEMENT',I9,        
     1        ' REFERENCED ON PLOAD1',I9,' NOT FOUND')        
      CALL MESAGE (-61,0,NAM)        
C        
  150 RETURN        
      END        
