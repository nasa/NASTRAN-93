      SUBROUTINE OPEN (*,NAME,BUFF,OP)        
C        
C     OPEN IS AN INTERMEDIARY TO ENTRY POINT QOPEN IN SUBROUTINE GINO.  
C     THE MAIN TASK OF OPEN IS TO INSURE THAT DATA BLOCKS WHICH WERE    
C     WRITTEN AND CLOSED OFF THE LOAD POINT HAVE AN END-OF-FILE BEFORE  
C     THEY ARE READ.        
C        
      INTEGER         BUFF(1),OP,XOP,XNAME,FILEX,RW,ENTRY,UNITAB,BUFADD 
      COMMON /GINOX / LGINOX,FILEX,EOR,XOP,ENTRY,LSTNAM,N,XNAME,        
     1                NTAPE,NBLOCK,NLR,UNITAB(75),BUFADD(75),NBUFF3,RW  
      COMMON /SYSTEM/ IBUF,NOUT        
C        
C     TEST FOR CONDITION IN WHICH END-OF-FILE IS TO BE WRITTEN        
C        
      XNAME = NAME        
      FILEX = 0        
      CALL GETURN (XNAME)        
C     IMHERE = 10        
      IF (FILEX .EQ. 0) GO TO 90        
      IF (OP.EQ.1 .OR. OP.EQ.3) GO TO 40        
      IF (NBLOCK+NLR .NE. 1) GO TO 20        
   10 IF (OP+2) 40,50,40        
   20 IF (RW .EQ. 0) GO TO 10        
C        
C     DATA BLOCK WAS PREVIOUSLY OPENED TO WRITE AND IS NOW OFF LOAD     
C     POINT.  WRITE AN END-OF-FILE. IF SPECIAL CALL, RETURN        
C        
      IMHERE = 20        
      CALL QOPEN (*60,NAME,BUFF,3)        
      CALL EOF (NAME)        
C        
C     NO NEED TO SET XOP INVOLVED HERE        
C        
      IF (OP .NE. -2) GO TO 30        
      CALL CLOSE (NAME,1)        
      GO TO 50        
   30 CALL CLOSE (NAME,2)        
C        
C     NOW OPEN ACCORDING TO OP. IF NECESSARY, POSITION PRIOR TO EOF     
C        
      CALL GETURN (NAME)        
      IMHERE = 30        
      CALL QOPEN (*60,NAME,BUFF,OP)        
      IF (OP .EQ. 2) CALL BCKREC (NAME)        
      GO TO 50        
C        
C     NORMAL OPEN CALL        
C        
   40 IMHERE = 40        
      CALL QOPEN (*60,NAME,BUFF,OP)        
   50 RETURN        
C        
C     FILE DOES NOT EXIST IN FIST.        
C     GIVE ERROR TRACEBACK IF DIAG 1 IS ON        
C        
   60 CALL SSWTCH (1,L)        
      IF (L .NE. 1) GO TO 90        
      WRITE  (NOUT,70) NAME,IMHERE,OP        
   70 FORMAT (//,' *** SUBROUTINE OPEN CAN NOT OPEN GINO FILE',I5,      
     1       '.  IMHERE,OP =',2I4)        
      CALL FNAME (NAME,UNITAB(71))        
      WRITE  (NOUT,80) UNITAB(71),UNITAB(72)        
   80 FORMAT (5X,'NAME OF FILE = ',2A4)        
      IF (NAME .NE. 0) CALL ERRTRC ('OPEN    ',70)        
   90 RETURN 1        
      END        
