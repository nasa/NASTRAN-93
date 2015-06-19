      SUBROUTINE QREAD (*,FILE,NREC,B,NB)        
C        
C     REFERENCE:        
C     VAX FORTRAN PERFORMANCE GUIDE, MAY 1990, SECTION 6.1.4        
C        
C     'READ (FILE,REC=NREC) B' IS MUCH MORE EFFICIENT THAN        
C     'READ (FILE,REC=NREC) (B(J),J=1,NB)'        
C     SIMILARLY FOR WRITE        
C        
      INTEGER    FILE,B(NB)        
C        
      READ (FILE,REC=NREC,ERR=10) B        
      RETURN        
   10 RETURN 1        
C        
C        
      ENTRY QWRITE (FILE,NREC,B,NB)        
C     =============================        
C        
      WRITE (FILE,REC=NREC) B        
      RETURN        
      END        
