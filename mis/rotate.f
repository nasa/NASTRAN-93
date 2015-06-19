      SUBROUTINE ROTATE (A,ROW,ROW1,ROW2,DA, O,SIN,COS)        
C        
C     SUBROUTINE ROTAX  (O,SIN,COS)        
C        
C     THIS ROUTINE WAS CALLED ROTAX BEFORE, WITH ENTRY POINT ROTATE     
C        
C     ROTATION OF A MATRIX PARTITION.        
C     THIS ROUTINE IS CALLED ONLY BY TRIDI SUBROUTINE, WHICH IS CALLED  
C     ONLY BY VALVEC        
C        
      INTEGER          ROW,ROW1,ROW2,NAME(2)
C    1,                CHECK
      REAL             A(1)        
      DOUBLE PRECISION O(1),SIN(1),COS(1),SINE,COSINE,X,Y,Z,DA(1)       
      COMMON /GIVN  /  TITLE(100),N        
      COMMON /SYSTEM/  DUMMY(54),IPREC        
      DATA    NAME  /  4HROTA,4HTE   /        
C        
C     O     = 2ND ROW OF THE COMPLETE MATRIX.        
C     SIN   = SINES.        
C     COS   = COSINES.        
C        
C     CHECK = 123456789        
C     RETURN        
C        
C     ENTRY ROTATE (A,ROW,ROW1,ROW2,DA)        
C     =================================        
C        
C     A  = MATRIX PARTITION (TRIANGULAR) - SINGLE PRECISION        
C     DA = MATRIX PARTITION (TRIANGULAR) - DOUBLE PRECISION        
C        
C     IF (CHECK .NE. 123456789) CALL MESAGE (-37,0,NAME)        
C        
      M    = 0        
      IF (IPREC .EQ. 2) GO TO 200        
      DO 105 J = ROW1,ROW2        
      SINE = SIN(J)        
      COSINE = COS(J)        
      M    = M + 1        
      IF (SINE .EQ. 0.D0) GO TO 101        
      X    = O(ROW+1)*COSINE + O(J)*SINE        
      Y    = A(M)    * SINE  + O(J)*COSINE        
      Z    = X       *COSINE + Y   *SINE        
      O(J) = Y       *COSINE - X   *SINE        
      A(M) = O(ROW+1) + A(M) - Z        
      O(ROW+1) = Z        
  101 IF (J .EQ. N) GO TO 105        
      JP1  = J + 1        
      DO 103  I = JP1,N        
      M    = M + 1        
      X    = A(M)*COSINE - O(I)*SINE        
      O(I) = O(I)*COSINE + A(M)*SINE        
      Y    = COS(I)*O(J) + SIN(I)*X        
      A(M) = COS(I)*X    - SIN(I)*O(J)        
      O(J) = Y        
  103 CONTINUE        
  105 CONTINUE        
      RETURN        
C        
  200 DO 230 J = ROW1,ROW2        
      SINE = SIN(J)        
      COSINE = COS(J)        
      M    = M + 1        
      IF (SINE .EQ. 0.0D0) GO TO 210        
      X    = O(ROW+1)*COSINE + O(J)*SINE        
      Y    = DA(M)   *SINE   + O(J)*COSINE        
      Z    = X       *COSINE + Y   *SINE        
      O(J) = Y       *COSINE - X   *SINE        
      DA(M)= O(ROW+1)+ DA(M) - Z        
      O(ROW+1) = Z        
  210 IF (J .EQ. N) GO TO 230        
      JP1  = J + 1        
      DO 220 I = JP1,N        
      M    = M + 1        
      X    = DA(M)*COSINE - O(I)*SINE        
      O(I) = O(I)*COSINE  + DA(M)*SINE        
      Y    = COS(I)*O(J)  + SIN(I)*X        
      DA(M)= COS(I)*X     - SIN(I)*O(J)        
      O(J) = Y        
  220 CONTINUE        
  230 CONTINUE        
      RETURN        
      END        
