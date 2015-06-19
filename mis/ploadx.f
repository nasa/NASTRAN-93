      SUBROUTINE PLOADX        
C        
C     PLOADX BUILDS THE PRESSURE LOADS FROM A PLOADX CARD FOR THE       
C     TRIAX6 ELEMENT        
C        
      INTEGER         SLT,IGD(1),ISLC(1),NAM(2)        
      DIMENSION       SLC(5),GD(12),P(3),PN(3)        
      COMMON /LOADX / LC,SLT        
      COMMON /SSG1AX/ Z(1)        
      COMMON /CONDSA/ PI        
      EQUIVALENCE     (SLC(1),ISLC(1),P1),(SLC(2),P3),(GD(1),IGD(1))    
      DATA    NAM   / 4HPLOA,4HDX    /        
C        
      CALL READ (*30,*40,SLT,SLC,5,0,FLAG)        
      J  = 1        
      DO 10 I = 1,3        
      CALL FNDPNT (GD(J),ISLC(I+2))        
   10 J  = J + 4        
      RL = GD(10) - GD(2)        
      ZL = GD(12) - GD(4)        
C        
C     LOADS IN NORMAL DIRECTION        
C        
      PN(1) = PI/30.*(9.0*GD( 2)*P1 + GD( 2)*P3 + GD(10)*P1 - GD(10)*P3)
      PN(2) = PI/7.5*(3.*(GD( 2)*P1 + GD(10)*P3)        
     1              + 2.*(GD( 2)*P3 + GD(10)*P1))        
      PN(3) = PI/30.*(9.0*GD(10)*P3 + GD( 2)*P3 + GD(10)*P1 - GD( 2)*P1)
C        
      J = 1        
      DO 20 I = 1,3        
      P(1) =-ZL*PN(I)        
      P(2) = 0.0        
      P(3) = RL*PN(I)        
C        
C     CONVERT TO GLOBAL IF NEEDED, AND INSERT INTO THE LOAD VECTOR      
C        
      IF (IGD(J) .NE. 0) CALL BASGLB (P,P,GD(J+1),IGD(J))        
      CALL FNDSIL (ISLC(I+2))        
      K = ISLC(I+2)        
      Z(K  ) = Z(K  ) + P(1)        
      Z(K+1) = Z(K+1) + P(2)        
      Z(K+2) = Z(K+2) + P(3)        
   20 J = J + 4        
      GO TO 60        
C        
C     ERROR MESSAGE        
C        
   30 J = -1        
      GO TO 50        
   40 J = -2        
   50 CALL MESAGE (J,SLT,NAM)        
C        
   60 RETURN        
      END        
