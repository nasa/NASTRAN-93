      SUBROUTINE T3BMGD (IERR,SHEART,IPT,IORDER,EGPDT,DGPTH,AIC,TH,     
     1                  DETJAC,SHP,BTERMS,BMATRX)        
C        
C     B-MATRIX GENERATOR ROUTINE FOR TRIA3 ELEMENTS        
C        
C     DOUBLE PRECISION ROUTINE TO GENERATE A 9XNDOF B-MATRIX AT A       
C     GIVEN INTEGRATION POINT, USING THE DERIVATIVES OF THE 2-D SHAPE   
C     FUNCTIONS.        
C     OPTIONALLY, AN 8XNDOF B-MATRIX IS CONSTRUCTED AND/OR SHEAR TERMS  
C     MAY BE DROPPED ALTOGETHER, YIELDING 6XNDOF MATRIX.        
C     FOR STRESS RECOVERY, THE EVALUATION POINTS ARE AT THE ELEMENT     
C     INTERIOR POINTS RATHER THAN ON THE EDGES.        
C     THE CONTENTS OF /TERMS/ ARE USED TO CONSTRUCT THE B-MATRIX        
C     ACCORDING TO THE BEHAVIORAL REQUIREMENTS OF THE ELEMENT.        
C        
C        
C     INPUT :        
C           IPT    - POINTER TO THE CURVILNEAR COORDINATES        
C           SHEART - LOGICAL INDICATING THE REQUIREMENT FOR OUT-OF-PLANE
C                    SHEAR TERMS        
C           IORDER - ARRAY OF INTERNAL SEQUENCE OF NODES        
C           EGPDT  - GRID POINT DATA IN THE ELEMENT COORD. SYSTEM       
C           DGPTH  - NODAL THICKNESSES        
C           AIC    - TRANSFORMATION TO RELIEVE GEOMETRY BIAS        
C     OUTPUT:        
C           IERR   - ERROR FLAG        
C           TH     - THICKNESS AT THE INTEG. PT.        
C           DETJAC - DETERMINANT OF JACOBIAN AT THE INTEG. PT.        
C           SHP    - ARRAY OF REORDERED SHAPE FUNCTIONS        
C           BTERMS - DERIVATIVES WRT THE PHYSICAL COORDINATES        
C           BMATRX - STRAIN-DISPLACEMENT RELATIONSHIP        
C        
C        
      LOGICAL          SHEART,MEMBRN,BENDNG,SHRFLX,MBCOUP,NORPTH        
      INTEGER          IORDER(3)        
      DOUBLE PRECISION EGPDT(4,1),DGPTH(1),AIC(1),TH,DETJAC,BMATRX(1),  
     1                 BTERMS(1),SHP(3),DSHPX(3),DSHPE(3),TSHP(3),      
     2                 TDSHPX(3),TDSHPE(3),VI(2),VJ(2),JACOB(4),EPS,    
     3                 PTINT(2,7),TRC(2,3),XSI,XSII,ETA,ETAI,PSI,PSII,  
     4                 DNX,DNY,SHPF        
      COMMON /TERMS /  MEMBRN,BENDNG,SHRFLX,MBCOUP,NORPTH        
      DATA    EPS   /  1.0D-13 /        
      DATA    PTINT /  0.5D0, 0.0D0, 0.5D0, 0.5D0, 0.0D0, 0.5D0,        
     1                 0.333333333333333D0, 0.333333333333333D0,        
     2                 0.166666666666667D0, 0.166666666666667D0,        
     3                 0.166666666666667D0, 0.666666666666667D0,        
     4                 0.666666666666667D0, 0.166666666666667D0/        
      DATA    TRC   /  0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 1.0D0/        
C        
C     INITIALIZE        
C        
      IERR = 0        
      NNODE= 3        
      ND1  = NNODE*6        
      ND2  = ND1*2        
      ND3  = ND1*3        
      ND4  = ND1*4        
      ND5  = ND1*5        
      ND6  = ND1*6        
      ND7  = ND1*7        
      ND8  = ND1*8        
      ND9  = ND1*9        
C        
      DO 30 I = 1,6        
      BTERMS(I) = 0.0D0        
   30 CONTINUE        
      DO 40 I = 1,ND9        
      BMATRX(I) = 0.0D0        
   40 CONTINUE        
C        
C     CALCULATE THE SHAPE FUNCTIONS AND THEIR DERIVATIVES, THEN SORT    
C     THEM.        
C        
      XSI = PTINT(1,IPT)        
      ETA = PTINT(2,IPT)        
      PSI = 1.0D0 - XSI - ETA        
C        
      DO 50 I = 1,3        
      XSII = TRC(1,I)        
      ETAI = TRC(2,I)        
      PSII = 1.0D0 - XSII - ETAI        
C        
      SHP(I)   = XSI*XSII + ETA*ETAI + PSI*PSII        
      DSHPX(I) = XSII - PSII        
      DSHPE(I) = ETAI - PSII        
   50 CONTINUE        
C        
      DO 60 I  = 1,NNODE        
      TSHP(I)  = SHP(I)        
      TDSHPX(I)= DSHPX(I)        
      TDSHPE(I)= DSHPE(I)        
   60 CONTINUE        
C        
      DO 70 I  = 1,NNODE        
      KK       = IORDER(I)        
      SHP(I)   = TSHP(KK)        
      DSHPX(I) = TDSHPX(KK)        
      DSHPE(I) = TDSHPE(KK)        
   70 CONTINUE        
C        
C     COMPUTE THE ELEMENT THICKNESS        
C        
      TH = 0.0D0        
      DO 80 ISH = 1,NNODE        
      TH = TH + SHP(ISH)*DGPTH(ISH)        
   80 CONTINUE        
C        
C     SET UP THE JACOBIAN        
C        
      DO 90 I = 1,2        
      VI(I) = 0.0D0        
      VJ(I) = 0.0D0        
      II = I + 1        
      DO 90 J = 1,NNODE        
      VI(I) = VI(I) + EGPDT(II,J)*DSHPX(J)        
      VJ(I) = VJ(I) + EGPDT(II,J)*DSHPE(J)        
   90 CONTINUE        
C        
C     INVERT THE JACOBIAN        
C        
      DETJAC = VI(1)*VJ(2) - VI(2)*VJ(1)        
      IF (DETJAC .GE. EPS) GO TO 100        
      IERR = 1        
      RETURN        
C        
  100 JACOB(1) =  VJ(2)/DETJAC        
      JACOB(2) = -VI(2)/DETJAC        
      JACOB(3) = -VJ(1)/DETJAC        
      JACOB(4) =  VI(1)/DETJAC        
C        
      DO 110 I = 1,4        
      IF (DABS(JACOB(I)) .LT. EPS) JACOB(I) = 0.0D0        
  110 CONTINUE        
C        
      IPT1 = IPT*2 - 1        
      I71  = IPT1        
      I72  = IPT1 + 1        
      I81  = IPT1 + 6        
      I82  = IPT1 + 7        
      I91  = IPT1 + 12        
      I92  = IPT1 + 13        
C        
C     LOOP OVER NODES AND BUILD PARTITIONS OF B-MATRIX        
C        
      IP = 0        
      DO 150 I = 1,NNODE        
C        
C     CALCULATE DERIVATIVES WRT THE PHYSICAL COORDINATES.        
C        
      DNX  = JACOB(1)*DSHPX(I) + JACOB(2)*DSHPE(I)        
      DNY  = JACOB(3)*DSHPX(I) + JACOB(4)*DSHPE(I)        
      SHPF = SHP(I)        
C        
      BTERMS(I      ) = DNX        
      BTERMS(I+NNODE) = DNY        
C        
      IF (.NOT.MEMBRN) GO TO 120        
C        
C     ROW 1        
C        
      BMATRX(IP+1) = DNX        
C        
C     ROW 2        
C        
      BMATRX(IP+2+ND1) = DNY        
C        
C     ROW 3        
C        
      BMATRX(IP+1+ND2) = DNY        
      BMATRX(IP+2+ND2) = DNX        
C        
  120 IF (.NOT.BENDNG) GO TO 150        
C        
C     ROW 4        
C        
      BMATRX(IP+5+ND3) = -DNX        
C        
C     ROW 5        
C        
      BMATRX(IP+4+ND4) =  DNY        
C        
C     ROW 6        
C        
      BMATRX(IP+5+ND5) = -DNY        
      BMATRX(IP+4+ND5) =  DNX        
C        
      IF (.NOT.SHEART) GO TO 150        
      IF (IPT .LT.  4) GO TO 130        
C        
C     8-ROW MATRIX        
C        
C     ROW 7        
C        
      BMATRX(IP+3+ND6) =  DNY        
      BMATRX(IP+4+ND6) = -SHPF        
C        
C     ROW 8        
C        
      BMATRX(IP+3+ND7) =  DNX        
      BMATRX(IP+5+ND7) =  SHPF        
      GO TO 150        
C        
C     9-ROW MATRIX        
C        
C     ROW 7        
C        
  130 BMATRX(IP+3+ND6) =  AIC(I71)*DNY + AIC(I72)*DNX        
      BMATRX(IP+4+ND6) = -AIC(I71)*SHPF        
      BMATRX(IP+5+ND6) =  AIC(I72)*SHPF        
C        
C     ROW 8        
C        
      BMATRX(IP+3+ND7) =  AIC(I81)*DNY + AIC(I82)*DNX        
      BMATRX(IP+4+ND7) = -AIC(I81)*SHPF        
      BMATRX(IP+5+ND7) =  AIC(I82)*SHPF        
C        
C     ROW 9        
C        
      BMATRX(IP+3+ND8) =  AIC(I91)*DNY + AIC(I92)*DNX        
      BMATRX(IP+4+ND8) = -AIC(I91)*SHPF        
      BMATRX(IP+5+ND8) =  AIC(I92)*SHPF        
C        
  150 IP = IP + 6        
C        
      RETURN        
      END        
