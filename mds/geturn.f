      SUBROUTINE GETURN (NAMDUM)        
C        
C     GETURN FINDS UNIT REFERENCE NUMBER FOR GINO FILE NAME.        
C        
      INTEGER         CORE  ,EXFIAT,FIAT  ,FIST  ,FILEX ,UNITAB,PRVOPN, 
     1                UNITS(300)   ,XYZ   ,ANDF  ,RSHIFT        
      COMMON /XXFIAT/ EXFIAT(1)        
      COMMON /XFIAT / FIAT(1)        
      COMMON /XFIST / MFIST ,NFIST ,FIST(1)        
      COMMON /GINOX / LGINOX,FILEX ,EOR   ,OP    ,ENTRY ,LSTNAM,N     , 
     1                NAME  ,NTAPE ,XYZ(2),UNITAB(75)   ,BUFADD(75)   , 
     2                NBUFF3,PRVOPN,UNITS        
C*****        
      ANDF(I,J)   = IAND(I,J)        
      LSHIFT(I,J) = ISHFT(I, J)        
      RSHIFT(I,J) = ISHFT(I,-J)        
C     WHERE         ISHFT(I,+J) IS  LEFT-SHIFT I BY J BITS, ZERO FILL   
C                   ISHFT(I,-J) IS RIGHT-SHIFT I BY J BITS, ZERO FILL   
C     AND           ISHFT IS SYSTEM ROUTINE        
C        
C UNIX:        
C     REMOVE ABOVE 3 ON-LINE FUNCTIONS IF IAND AND ISHFT SYSTEM        
C     FUNCTIONS ARE NOT AVAILABLE. ANDF AND L/RSHIFT ARE ALREADY        
C     ENTRY POINTS IN SUBROUTINE MAPFNS.        
C*****        
      NN = 2*NFIST - 1        
      DO 2001 I = 1,NN,2        
      IF (FIST(I) .EQ. NAME) GO TO 2002        
 2001 CONTINUE        
      FILEX = 0        
      RETURN        
C        
 2002 J = FIST(I+1)        
      IF (J) 2003,2004,2005        
 2003 J = -J        
 2004 I = EXFIAT(J+1)        
      GO TO 2008        
 2005 I= FIAT(J+1)        
 2008 FILEX  = ANDF(I,32767)        
      NTAPE  = ANDF(I,32768)        
      PRVOPN = RSHIFT(UNITAB(FILEX),28)        
      UNITAB(FILEX) = RSHIFT(LSHIFT(UNITAB(FILEX),4),4)        
      XYZ(1) = RSHIFT(UNITAB(FILEX),12)        
      XYZ(2) = ANDF(UNITAB(FILEX),4095)        
      IF (NAME .GE. 400) RETURN        
      UNITS(NAME-100) = FILEX        
      RETURN        
      END        
