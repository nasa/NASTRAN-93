      SUBROUTINE TMTOGO (TOGO)        
C        
C     TO COMPUTE THE TIME (IN SECONDS) REMAINING        
C        
      INTEGER TBEGIN, TPROB,TNOW,TOGO        
      COMMON /SYSTEM/ XSYS(17),TBEGIN        
      COMMON /STIME / TPROB        
C        
C     GET PRESENT TIME        
C        
      CALL KLOCK (TNOW)        
C        
C     COMPUTE TIME TO GO        
C        
      TOGO = TPROB - (TNOW - TBEGIN)        
      RETURN        
      END        
