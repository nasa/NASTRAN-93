      SUBROUTINE SOFIOF (IRW,IFILE,IBLK,BUF,BLKSIZ)        
      INTEGER BLKSIZ,BUF(BLKSIZ)        
C        
      GO TO (1,2), IRW        
    1 READ (IFILE,REC=IBLK) BUF        
      RETURN        
C        
    2 WRITE (IFILE,REC=IBLK) BUF        
      RETURN        
      END        
