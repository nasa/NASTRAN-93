      SUBROUTINE XSFADD                                                 00000100
C                                                                       00000301
CXSFABD                                                                 00000401
C                                                                       00000501
C     REVISED  8/89 BY G.C./UNISYS                                      00000601
C          1.  THE ORDER OF COMM AND XFIAT IN /XSFA1/ ARE REVERSED IN   00000702
C              THIS ROUTINE AND IN THE FOLLOWING 7 SUBROUTINES -        00000801
C              XCLEAN, XDPH, XPOLCK, XPUNP, XPURGE, XSFA AND XSOSGN.    00000901
C              ANY INCREASE IN SIZE OF XFIAT CAN THEREFORE BE MADE      00001002
C              EASILY THROUGH OUT THESE GROUP OF ROUTINES BY JUST       00002001
C              CHANGING THE XFIAT DIMENSION HERE.                       00003002
C          2.  IN THIS GROUP OF ROUTINES, THE ARRAY XFIAT IN /XSFA1/ IS 00004001
C              RENAMED TO XFIAT, NOT TO BE CONFUSED WITH THE XFIAT ARRAY00005002
C              IN /XFIAT/                                               00006001
C          3.  ENTN1 MUST EQUAL ICFIAT, THE 24TH WORD OF /SYSTEM/       00007001
C              HOWEVER, XSFA AND XPURGE ROUTINES INITIALIZE ENTN1 AGAIN 00008001
C              TO ICFIAT, JUST TO BE SURE.                              00009001
C          4.  THE DIMENSION OF XFIAT SHOULD BE 800 WHEN ENTN1 = 8, OR  00010002
C              1100 WHEN ENTN1 IS 11                                    00020001
C                                                                       00030001
      INTEGER         ALMSK,APNDMK,COMM,CURSNO,ENTN1,ENTN2,ENTN3,       00040001
     1                ENTN4,FLAG,FNX,RMSK,RXMSK,S,SCORNT,SOS,TAPMSK,    00050001
     2                THCRMK,XFIAT,ZAP                                  00060002
      COMMON /XSFA1 / MF(401),SOS(1501),COMM(20),XFIAT(1100)            00070002
      EQUIVALENCE            (COMM (1),ALMSK ),(COMM (2),APNDMK),       00080001
     1     (COMM (3),CURSNO),(COMM (4),ENTN1 ),(COMM (5),ENTN2 ),       00090001
     2     (COMM (6),ENTN3 ),(COMM (7),ENTN4 ),(COMM (8),FLAG  ),       00100001
     3     (COMM (9),FNX   ),(COMM(10),LMSK  ),(COMM(11),LXMSK ),       00110001
     4     (COMM(12),MACSFT),(COMM(13),RMSK  ),(COMM(14),RXMSK ),       00120001
     5     (COMM(15),S     ),(COMM(16),SCORNT),(COMM(17),TAPMSK),       00130001
     6     (COMM(18),THCRMK),(COMM(19),ZAP   )                          00140001
CIBMD DATA   ENTN1  , ENTN2 ,ENTN3    ,ENTN4 / 11,3,4,3/, FLAG/ 0 /     00150001
CIBMNB                                                                  00150101
      ENTN1 = 11                                                        00151001
      ENTN2 = 3                                                         00152001
      ENTN3 = 4                                                         00153001
      ENTN4 = 3                                                         00154001
      FLAG  = 0                                                         00155001
CIBMNE                                                                  00156001
CIBMD DATA   XFIAT  / 1100*0     /                                      00160002
CIBMNB                                                                  00161001
      DO 10 I = 1, 1100                                                 00162001
   10 XFIAT(I) = 0                                                      00163001
CIBMNE                                                                  00164001
CIBMR DATA   TAPMSK / 32768      /                                      00170001
      TAPMSK = 32768                                                    00171001
C            TAPMSK = O 000000100000  = Z 00008000                      00180001
CIBMR DATA   APNDMK / 1073741824 /                                      00190001
      APNDMK = 1073741824                                               00191001
C            APNDMK = O 010000000000  = Z 40000000                      00200001
CIBMR DATA   RMSK   / 32767      /                                      00210001
      RMSK   = 32767                                                    00211001
C            RMSK   = O 000000077777  = Z 00007FFF                      00220001
CIBMR DATA   RXMSK  / 65535      /                                      00230001
      RXMSK  = 65535                                                    00231001
C            RXMSK  = O 000000177777  = Z 0000FFFF                      00240001
CIBMR DATA   LMSK   / 1073676288 /                                      00250001
      LMSK   = 1073676288                                               00251001
C            LMSK   = O 007777600000  = Z 3FFF0000                      00260001
CIBMR DATA   LXMSK  / 2147418112 /                                      00270001
      LXMSK  = 2147418112                                               00271001
C            LXMSK  = O 017777600000  = Z 7FFF0000                      00280001
CIBMR DATA   SCORNT / 1073708992 /                                      00290001
      SCORNT = 1073708992                                               00291001
C            SCORNT = O 007777677700  = Z 3FFF7FC0                      00300001
CIBMR DATA   ZAP    / 32767      /                                      00310001
      ZAP    = 32767                                                    00311001
C            ZAP    = O 000000077777  = Z 00007FFF                      00320001
      END                                                               00330001
