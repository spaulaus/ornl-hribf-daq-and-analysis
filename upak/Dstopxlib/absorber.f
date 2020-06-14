C$PROG ABSORBER  - Interprets absorber specifications
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE ABSORBER(IWD,IB,IM,IFLG)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/ABSS/    AZ(5,10),AA(5,10),INUM(5,10),IKEY(5,10),
     &                FRACT(5,10),NCOM(10),PRES(10),THCK(10),IONZ(10)
C
      REAL*4          AZ,AA,FRACT,PRES,THCK,IONZ
C
      INTEGER*4       INUM,IKEY,NCOM
C     ------------------------------------------------------------------
      COMMON/ABS2/    KEY,FRCT
      INTEGER*4       KEY
      REAL*4              FRCT
C     ------------------------------------------------------------------
      INTEGER*4       IWD(20),IB,IM,IFLG
C
      INTEGER*4       ICH(80),II,IGO
C
      REAL*4          PRS,THK,ANUM
C
      EXTERNAL        UNPACK
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IFLG=1
      NCOM(IM)=0
      PRS=0.
      THK=0.
      FRCT=1.
      KEY=1
      CALL UNPACK(IWD,ICH,IB)
      II=1
      CALL BLANK(ICH,II,IFLG)
      IF (IFLG.EQ.2) GO TO 900
   50 CALL ELEMENTX(ICH,II,IM,IGO,0)
      GO TO (500,100) IGO
  100 CALL CHARA(ICH,II,1,IGO)
      GO TO (110,50) IGO
  110 CALL CHARA(ICH,II,2,IGO)
      GO TO (120,115) IGO
  115 KEY=KEY+1
      GO TO 50
  120 CALL GAS(ICH,II,IGO,PRS,THK)
      GO TO (130,900,810) IGO
  130 CALL ELEMENTX(ICH,II,IM,IGO,0)
      GO TO (140,100) IGO
  140 CALL THICK(ICH,II,IGO,THK)
      GO TO (800,900) IGO
C
  500 CALL NUMBER(ICH,II,IGO,ANUM)
      FRCT=ANUM
      GO TO (800,510) IGO
  510 CALL CHARA(ICH,II,1,IGO)
      GO TO (800,520) IGO
  520 CALL ELEMENTX(ICH,II,IM,IGO,0)
      GO TO (600,530) IGO
  530 CALL CHARA(ICH,II,1,IGO)
      GO TO (540,520) IGO
  540 CALL CHARA(ICH,II,2,IGO)
      GO TO (560,550) IGO
  550 KEY=KEY+1
      CALL NUMBER(ICH,II,IGO,ANUM)
      FRCT=ANUM
      GO TO (520,510) IGO
  560 CALL GAS(ICH,II,IGO,PRS,THK)
      GO TO (570,900,810) IGO
  570 CALL ELEMENTX(ICH,II,IM,IGO,0)
      GO TO (580,520) IGO
  580 CALL THICK(ICH,II,IGO,THK)
      GO TO (800,900) IGO
C
  600 CALL CHARA(ICH,II,3,IGO)
      GO TO (800,610) IGO
  610 CALL ELEMENTX(ICH,II,IM,IGO,0)
      GO TO (800,620) IGO
  620 CALL CHARA(ICH,II,1,IGO)
      GO TO (630,610) IGO
  630 CALL CHARA(ICH,II,4,IGO)
      GO TO (650,540) IGO
  650 CALL ELEMENTX(ICH,II,IM,IGO,0)
      GO TO (800,620) IGO
C
  800 WRITE(6,5000)
 5000 FORMAT(' NONRECOGNIZABLE ABSORBER DEFINITION.')
      WRITE(6,5001) ICH(II)
 5001 FORMAT(' ',Z8)
      IFLG=3
      RETURN
  810 WRITE(6,5100)
 5100 FORMAT(' IMPROPER GAS PRESSURE AND/OR THICKNESS.')
      IFLG=3
      RETURN
  900 CONTINUE
      PRES(IM)=PRS
      THCK(IM)=THK
      RETURN
      END
