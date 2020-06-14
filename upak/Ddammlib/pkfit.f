C$PROG PKFIT     - Set components, normal equations & does fit
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PKFIT
C
C     ------------------------------------------------------------------
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C
      COMMON/SM02/COMP(2048,44),YCAL(2048),BGD(2048),DATA(2048),WT(2048)
C
      COMMON/SM03/ XP(4,44),XPSAV(4,44),IPO(176),JPO(176)
C
      COMMON/SM04/ KXF(4,44),KFUN(44),BETA(50),IVF(4)
C
      COMMON/SM28/ RELI(44),NREL,NPKK,KRELF
C     ------------------------------------------------------------------
      COMMON/MAINV8/ A(50,50),B(50,50),DET,IFS
      REAL*8         A,       B,       DET
C     ------------------------------------------------------------------
C   
      DIMENSION COMPL(90112),COMPX(2048)
C   
      EQUIVALENCE (COMPL,COMP)
C   
      DATA NR,NP/1,4/
C
      SAVE
C
C   
C     ==================================================================
C     SET UP COMPONENTS CORRESPONDING TO ALL PEAKS
C     ------------------------------------------------------------------
C   
      IF(NREL.LE.1) GO TO 40
C
      DO 10 I=I1,I2
      COMP(I,1)=0.0
   10 CONTINUE
C
      KP=1
      DO 20 JP=1,NPK
      IF(RELI(JP).LE.0.0) GO TO 20
      NR=KFUN(JP)+1
      CALL SFUNK(NR,NP,I1,I2,XP(1,JP),COMPX)
      DO 15 J=I1,I2
      COMP(J,KP)=COMP(J,KP)+RELI(JP)*COMPX(J)
   15 CONTINUE
   20 CONTINUE
C
      DO 30 JP=1,NPK
      IF(RELI(JP).GT.0.0) GO TO 30
      NR=KFUN(JP)+1
      KP=KP+1
      CALL SFUNK(NR,NP,I1,I2,XP(1,JP),COMP(1,KP))
   30 CONTINUE
      GO TO 70
C   
   40 DO 50 JP=1,NPK
      NR=KFUN(JP)+1
      CALL SFUNK(NR,NP,I1,I2,XP(1,JP),COMP(1,JP))
   50 CONTINUE
C   
C     ------------------------------------------------------------------
C     CALCULATE NORMAL EQUATIONS FOR DMAINV8
C     ------------------------------------------------------------------
C   
   70 DO 100 N=1,NCOMP
      IOF=2048*(N-1)
      SUM=0.0
      DO 80 I=I1,I2
      NDX=IOF+I
      IF(COMPL(NDX).EQ.0.0) GO TO 80
      SUM=SUM+WT(I)*DATA(I)*COMPL(NDX)
   80 CONTINUE
  100 B(N,1)=SUM
      DO 160 I=1,NCOMP
      IOF=2048*(I-1)
      DO 140 J=1,I
      JOF=2048*(J-1)
      SUM=0.0
      DO 120 K=I1,I2
      IDX=K+IOF
      JDX=K+JOF
      IF(COMPL(IDX).EQ.0.0.OR.COMPL(JDX).EQ.0.0) GO TO 120
      SUM=SUM+WT(K)*COMPL(IDX)*COMPL(JDX)
  120 CONTINUE
      A(I,J)=SUM
      A(J,I)=SUM
  140 CONTINUE
  160 CONTINUE
C   
C     ------------------------------------------------------------------
C     INVERT THE MOTHER
C     ------------------------------------------------------------------
C   
      CALL DMAINV8(NCOMP,1)
      DO 180 I=1,NCOMP
      BETA(I)=B(I,1)
  180 CONTINUE
C   
C     ------------------------------------------------------------------
C     CALCULATE BGD AND YCAL
C     ------------------------------------------------------------------
C   
      IF(IFBGD.EQ.1) GO TO 240
      J1=NPKK+1
      DO 220 I=I1,I2
      SUM=0.0
      DO 200 J=J1,NCOMP
      SUM=SUM+COMP(I,J)*BETA(J)
  200 CONTINUE
  220 BGD(I)=SUM
  240 DO 280 I=I1,I2
      SUM=0.0
      DO 260 J=1,NCOMP
      SUM=SUM+COMP(I,J)*BETA(J)
  260 CONTINUE
      YCAL(I)=SUM
  280 CONTINUE
C   
C     ------------------------------------------------------------------
C     CALCULATE QFN
C     ------------------------------------------------------------------
C   
      SUM=0.0
      DO 300 I=I1,I2
      SUM=SUM+WT(I)*(YCAL(I)-DATA(I))**2
  300 CONTINUE
      DENO=I2-I1-NCOMP+1
      IF(DENO.LT.1.0) DENO=1.0
      QFN=SUM/DENO
      RETURN
      END
