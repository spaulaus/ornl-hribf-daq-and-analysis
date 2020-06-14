C$PROG REBINIT   - Gain-shifts XBUF using TBUF for summing
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE REBINIT(MINXI,MAXXI,MINXO,MAXXO)
C
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     ------------------------------------------------------------------
      COMMON/TDX3/ XGS(4),YGS(4),IGSX,IGSY
C     ------------------------------------------------------------------
      COMMON/TDX8/ TBUF(8192)
C     ------------------------------------------------------------------
      INTEGER*4 IDAT(8192),JDAT(8192)
C
      DIMENSION XBUF(8192)
C   
      EQUIVALENCE (IDAT,IDATF(49153)),(JDAT,IDATF(57345))
C
      EQUIVALENCE (XBUF(1),IDAT(1))
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO GAIN SHIFT XBUF - USING TBUF TO SUM INTO
C     ------------------------------------------------------------------
C     XBUF - CONTAINS ARRAY TO BE GAIN-SHIFTED
C     XGS  - CONTAINS GAIN-SHIFT CONSTANTS (XI1,XI2 XF1,XF2)
C     NA   = NUMBER OF CHANNELS IN THE INPUT  ARRAY (PRE -SHIFT)
C     NB   = NUMBER OF CHANNELS IN THE OUTPUT ARRAY (POST-SHIFT)
C     ------------------------------------------------------------------
C   
      DO 10 J=1,8192                     !ZOT THE SUM BUFFER
      TBUF(J)=0.0
   10 CONTINUE
C   
      BB=(XGS(4)-XGS(3)+1.)/(XGS(2)-XGS(1)+1.) !GAIN-SHIFT COEF
      AA=XGS(3)-BB*XGS(1)                      !GAIN-SHIFT COEF
      AA=AA+BB*FLOAT(MINXI-MINXO)
      CNOR=1.0/BB                              !COUNT NORM-FACTOR
C   
C     ------------------------------------------------------------------
C     GAIN-SHIFTING PROCESS FROM HERE TO 100
C     ------------------------------------------------------------------
C   
      XX=-1.0                            !INIT INPUT CHAN-CNTR
      NA=MAXXI-MINXI+1                   !# OF INPUT  CHANNELS
      NB=MAXXO-MINXO+1                   !# OF OUTPUT CHANNELS
C   
      DO 100 IX=1,NA                     !LOOP ON INPUT CHANNELS
      XX=XX+1.0                          !INC INPUT CHAN-CNTR
      X1=AA+BB*XX                        !OUTPUT-CHAN LO-VALUE
      X2=X1+BB                           !OUTPUT-CHAN HI-VALUE
      CTS=XBUF(IX)                       !COUNTS TO ADD SOMEWHERE
      ILO=X1                             !OUTPUT-INDEX LO-VALUE
      IHI=X2+0.99                        !OUTPUT-INDEX HI-VALUE
      NBIN=IHI-ILO                       !# OUTPUT BINS INVOLVED
C   
      IF(NBIN.GT.1) GO TO 50             !TST FOR SPLIT
      IF(IHI.LT.1)  GO TO 100            !TST AGAINST LO-LIMIT
      IF(IHI.GT.NB) GO TO 110            !TST AGAINST HI-LIMIT
C   
      TBUF(IHI)=TBUF(IHI)+CTS            !ADD COUNTS TO SUM-BUFF
      GO TO 100
C   
C     ********************************** SPLIT ACROSS OUT-CHANS
C   
   50 JX=ILO                             !INIT OUTPUT BIN-INDEX
      DO 60 N=1,NBIN                     !LOOP ON OUTPUT BINS
      JX=JX+1                            !INC BIN-INDEX
C   
      IF(JX.LT.1)  GO TO 60              !TST INDEX AGAINST LO-LIMIT
      IF(JX.GT.NB) GO TO 110             !TST INDEX AGAINST HI-LIMIT
C   
      XJ=JX                              !FLOAT IT
      F=CNOR                             !COUNT NORM-FACTOR
C   
      IF(N.EQ.1)    F=CNOR*(XJ-X1)       !ADJUST FOR FIRST BIN
      IF(N.EQ.NBIN) F=CNOR*(X2-(XJ-1.0)) !ADJUST FOR LAST  BIN
C   
      TBUF(JX)=TBUF(JX)+F*CTS            !ADD COUNTS TO SUM-BUFF
C   
   60 CONTINUE
C   
  100 CONTINUE
C   
C     ------------------------------------------------------------------
C     LOAD IT BACK IN TO BUFFER FROM WHENCE IT CAME
C     ------------------------------------------------------------------
C   
  110 DO 120 I=1,NB
      XBUF(I)=TBUF(I)
  120 CONTINUE
      RETURN
      END
