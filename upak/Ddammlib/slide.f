C$PROG SLIDE     - Slides XBUF using TBUF to slide into - for SHIFT
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SLIDE(MINXI,MAXXI,MINXO,MAXXO)
C
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
C     ------------------------------------------------------------------
      COMMON/TDX8/ TBUF(8192)
C     ------------------------------------------------------------------
      INTEGER*4    IDAT(8192),JDAT(8192)
C
      DIMENSION    XBUF(8192)
C   
      EQUIVALENCE (IDAT,IDATF(49153)),(JDAT,IDATF(57345))
C
      EQUIVALENCE (XBUF(1),IDAT(1))
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO SLIDE XBUF - USING TBUF TO SLIDE INTO
C     ------------------------------------------------------------------
C     XBUF - CONTAINS ARRAY TO BE SLID
C     NA   = NUMBER OF CHANNELS IN THE INPUT  ARRAY (PRE -SLIDE)
C     NB   = NUMBER OF CHANNELS IN THE OUTPUT ARRAY (POST-SLIDE)
C     ------------------------------------------------------------------
C   
      IF(MINXI.EQ.MINXO) RETURN
C   
      DO 10 J=1,8192                     !ZOT THE SUM BUFFER
      TBUF(J)=0.0
   10 CONTINUE
C   
      IOF=MINXI-MINXO                    !OFFSET (# CHANS TO SLIDE)
      NA =MAXXI-MINXI+1                  !# OF INPUT  CHANNELS
      NB =MAXXO-MINXO+1                  !# OF OUTPUT CHANNELS
C   
C     ------------------------------------------------------------------
C     SLIDING PROCESS FROM HERE TO 100
C     ------------------------------------------------------------------
C   
      DO 100 IX=1,NA                     !LOOP ON INPUT CHANNELS
C   
      JX=IX+IOF                          !OUTPUT INDEX
      IF(JX.LT.1)  GO TO 100             !TST LO-LIMIT
      IF(JX.GT.NB) GO TO 110             !TST HI-LIMIT
C   
      TBUF(JX)=XBUF(IX)                  !LOAD INTO TBUF
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
