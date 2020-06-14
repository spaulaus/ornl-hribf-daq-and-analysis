C$PROG WIDTH     - Estimate peak-width for GFITT
C
C     ******************************************************************
C     From M.L. Halbert
C     ******************************************************************
C
      SUBROUTINE WIDTH(P,YO,X,NO,NP,W)
C
C     ------------------------------------------------------------------
C     FOR AUTOMATIC PEAK-WIDTH ESTIMATE, FROM LIZASTUP.DEC
C     MLH    11-15-83
C     ------------------------------------------------------------------
C
      DIMENSION P(18),YO(2048),X(2048)
C
C     ------------------------------------------------------------------
C     ESTIMATE WIDTH.  FIRST FIND LARGEST PEAK
C     ------------------------------------------------------------------
C
      KM=P(1)
      XX=X(KM)
      YM=YO(KM)-(P(16)+XX*P(17))
      IF(NP-1) 530,530,501
  501 DO 520 I=2,NP
      K=P(I)
      XX=X(K)
      YNET=YO(K)-(P(16)+XX*P(17))
      IF(YNET-YM) 520,520,510
  510 KM=P(I)
      YM=YNET
  520 CONTINUE
C
C     ------------------------------------------------------------------
C     NOW LOOK FOR FIRST CHANNEL ON EITHER SIDE SMALLER
C     THAN 1/2 OF PEAK VALUE
C     ------------------------------------------------------------------
C
  530 YM=0.5*YM
      J=0
  535 IFLAG=0
      J=J+1
      JK=KM-J
      IF(JK-1) 539,537,537
  537 XX=X(JK)
      IF(YO(JK)-(P(16)+XX*P(17))-YM) 550,550,540
  539 IFLAG=IFLAG+1
  540 JK=KM+J
      IF(JK-NO) 545,545,580
  545 XX=X(JK)
      IF(YO(JK)-(P(16)+XX*P(17))-YM) 550,550,535
  580 IFLAG=IFLAG+1
      IF(IFLAG-2) 535,585,585
C
C     ------------------------------------------------------------------
C     HAVE GONE OUTSIDE LIMITS 1,NO WITHOUT FINDING W
C     SET WIDTH < 0 AS A FLAG TO CALLING PROGRAM
C     ------------------------------------------------------------------
C
  585 W=-1.0
      GO TO 551
    
  550 W=2*J
  551 RETURN
      END
