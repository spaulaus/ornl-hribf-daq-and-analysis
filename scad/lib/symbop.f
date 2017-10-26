C$PROG SYMBOP    - Gets OPERs & SYMs from list and stores for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE SYMBOP
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD05/ LISI(400),SYM(3,50),OPR(50),NLS, NSY, DSPF
      CHARACTER*4                                        DSPF
      INTEGER*4    LISI,     SYM,      OPR,    NLS, NSY
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
C
      INTEGER*4    X20,X3D,X3A
      DATA         X20,X3D,X3A/'20'X,'3D'X,'3A'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C     PICKS UP OPERATORS & SYMBOLS FROM LISI & STORES IN SYM & OPR
C     ------------------------------------------------------------------
C
      NSY=0                                       !ZERO # SYMBOLS
      DSPF='FLOT'                                 !SET DISPLAY-FLG
C
      NDO=4*NLS                                   !# WDS IN LISI
      IHI=0                                       !RESET HI-BYTE #
C
      DO 20 I=1,NDO                               !LOOP ON LISI WDS
C
      CALL ILBYTE(IT,LISI,I-1)                    !GET BYTE
C
      IF(IT.NE.X20) IHI=I                         !SET IHI IF NON-BLK
C
      IF(IT.EQ.X3D) THEN                          !TST FOR =
                    CALL ISBYTE(X20,LISI,I-1)     !IF YES, ZOT IT
                    GO TO 20
                    ENDIF
C
      IF(IT.EQ.X3A) THEN                          !TST FOR :
                    CALL ISBYTE(X20,LISI,I-1)     !IF YES, ZOT IT
                    DSPF='NONE'                   !SET NO-DISPLAY FLG
                    ENDIF
C
   20 CONTINUE
C
      ILO=1                                       !INIT START BYTE #
      NS=0                                        !ZERO SYMBOL CNTR
   50 NS=NS+1                                     !INC  SYMBOL CNTR
      IF(NS.GT.MAXS) GO TO 100
      CALL NXSYM(ILO,IHI,OPR(NS),SYM(1,NS),STAT)  !GET NEXT OP & SYMB
      IF(STAT.EQ.0) GO TO 50                      !TST FOR DONE
C
      NSY=NS-1                                    !ALWAYS OVERSHOOT
      RETURN
C
  100 WRITE(6,105)
  105 FORMAT(1H ,'MORE THAN 49 SYMBOLS IN EXPRESSION - NOT ALLOWED')
      WRITE(6,110)
  110 FORMAT(1H ,'NO COMPUTED SCALERS PROVIDED')
      NERR=1
      RETURN
      END
