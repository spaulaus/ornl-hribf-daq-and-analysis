C$PROG SYMBOP
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE SYMBOP
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SCATA/ MAXT,MAXS,SEC,ISET,NERR,MODEGO
      CHARACTER*4                 ISET,     MODEGO
      INTEGER*4     MAXT,MAXS,SEC,     NERR
C     ------------------------------------------------------------------
      COMMON/SCAT4/ LISI(400),SYM(3,50),OPR(50),NLS,NSY,DSPF
      CHARACTER*4                                       DSPF
C     ------------------------------------------------------------------
      INTEGER*4     X20,X3D,X3A
      DATA          X20,X3D,X3A/'20'X,'3D'X,'3A'X/
C
      SAVE
C
C     ******************************************************************
C     PICKS UP OPERATORS & SYMBOLS FROM LISI & STORES IN SYM & OPR
C     ******************************************************************
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
                      CALL ISBYTE('20'X,LISI,I-1) !IF YES, ZOT IT
                      GO TO 20
                      ENDIF
C
      IF(IT.EQ.X3A) THEN                          !TST FOR :
                      CALL ISBYTE('20'X,LISI,I-1) !IF YES, ZOT IT
                      DSPF='NONE'                 !SET NO-DISPLAY FLG
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
  100 WRITE(CMSSG,105)
  105 FORMAT('MORE THAN 49 SYMBOLS IN SCAT EXPRESSION - NOT ALLOWED')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,110)
  110 FORMAT('NO COMPUTED SCALERS PROVIDED')
      CALL MESSLOG(LOGUT,LOGUP)
      NERR=1
      RETURN
      END
