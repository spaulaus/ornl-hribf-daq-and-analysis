C$PROG CMPSAM    - Command processor for fitting operations
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE CMPSAM(IDONE,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SM23/ JYLOG,JDSP,KDSP,MXDL,MXFR,MAXH,
     &             NOP,NBC,KINBAK,MARKS,ECA,ECB,ECC,
     &             DEL,DELFAC,NMUL,XSTEP,DXMAX,FWLO,FWHI,FALO,FAHI,
     &             MAXNC,MAXVP,KFUNS,NWOOD,KPPL,ASUM,ID,NSKIP,RESOK,
     &             ILO,IHI,SETW
      CHARACTER*4  JDSP,KDSP,KINBAK,NWOOD,KPPL,MARKS,SETW,RESOK
C     ------------------------------------------------------------------
      CHARACTER*4  IDONE,KMD
C
      EQUIVALENCE (KMD,LWD)
C   
      DATA NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.NE.0) GO TO 10
C
      CALL SAMNIT
      NCALL=1
C
   10 IERR=0
C
      IF(KMD.EQ.'RELI') GO TO 140   
      IF(KMD.EQ.'FSTA') GO TO 150
      IF(KMD.EQ.'ECAL') GO TO 210
C   
      IF(NF.LE.0)       GO TO 900
      IF(ITYP(1).NE.1)  GO TO 500
      IF(KMD.EQ.'CAL ') RETURN
      IF(NTER.NE.0)     GO TO 500
C   
      IF(KMD.EQ.'FW  ') GO TO 210
      IF(KMD.EQ.'ASYM') GO TO 210
      IF(KMD.EQ.'WLIM') GO TO 210
      IF(KMD.EQ.'ALIM') GO TO 210
      IF(KMD.EQ.'DPX ') GO TO 210
      IF(KMD.EQ.'DEL ') GO TO 210
      IF(KMD.EQ.'NBC ') GO TO 210
      IF(KMD.EQ.'FUNO') GO TO 210
      IF(KMD.EQ.'KPPL') GO TO 210
      IF(KMD.EQ.'LIN ') GO TO 210
      IF(KMD.EQ.'LOG ') GO TO 210
      IF(KMD.EQ.'VB  ') GO TO 210
      IF(KMD.EQ.'FB  ') GO TO 210
      IF(KMD.EQ.'VX  ') GO TO 210
      IF(KMD.EQ.'VW  ') GO TO 210
      IF(KMD.EQ.'VALO') GO TO 210
      IF(KMD.EQ.'VAHI') GO TO 210
C   
      IF(KMD.EQ.'MON ') GO TO 210
      IF(KMD.EQ.'MOF ') GO TO 210
      IF(KMD.EQ.'DFI ') GO TO 210
      IF(KMD.EQ.'DPK ') GO TO 210
      IF(KMD.EQ.'DPPB') GO TO 210
      IF(KMD.EQ.'DMM ') GO TO 210
C
      IF(KMD.EQ.'WOOD') GO TO 210   
      IF(KMD.EQ.'BACK') GO TO 210
      IF(KMD.EQ.'SKIP') GO TO 210
      IF(KMD.EQ.'PZOT') GO TO 210
      IF(KMD.EQ.'BZOT') GO TO 210
C   
      IF(KMD.EQ.'PRP ') GO TO 220
      IF(KMD.EQ.'PRB ') GO TO 220
C   
      IF(KMD.EQ.'SAV ') GO TO 230
      IF(KMD.EQ.'SAX ') GO TO 230
      IF(KMD.EQ.'SAW ') GO TO 230
      IF(KMD.EQ.'SAL ') GO TO 230
      IF(KMD.EQ.'SAH ') GO TO 230
C   
      IF(KMD.EQ.'SET-') GO TO 240
      IF(KMD.EQ.'SETW') GO TO 240
      IF(KMD.EQ.'SETL') GO TO 240
      IF(KMD.EQ.'SETH') GO TO 240
C   
      IF(KMD.EQ.'PK  ') GO TO 250
C   
      IF(KMD.EQ.'WIN ') GO TO 260
      IF(KMD.EQ.'DL  ') GO TO 260
      IF(KMD.EQ.'DSX ') GO TO 260
      IF(KMD.EQ.'DS  ') GO TO 260
      IF(KMD.EQ.'DF  ') GO TO 260
      IF(KMD.EQ.'DC  ') GO TO 260
C   
      IF(KMD.EQ.'FIT ') GO TO 300
      IF(KMD.EQ.'GFIT') GO TO 300
      IF(KMD.EQ.'RFIT') GO TO 300
      IF(KMD.EQ.'LFIT') GO TO 300
      IF(KMD.EQ.'RLFI') GO TO 300
C   
      IF(KMD.EQ.'PR  ') GO TO 310
      IF(KMD.EQ.'DR  ') GO TO 320
C   
      RETURN
C   
C     ------------------------------------------------------------------
C     CALL VARIOUS SUBROUTINES
C     ------------------------------------------------------------------
C
  140 CALL RELISET                !SET RELATIVE INTENSITIES
      GO TO 900
C   
  150 CALL DSTAT                  !DISPLAY FIT PARAMETERS
      GO TO 900
C
  210 CALL LSTPRO(IERR)           !PROCESS MISC CMD-LISTS
      IF(IERR.NE.0) GO TO 500
      GO TO 900
C   
  220 CALL DISPB(IERR)            !LIST PEAKS OR BGD POINTS ON VDT
      IF(IERR.NE.0) GO TO 500
      GO TO 900
C   
  230 CALL SAVUM(IERR)            !SAVE FIT PARAMETERS IN LIBRARY
      IF(IERR.NE.0) GO TO 500
      GO TO 900
C   
  240 CALL SETFA(IERR)            !SET FWHM AND ASYM PARAMETERS
      IF(IERR.NE.0) GO TO 500
      GO TO 900
C   
  250 CALL PEAKADD(IERR)          !ADD PEAKS TO LIBRARY
      IF(IERR.NE.0) GO TO 500
      GO TO 900
C   
  260 CALL DISPHANS(KERR)         !DISPLAY HANDLER
      GO TO 900
C   
  300 CALL FITUM(KERR)            !SET UP AND DO THE FIT
      IF(KERR.NE.0) GO TO 900
      MSGF='    '
      KMD='DF  '
      NF=1
      CALL DRESULT                !DISPLAY NUMERIC RESULTS
      CALL DISPHANS(KERR)         !DISPLAY GRAPHIC RESULTS
      GO TO 900
C   
  310 CALL LISTEM(KERR)           !PRINT OUT RESULTS
      GO TO 900
C   
  320 CALL DRESULT                !DISPLAY RESULTS
      GO TO 900
C   
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES  ETC
C     ------------------------------------------------------------------
C   
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR OR ILLEGAL CMD')
C   
      CALL MESSLOG(LOGUT,LOGUP)
C   
  900 IDONE='YES '
      RETURN
      END
