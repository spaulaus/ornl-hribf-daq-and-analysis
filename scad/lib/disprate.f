C$PROG DISPRATE  - Displays RATE in meter form
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 03/16/2000
C     ******************************************************************
C
      SUBROUTINE DISPRATE
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ==================================================================
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                                      !STAR-8
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/XLBB/ AA(2,20),BB(2,20),PLOTYP(20)
      REAL*4       AA,      BB
      CHARACTER*4                    PLOTYP
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      INTEGER*4    WINDAT,       WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
      INTEGER*4    GCOR,   GCON                                  !STAR-8
C     ------------------------------------------------------------------
      COMMON/DD03/ NCALLR
      INTEGER*4    NCALLR
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD12/ NRATSCA(16)
      INTEGER*4    NRATSCA
C     ------------------------------------------------------------------
      COMMON/DD15/ NCALLRI
      INTEGER*4    NCALLRI
      DATA         NCALLRI/0/
C     ==================================================================
      INTEGER*4   NWIN,NLAS,NDO,I,J
C
      INTEGER*4   NRAT(9),IRAT(9),ID1
C
      REAL*4      RATLIS(120,9),RATE(9),RAT(9),SUM(9)
C
      REAL*4      SECVLU,TLAST,TNOW,DELT
C
      DATA        NWIN/1/
      DATA        NLAS/0/
      DATA        RATE/9*1.0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C
      IF(RATDSP.EQ.'LOG ') GO TO 20
C
      IF(NCALLRI.GT.0)     GO TO 20
C
C     ------------------------------------------------------------------
C     Init range indicators and time on first call
C     ------------------------------------------------------------------
C
      TLAST=SECVLU(0.0)
      NCALLRI=1
      RETURN
C
C     ------------------------------------------------------------------
C     Compute time & erase last display if appropriate ??
C     ------------------------------------------------------------------
C
   20 TNOW=SECVLU(0.0)
      DELT=TNOW-TLAST
      TLAST=TNOW
C
      IF(NCALLR.EQ.0) GO TO 30
      IF(NLAS.LE.0)   GO TO 30
C
   30 IF(RATFLG.NE.'RON ') THEN
      NLAS=0
      RETURN
      ENDIF
C
C     ------------------------------------------------------------------
C     Compute rates, add to rate-lists, do averaging, etc
C     ------------------------------------------------------------------
C
      DO 40 I=1,NRATE
C
      IF(RATTYP(I).EQ.'SCAL') THEN
      ID1=RATPAR(I)
      RATE(I)=FLOAT(NRATSCA(ID1))
      GO TO 40
      ENDIF
C
      RATE(I)=0.0
C
   40 CONTINUE
C
C
      DO 80 I=1,NRATE
      NRAT(I)=RATAVG(I)
      IF(RATE(I).LT.0.0) GO TO 80
      IRAT(I)=IRAT(I)+1
      IF(IRAT(I).GT.NRAT(I)) IRAT(I)=1
      RATLIS(IRAT(I),I)=RATE(I)
   80 CONTINUE
C
      DO 100 J=1,NRATE
      SUM(J)=0
      NDO=NRAT(J)
      DO 90 I=1,NDO
      SUM(J)=SUM(J)+RATLIS(I,J)
   90 CONTINUE
      RATE(J)=SUM(J)/FLOAT(NRAT(J))
  100 CONTINUE
C
C     ------------------------------------------------------------------
C     Do METER display
C     ------------------------------------------------------------------
C
  500 DO 520 I=1,NRATE
C
      CALL DOMETER('RATE',I,RATE(I))
C
  520 CONTINUE
C
      NLAS=NDO
C
      NCALLR=1
C
      NCALLRI=0
C
      RETURN
      END
