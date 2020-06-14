C$PROG ABORTUM   - Closes EPICS connection and EXITs
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 03/16/2000
C     ******************************************************************
C
      SUBROUTINE ABORTUM
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/EP01/ BLNAME,EPOPEN
      CHARACTER*5  BLNAME,EPOPEN
C     ------------------------------------------------------------------
      CHARACTER*128 ERRMSG
C
      INTEGER*4     IERR
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
CX    IF(EPOPEN.EQ.'YES') CALL FCLOSE_EPICS(IERR,ERRMSG)
C
CX    IF(IERR.NE.0) WRITE(6,10)ERRMSG
CX 10 FORMAT(' ',A)
C
      CALL EXIT(0)
C
      END
C$PROG DISPRATE  - Displays RATE in meter form
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 03/16/2000
C     ******************************************************************
C
      SUBROUTINE DISPRATE(MODE)
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
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C     ==================================================================
      CHARACTER*4  MODE
C
      CHARACTER*128 ERRMSG
C
      INTEGER*4   NWIN,NLAS,NDO,IERR,NERR,I,J
C
      INTEGER*4   NRAT(9),IRAT(9),ID1
C
      REAL*4      RATLIS(120,9),RATE(9),RAT(9),SUM(9)
C
      REAL*4      SECVLU,TLAST,TNOW,DELT
C
      DATA        NWIN/1/
      DATA        NLAS/0/
      DATA        NERR/0/
      DATA        RATE/9*1.0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(MODE.EQ.'ZERO') THEN
      DO 15 J=1,9
      IRAT(J)=0
      DO 10 I=1,120
      RATLIS(I,J)=0
   10 CONTINUE
   15 CONTINUE
      RETURN
      ENDIF
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
C     Do METER display & send rate-data to EPICS
C     ------------------------------------------------------------------
C
  500 IF(DISPTYP.EQ.'DOFF') GO TO 600
C
      DO 520 I=1,NRATE
C
      CALL DOMETER('RATE',I,RATE(I))
C
  520 CONTINUE
C
  600 CONTINUE
C
CX    CALL FWRITE_EPICS(NRATE,RATE,IERR,ERRMSG)
C
CX    IF(IERR.NE.0) THEN
CX    NERR=NERR+1
CX    WRITE(6,530)ERRMSG
CX530 FORMAT(' ',A)
CX    ENDIF
C
CX    IF(NERR.GE.100) CALL ABORTUM
C
      NLAS=NDO
C
      NCALLR=1
C
      NCALLRI=0
C
      RETURN
      END
C$PROG SNITTER   - Opens & processes scaler init file
C
C     ******************************************************************
C     BY W.T. MILNER AT ORPH - LAST MODIFIED 03/23/2000
C     ******************************************************************
C
      SUBROUTINE SNITTER(IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD1/ SNITNAM
      CHARACTER*80 SNITNAM
C     ------------------------------------------------------------------
      COMMON/EP01/ BLNAME,EPOPEN
      CHARACTER*5  BLNAME,EPOPEN
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
      CHARACTER*128  ERRMSG
C
      INTEGER*4    IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      INTEGER*4    STRLEN,NXBL,NXNB
C
      INTEGER*4    IERR,JERR,KIND,ISTAT,NLN,IA,I,J,N
C
      INTEGER*4    IV(4)
C
      REAL*4       XV
C
      CHARACTER*80 CWDRAW,CNAM
C
      CHARACTER*4  CWD(20)
C
      EQUIVALENCE (CWDRAW,IWDRAW),(CWD,IWD)
C
      CHARACTER*1  COMBYT
C
      EQUIVALENCE (COMBYT,IWD)
C
      INTEGER*4    MXSCA,LU
      DATA         MXSCA,LU/9,17/
C
      INTEGER*4    BLANK
      DATA         BLANK/'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     Pick up file name and open the mighyt snit-file
C     ------------------------------------------------------------------
C
      IERR=0
C
      IA=NXNB(IWDRAW,5,80)
      IF(IA.LE.0) GO TO 500
      CNAM=CWDRAW(IA:STRLEN(CWDRAW))
C
      CLOSE(UNIT=LU)
C
      OPEN(UNIT     = LU,
     &     FILE     = CNAM,
     &     STATUS   = 'OLD',
     &     IOSTAT   = ISTAT)
C
      IF(ISTAT.NE.0) GO TO 510
C
      SNITNAM=CNAM
C
C     ------------------------------------------------------------------
C     Initialize scaler labels for whatever reason
C     ------------------------------------------------------------------
C
      DO 20 J=1,MXSCA
      DO 10 I=1,3
      LAG(I,J)=BLANK
   10 CONTINUE
      GLIMON(J)='NULL'
      GLIMLO(J)=0.0
      GLIMHI(J)=0.0
   20 CONTINUE
C
C     ------------------------------------------------------------------
C     Read and process the snit-file
C     ------------------------------------------------------------------
C
      N=0                                    !Init # of scalers
      NLN=0                                  !Init line# counter
C
   50 READ(LU,55,END=200)IWD
   55 FORMAT(20A4)
C
      NLN=NLN+1                              !Inc line# counter
C
      IF(CWD(1).EQ.' ')    GO TO 50          !Ignore blank lines
C
      IF(COMBYT.EQ.'#')    GO TO 50          !Ignore comment lines
C
      IF(CWD(1).EQ.'$END') GO TO 200
C
      IA=NXBL(IWD,1,80)                      !Locate label field
      IF(IA.GT.12)   GO TO 520               !Tst for error
      IF(IA.LE.0)    GO TO 520               !Tst for error
C
      N=N+1                                  !Inc # of scalers
      IF(N.GT.MXSCA) GO TO 540               !Tst for too many
C
      CALL LODUP(IWD,1,IA,LAG(1,N),1)         !load up the label
C
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER) !Reformat CNAF fields
      IF(NTER.NE.0)  GO TO 520               !Tst for Error
C
      DO 60 I=1,4
      CALL MILV(LWD(1,I),IV(I),XV,KIND,JERR)
      IF(JERR.NE.0) GO TO 520
   60 CONTINUE
C
      CC(N)=IV(1)
      NN(N)=IV(2)
      AA(N)=IV(3)
      FF(N)=0
C
      GO TO 50                               !Go back for more
C
  200 CLOSE(LU)                              !Close snit-file
      NSCA=N                                 !Save # of scalers
      IF(NSCA.GT.8) NSCA=8                   !Limit to 8
      NRATE=NSCA                             !Save # of scalers
      WRITE(CMSSG,205)NSCA
  205 FORMAT(I3,' scalers set up')
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Call routine to open EPICS server
C     ------------------------------------------------------------------
C
CX    IF(EPOPEN.EQ.'YES') THEN
CX    CALL FCLOSE_EPICS(IERR,ERRMSG)
CX    EPOPEN='NO'
CX    ENDIF
C
CX    CALL FOPEN_EPICS(NSCA,BLNAME,IERR,ERRMSG)
C
CX    IF(IERR.NE.0) THEN
CX    WRITE(6,210)ERRMSG
CX210 FORMAT(' ',A)
CX    IERR=1
CX    RETURN
CX    ENDIF
C
      WRITE(6,250)BLNAME
  250 FORMAT(' Beamline is ',A)
C
      EPOPEN='YES'
C
      RETURN                                 !Return
C
C     ------------------------------------------------------------------
C     Error Returns
C     ------------------------------------------------------------------
C
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error in snit-file name')
      GO TO 1000
C
  510 WRITE(CMSSG,515)CNAM
  515 FORMAT('Unable to open snit-file - ',A)
      GO TO 1000
C
  520 WRITE(CMSSG,525)NLN
  525 FORMAT('Syntax error on snit-file line# = ',I3)
      GO TO 1000
C
  540 WRITE(CMSSG,545)MXSCA,NLN
  545 FORMAT('More than max of ',I2,' scalers defined on line#',I3)
      GO TO 1000
C
C
 1000 IERR=1
      NSCA=0
      NRATE=0
      SNITNAM='Undefined!'
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
