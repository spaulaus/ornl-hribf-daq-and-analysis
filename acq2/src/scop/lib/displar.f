C$PROG DISPLAR   - Display Routine for SCOP
C
C     ***************************************************************
C     BY W.T. MILNER AT ORPH - LAST MODIFIED 10/31/2001
C     ***************************************************************
C
      SUBROUTINE DISPLAR(DMODE)
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
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      INTEGER*4    WINDAT,       WINFLG,      NUMWIN,ISOPEN
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
      COMMON/SCD2/ MSPRED,DISPSEC
      INTEGER*4    MSPRED,DISPSEC
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C     ------------------------------------------------------------------
      INTEGER*4    NWAIT,IDS,TLAP,IERR
C
      INTEGER*4    TIC1
C
      INTEGER*4    LCMD,LU,I
C
      CHARACTER*4  DMODE
C
      CHARACTER*80 CWD
C
      DATA         LU    /1/
C
      INTEGER*4    NCALL
      DATA         NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(NCALL.NE.0) GO TO 50
C
      TIC1=0
C
      NWAIT=10
C
      NCALL=1
C
C     ------------------------------------------------------------------
C     Check for proper initialization
C     ------------------------------------------------------------------
C
   50 IF(NRATE.LE.0) GO TO 1000
C
      IF(NSCA.LE.0)  GO TO 1010
C
      DO 60 I=1,NRATE
      IF(RATPAR(I).LE.0) GO TO 60
      IF(RATPAR(I).GT.NSCA) THEN
      IERR=1
      WRITE(CMSSG,55)RATPAR(I)
   55 FORMAT('Rate parameter-',I3,' greater than # of scalers defined')
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
   60 CONTINUE
      IF(IERR.NE.0) GO TO 1020
C
      WRITE(CMSSG,90)
   90 FORMAT('Type: CTRL/C to interrupt display & return to cmd mode')
      CALL MESSLOG(LOGUT,0)
      WRITE(CMSSG,95)
   95 FORMAT('THINK BEFORE YOU INTERRUPT - OPERATORS MAY BE ',
     &       'MONITORING SCOP OUTPUT')
      CALL MESSLOG(LOGUT,0)
C
C
C     ****************************************************************
C     Monitor and Control loop starts here                           !
C     ****************************************************************
C                                                                    !
  100 CALL WAIT(NWAIT,1,IDS)          !Monitor loop                  !
C                                                                    !
      IF(MSGF.NE.'    ') GO TO 500    !Tst for Ctrl/C                !
C                                                                    !
      TIC1=TIC1+NWAIT                                                !
C                                                                    !
      IF(TIC1.LT.MSPRED)   GO TO 100                                 !
C                                                                    !
      IF(RATFLG.NE.'RON ') GO TO 102                                 !
C                                                                    !
      IF(DMODE.EQ.'RUN ')  CALL CAMRED                               !
C                                                                    !
      IF(DMODE.EQ.'TST ')  CALL CAMSYM('RUN ')                       !
C                                                                    !
  102 CALL DISPRATE('RUN ')                                          !
C                                                                    !
      TIC1=0                                                         !
C                                                                    !
      GO TO 100                       !Go to top of monitor loop     !
C                                                                    !
C     ****************************************************************
C     Monitor and Control loop ends here                             !
C     ****************************************************************
C
  500 IF(DISPTYP.NE.'DOFF') CALL DOMETER('INIT',0,0.0)
C
      RETURN
C
C     ------------------------------------------------------------------
C     Error returns
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Rates to display have not defined - cmd ignored')
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('SNIT-file is undefined - cmd ignored')
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Command ignored')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
      END
