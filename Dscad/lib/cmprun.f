C$PROG CMPRUN    - Command processor for RUN & TST commands
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 05/22/2005
C     ******************************************************************
C
C
      SUBROUTINE CMPRUN(IDONE,IERR)
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD10/ MODEGO
      CHARACTER*4  MODEGO
C     ------------------------------------------------------------------
      COMMON/SD13/ ISET,LULG
      CHARACTER*4  ISET
      INTEGER*4         LULG
C     ------------------------------------------------------------------
      COMMON/SD14/ MXBEEP,MXGOOD,BELL,BELLV,NBEEP
      INTEGER*4    MXBEEP,MXGOOD,BELL,BELLV,NBEEP
C     ------------------------------------------------------------------
      COMMON/SD15/ LUAL,THUSH,TNOW,ALFLG
      INTEGER*4    LUAL,THUSH,TNOW
      CHARACTER*4                  ALFLG
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI,LOU
      INTEGER*4    LIN,LCM,LCI,LOU
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
      INTEGER*4    IERR,TSEC,NTIC,ITOG,ISTAT,I
C
      CHARACTER*4  CLWD(2,40),IDONE
      EQUIVALENCE (CLWD,LWD)
C
      CHARACTER*20 MDATIM
C
      INTEGER*4    NWAIT,IDS,TLAP
C
      INTEGER*4    TIC1,TIC2
C
      CHARACTER*4  GFLG
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      KMD=CLWD(1,1)
C
      IF(KMD.EQ.'TSTA') GO TO 20
C
      IF(KMD.EQ.'RUN ') GO TO 50
C
      IF(KMD.EQ.'TST ') GO TO 50
C
      RETURN
C
C     ------------------------------------------------------------------
C     Do an ALARM test
C     ------------------------------------------------------------------
C
   20 CALL ALARMTST
C
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Setup to run
C     ------------------------------------------------------------------
C
   50 CALL RESETX(LULG,ISET)
      IF(ISET.NE.'YES ') GO TO 1050
C
      MODEGO=KMD
      CALL SCRNIT                       !INIT SCREEN
      CALL DISP(1)                      !DISPLAY SCALERS
      TSEC=0                            !RESET LOG-INTERVAL TIMER
C
      NBEEP=0                           !Reset beep cntr
      BELLV=BELL                        !Reset beep value
      NTIC=0                            !Init time ticker
      ITOG=1                            !Init toggler
C
      GFLG='ON  '
      IF(NRATE.LE.0) GFLG='OFF '
      IF(NSCA.LE.0)  GFLG='OFF '
C
      DO 60 I=1,NRATE
      IF(RATPAR(I).LE.0) GO TO 60
      IF(RATPAR(I).GT.NSCA) THEN
      WRITE(CMSSG,55)RATPAR(I)
   55 FORMAT('Rate parameter-',I3,' greater than # of scalers defined')
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
   60 CONTINUE
      IF(IERR.NE.0) GFLG='OFF '
C
      NWAIT=10
      TIC1=0
      TIC2=0
C
C
C     ------------------------------------------------------------------
C     Start run-loop
C     ------------------------------------------------------------------
C
  100 IF(MSGF.EQ.'ZZZZ') THEN           !TST FOR CTRL/Z
      CALL SNAP
      CALL SCRNIT
      MSGF='    '
      ENDIF

      IF(MSGF.EQ.'XXXX') GO TO 2000     !TST FOR CTRL/C
C
      CALL WAIT(NWAIT,1,ISTAT)
C
      IF(GFLG.EQ.'OFF ') GO TO 200
C
      TIC1=TIC1+NWAIT                                                !
C                                                                    !
      IF(TIC1.LT.MSPRED)   GO TO 200                                 !
C                                                                    !
      IF(RATFLG.NE.'RON ') GO TO 110                                 !
C                                                                    !
      IF(KMD.EQ.'RUN ')  CALL CAMREDD                                !
C                                                                    !
      IF(KMD.EQ.'TST ')  CALL CAMSYM                                 !
C                                                                    !
  110 CALL DISPRATE                                                  !
C                                                                    !
      TIC1=0                                                         !
C
  200 NTIC=NTIC+NWAIT
C
      IF(NTIC.LT.1000*SEC) GO TO 250
C
      NTIC=0
C
      CALL READUM                       !READ    SCALERS
C
      CALL DISP(1)                      !DISPLAY SCALERS
C
      CALL SENDBUF(6,DATLOC,8)
C
      CALL MILDATIM(MDATIM)
C
      WRITE(6,210)MDATIM
  210 FORMAT(/,A)
C
      IF(LSEC.LE.0)    GO TO 220        !TST FOR AUTOLOG ON
      TSEC=TSEC+SEC                     !INC LOG-INTERVAL COUNTER
      IF(TSEC.LT.LSEC) GO TO 220        !TST FOR LOG NEEDED
      CALL LOGUM(0,LOGUP)               !IF YES, LOGUM
      TSEC=0                            !RESET LOG-INTERVAL COUNTER
C
  220 CALL LIMDSP('INIT')               !Init limit display
C
      IF(ALFLG.NE.'ON  ') GO TO 100     !Tst alarm-on flag
      CALL SECSENS70(TNOW)              !Get current epoch time
      IF(TNOW.LT.THUSH)   GO TO 100     !Tst for hush-up state
      WRITE(LUAL,REC=1)TNOW             !Otherwise, record time
      CALL FLUSH(LUAL)                  !Force output to disk
C
      GO TO 100 
C
  250 TIC2=TIC2+NWAIT
      IF(TIC2.LT.500) GO TO 100
      CALL LIMDSP('CHEK')
      TIC2=0
      GO TO 100
C
C     ------------------------------------------------------------------
C     END run-loop
C     ------------------------------------------------------------------
C
 1050 WRITE(6,1055)
 1055 FORMAT(1H ,'SCAD not properly initialized - try again')
      GO TO 1500
C
 1500 IERR=1
C
 2000 IDONE='YES '
      RETURN
      END
