C$PROG ALARMTST  - Tests ALARM function
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/17/2004
C     ******************************************************************
C
      SUBROUTINE ALARMTST
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SD15/ LUAL,THUSH,TNOW,ALFLG
      INTEGER*4    LUAL,THUSH,TNOW
      CHARACTER*4                  ALFLG
C     ------------------------------------------------------------------
      INTEGER*4    SEC70,STAT,N
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      DO 100 N=1,3
C
      CALL WAIT(5000,1,STAT)
C
      CALL SECSENS70(SEC70)             !Get current epoch time
C
      WRITE(LUAL,REC=1)SEC70            !Record time
C
      CALL FLUSH(LUAL)                  !Force output to disk
C
  100 CONTINUE
C
      RETURN
C
      END
C$PROG BLANK     - Sets specified region of array to BLANK
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE BLANK(IBUF,IA,IB)
C
      INTEGER*4 IBUF(*)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 I=IA,IB
      IBUF(I)='20202020'X
   10 CONTINUE
      RETURN
      END
C$PROG CAMERRR   - Customized CAMAC error routine
C
C     ****************************************************************
C     Customized CAMAC error routine 
C     ****************************************************************
C
      subroutine CAMERRR(c,n,a,f,stat)

      implicit none
C
C     ****************************************************************
      integer*4 NOX
      integer*4 NOQ
      integer*4 NOX_NOQ
      integer*4 NOQ_NOX
      integer*4 TIMEOUT
      integer*4 ILL_CRATE
      integer*4 ILL_MODULE
      integer*4 ILL_ADDRESS
      integer*4 ILL_FUNCTION
      integer*4 CRATE_OFF_LINE
*
*  All codes are possible for CAMAC I/O.  For FASTBUS I/O, only
*  TIMEOUT and ILL_ADDRESS are possible.
*
      parameter (NOQ = 1)                  ! X = 1, Q = 0
      parameter (NOX = 2)                  ! X = 0, Q = 1
      parameter (NOX_NOQ = 3)              ! X = 0, Q = 0
      parameter (NOQ_NOX = 3)              ! X = 0, Q = 0
      parameter (TIMEOUT = 128)            ! Timeout
      parameter (ILL_CRATE = 'c004'X)      !Illegal CAMAC crate number 
      parameter (ILL_MODULE = 'c005'X)     !Illegal CAMAC module number 
      parameter (ILL_ADDRESS = 'c006'X)    !Illegal CAMAC subaddress 
      parameter (ILL_FUNCTION = 'c009'X)   !Illegal CAMAC function code
      parameter (CRATE_OFF_LINE = 'a000'X) !CAMAC crate off line
C     ****************************************************************
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      integer*4 stat,c,n,a,f
      integer*4 i
      character*20 msg(10)/
     1 'GOOD X   BAD Q','GOOD Q   BAD X','BAD  Q   - BAD X',
     2 'Timeout',
     3 'Illegal Crate','Illegal Module','Illegal Address',
     4 'Illegal Function','Crate Off_Line','Unknown error code'/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
      IF(STAT.EQ.0)        RETURN  !Return if GOOD
C
C     X = 1 and Q = 0 is ignored in this error handler
C
      if(stat.eq.NOQ) return   !return if bad Q only
C
      if (stat .eq. NOQ) then
         i = 1
      elseif (stat .eq. NOX) then
         i = 2
      elseif (stat .eq. NOX_NOQ) then
         i = 3
      elseif (stat .eq. TIMEOUT) then
         i = 4
      elseif (stat .eq. ILL_CRATE) then
         i = 5
      elseif (stat .eq. ILL_MODULE) then
         i = 6
      elseif (stat .eq. ILL_ADDRESS) then
         i = 7
      elseif (stat .eq. ILL_FUNCTION) then
         i = 8
      elseif (stat .eq. CRATE_OFF_LINE) then
         i = 9
      else
         i = 10
      endif
C
      write(cmssg,10) c,n,a,f,msg(i)
10    format(' CAM ERROR-C,N,A,F=',4I3,'  ',A)
      call messlog(logut,logup)
      return
      end
C$PROG CAMREDD   - Reads CAMAC scalers for RATE data
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 03/21/2000
C     ******************************************************************
C
      SUBROUTINE CAMREDD
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/DD12/ NRATSCA(16)
      INTEGER*4    NRATSCA
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      INTEGER*4    VBUF(16),STAT(16),NCALL,MODE,I
C
      REAL*4       TLAST,TNOW,DELT,VLAST(16),VNOW(16),RATE
C
      REAL*4       SECVLU
C
      DATA MODE/1/
      DATA NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 100
      TLAST=SECVLU(0.0)
      CALL CAMLIST(CC,NN,AA,FF,MODE,VBUF,NSCA,STAT)
      NCALL=1
      DO 10 I=1,NSCA
      VLAST(I)=VBUF(I)
   10 CONTINUE
      RETURN
C
  100 DO 110 I=1,NSCA
      STAT(I)=0
  110 CONTINUE
C
      TNOW=SECVLU(0.0)
      DELT=TNOW-TLAST
      TLAST=TNOW
C
      CALL CAMLIST(CC,NN,AA,FF,MODE,VBUF,NSCA,STAT)
C
      DO 150 I=1,NSCA
C
      IF(STAT(I).NE.0) CALL CAMERRR(CC(I),NN(I),AA(I),FF(I),STAT(I))
C
  150 CONTINUE
C
      DO 160 I=1,NSCA
C
      VNOW(I)=VBUF(I)
C
      RATE=(VNOW(I)-VLAST(I))/DELT
C
      NRATSCA(I)=RATE
C
      VLAST(I)=VNOW(I)
C
  160 CONTINUE
C
      RETURN
      END
C$PROG CAMRED    - Reads CAMAC scalers for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE CAMRED
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD09/ CC(512),NN(512),AA(512),FF(512),VBUF(512),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
      INTEGER*4 STAT(512)
C
      DATA MODE/1/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 I=1,NR
      STAT(I)=0
   10 CONTINUE
C
      CALL CAMLIST(CC,NN,AA,FF,MODE,VBUF,NLIST,STAT)
C
      DO 50 I=1,NR
C
      IF (STAT(I).NE.0) CALL CAMERRR(CC(I),NN(I),AA(I),FF(I),STAT(I))
C
   50 CONTINUE
      RETURN
      END
C$PROG CAMSYM    - Simulates reading of  CAMAC scalers for RATE data
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 03/21/2000
C     ******************************************************************
C
      SUBROUTINE CAMSYM
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/DD12/ NRATSCA(16)
      INTEGER*4    NRATSCA
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      INTEGER*4    VBUF(16),NCALL,MODE,I
C
      INTEGER*4    ISEED
C
      REAL*4       TLAST,TNOW,DELT,VLAST(16),VNOW(16),RATE
C
      REAL*4       FAC,RFAC,RAN,SECVLU
C
      REAL*4       SINFAC,DEG,DTOR
C
      DATA         DEG   /0.0/
      DATA         DTOR  /0.017453292/
      DATA         MODE  /1/
      DATA         NCALL /0/
      DATA         ISEED /-1/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 100
      TLAST=SECVLU(0.0)
      NCALL=1
      DO 10 I=1,NSCA
      FAC=10.0*10.0**I
      RFAC=0.50*FAC
      VBUF(I)=FAC+RAN(ISEED)*RFAC
      VLAST(I)=VBUF(I)
   10 CONTINUE
      RETURN
C
  100 DEG=DEG+1.0
      SINFAC=0.5*(1.0+SIN(DTOR*DEG))
C
      TNOW=SECVLU(0.0)
      DELT=TNOW-TLAST
      TLAST=TNOW
C
      DO 160 I=1,NSCA
C
CX    FAC=SINFAC*10.0*10.0**I
      FAC=SINFAC*10.0**I
C
      RFAC=0.20*FAC
C
      VBUF(I)=VLAST(I)+FAC+RFAC*RAN(ISEED)
C
      VNOW(I)=VBUF(I)
C
      RATE=(VNOW(I)-VLAST(I))/DELT
C
      NRATSCA(I)=RATE
C
      VLAST(I)=VNOW(I)
C
  160 CONTINUE
C
      RETURN
      END
C$PROG CBSAST    - Sets MSGF & renables CTRL-backslash
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE CBSAST(IDUM)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C
      MSGF='ZZZZ'                      !SET MSG-FLAG
C
      CALL CTBSNIT                     !ENABLE CTRL-backslash AGAIN
C
      CALL STOP_RTIMER
C
      RETURN
      END

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
C$PROG CMPSCAD   - Command processor for program SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/22/2005
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SCALER DISPLAY PROGRAM - FOR CAMAC SCALERS
C     CAN USE TVI912C OR ANSI TERMINALS (DEPENDING ON START STRING)
C     ------------------------------------------------------------------
C
      SUBROUTINE CMPSCAD(IDONE,IERR)
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
      COMMON/HEP/  IHEPF
      INTEGER*4    IHEPF
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      INTEGER*4    KTERM
C     ------------------------------------------------------------------
      COMMON/SD10/ MODEGO
      CHARACTER*4  MODEGO
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
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
      COMMON/SD16/ PRNAM
      CHARACTER*76 PRNAM
      DATA         PRNAM/' '/
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI,LOU
      INTEGER*4    LIN,LCM,LCI,LOU
C     ------------------------------------------------------------------
      INTEGER*4    IHELP(20,300),LHEP
C
      CHARACTER*20 MDATIM
C
      CHARACTER*4  CLWD(2,40),IDONE
      EQUIVALENCE (CLWD,LWD)
C
      INTEGER*4    NAMCMD(20),IERR,BLANK
C
      DATA         SEC,LSEC/5,0/
C
      DATA         KTERM,BLANK/'ANSI','    '/
C
      DATA         LHEP/16/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
C
      WRITE(6,777)KMY
  777 FORMAT(A)
C
      IERR=0
C
      KMD=CLWD(1,1)
C
      IF(KMD.EQ.'LON ') GO TO 100
      IF(KMD.EQ.'LOF ') GO TO 100
      IF(KMD.EQ.'CMD ') GO TO 110
      IF(KMD.EQ.'    ') GO TO 2000
      IF(KMD.EQ.'H   ') GO TO 125
      IF(KMD.EQ.'HELP') GO TO 125
      IF(KMD.EQ.'END ') GO TO 500
      IF(KMD.EQ.'NORS') GO TO 130
      IF(KMD.EQ.'NORT') GO TO 130
      IF(KMD.EQ.'TAB ') GO TO 140
      IF(KMD.EQ.'LOG ') GO TO 150
      IF(KMD.EQ.'SNAP') GO TO 155
      IF(KMD.EQ.'ZERO') GO TO 160
      IF(KMD.EQ.'SEC ') GO TO 170
      IF(KMD.EQ.'LSEC') GO TO 170
C
      IF(KMD.EQ.'HUSH') GO TO 180
C
      IF(KMD.EQ.'RLIM') GO TO 200
      IF(KMD.EQ.'RLMM') GO TO 205
      IF(KMD.EQ.'GLIM') GO TO 205
C
      IF(KMD.EQ.'BPON') GO TO 210
      IF(KMD.EQ.'BPOF') GO TO 210
C
      IF(KMD.EQ.'NAMP') GO TO 220
C
      RETURN
C
  100 LISFLG=KMD
      GO TO 2000
C
  110 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)  !OPEN COMMAND FILE
      GO TO 2000
C
  125 CALL HELPMANU(IWD,LHEP,IHELP,300,20,IHEPF)
      GO TO 2000
C 
  130 CALL NORMAN
      GO TO 2000
C
  140 CALL TABO                         !LIST ARRAYS FOR DIAGNOSTICS
      GO TO 2000
C
  150 CALL READUM                       !READ SCALERS
      CALL LOGUM(LOGUT,LOGUP)           !LOG ON SCAD.LOG
      GO TO 2000
C
  155 CALL SNAP                         !WRITE SCALERS TO scadymdhms.snap
      GO TO 2000
C
  160 CALL ZOTUM                        !ZERO ALL SCALERS
      GO TO 2000
C
  170 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 1000
      IF(KMD.EQ.'LSEC') GO TO 172
      CALL LIMIV(LWD(1,2),1,20,IV,IERR)
      IF(IERR.NE.0) GO TO 1010
      SEC=IV
      GO TO 2000
C
  172 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1010
      IF(IV.EQ.0)   GO TO 174
      IF(IV.GE.300.AND.IV.LE.3600) GO TO 174
      GO TO 1020
  174 LSEC=IV
      GO TO 2000
C
  180 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 1000
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      CALL SECSENS70(TNOW)
      THUSH=TNOW+60*IV
      GO TO 2000
C
  200 CALL LIMSET
      GO TO 2000
C
  205 CALL GLIMSET
      GO TO 2000
C
  210 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      IF(IV.LT.0.OR.IV.GT.1000) GO TO 1060
C
      IF(KMD.EQ.'BPON') MXGOOD=IV
      IF(KMD.EQ.'BPOF') MXBEEP=IV
      GO TO 2000
C
  220 IF(IWDRAW(2).EQ.BLANK) THEN
      PRNAM=' '
      GO TO 2000
      ENDIF
      CALL FINAME(IWDRAW,5,80,PRNAM,IERR)
      IF(IERR.NE.0) GO TO 1000
      GO TO 2000
C
  500 CALL SCLR
      CALL EXIT
C
 1000 WRITE(6,1005)
 1005 FORMAT(1H ,'Syntax error or illegal value - Ignored')
      GO TO 1500
C
 1010 WRITE(6,1015)
 1015 FORMAT(1H ,'Illegal display interval - Legal range = 1,20')
      GO TO 1500
C
 1020 WRITE(6,1025)
 1025 FORMAT(1H ,'Illegal Log-interval - Legals = 0 or 300 to 3600')
      GO TO 1500
C
 1060 WRITE(6,1065)IV
 1065 FORMAT(1H ,'Illegal value =',I8,' - legal range = 0 to 1000')
      GO TO 1500
C
 1500 IERR=1
C
 2000 IDONE='YES '
      RETURN
      END
C$PROG CMPSCUD   - Command processor for SCUD
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/08/2004
C     ******************************************************************
C
      SUBROUTINE CMPSCUD(IDONE,IERR)
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
      COMMON/III/  LIN,LCM,LCI,LOU
      INTEGER*4    LIN,LCM,LCI,LOU
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DD03/ NCALLR
      INTEGER*4    NCALLR
C     ==================================================================
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                                      !STAR-8
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                                    ISOPEN
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
      INTEGER*4    GCOR,   GCON                                  !STAR-8
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL,    IBANNER
C     ==================================================================
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
C     ==================================================================
      CHARACTER*4  KMD,KMX,KMM,IDONE,JDONE
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(1,2))
C     ==================================================================
C
      INTEGER*4    NAMCMD(20),IERR
C
      INTEGER*4    KIND,IV,NDX,LU,I
C
      REAL*4       XV
C
      CHARACTER*80 CWD
C
      DATA         LU/1/
C     ==================================================================
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'DLIN') GO TO 80
      IF(KMD.EQ.'DLOG') GO TO 85
C
      IF(KMD.EQ.'GLIN') GO TO 90
      IF(KMD.EQ.'GLOG') GO TO 90
C
      IF(KMD.EQ.'STAT') GO TO 120
      IF(KMD.EQ.'CMD ') GO TO 130
C
      IF(KMD.EQ.'FIG ') GO TO 220
      IF(KMD.EQ.'CMAP') GO TO 230
      IF(KMD.EQ.'REVV') GO TO 260
C
      IF(KMD.EQ.'RAV1') GO TO 310
      IF(KMD.EQ.'RAV2') GO TO 310
      IF(KMD.EQ.'RAV3') GO TO 310
      IF(KMD.EQ.'RAV4') GO TO 310
      IF(KMD.EQ.'RAV5') GO TO 310
      IF(KMD.EQ.'RAV6') GO TO 310
      IF(KMD.EQ.'RAV7') GO TO 310
      IF(KMD.EQ.'RAV8') GO TO 310
      IF(KMD.EQ.'RAV ') GO TO 320
C
      IF(KMD.EQ.'RAT1') GO TO 350
      IF(KMD.EQ.'RAT2') GO TO 350
      IF(KMD.EQ.'RAT3') GO TO 350
      IF(KMD.EQ.'RAT4') GO TO 350
      IF(KMD.EQ.'RAT5') GO TO 350
      IF(KMD.EQ.'RAT6') GO TO 350
      IF(KMD.EQ.'RAT7') GO TO 350
      IF(KMD.EQ.'RAT8') GO TO 350
C
      IF(KMD.EQ.'DPS ') GO TO 400
C
      IF(KMD.EQ.'GNIT') GO TO 500
      IF(KMD.EQ.'CLRG') GO TO 550
      IF(KMD.EQ.'CLRD') GO TO 550
C
      RETURN
C
   80 KMM='GLIN'
      GO TO 100
   85 KMM='GLOG'
      GO TO 100
   90 KMM=KMD
C
  100 DISPTYP=KMM
      CALL DOMETER('INIT',0,0.0)
      GO TO 2500
C
  120 CALL STATMAN
      GO TO 2500
C
  130 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)  !OPEN COMMAND FILE
      GO TO 2500
C
C
C     ------------------------------------------------------------------
C     Process FIG related commands
C     ------------------------------------------------------------------
C
  220 IF(NSCA.LE.0) GO TO 1330
      WRITE(CWD,225)NSCA
  225 FORMAT('FIG ',I1)
      CALL NEWFIG(LU,CWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL DOMETER('INIT',0,0.0)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NCALLR   =0
      GO TO 2500
C
  230 CALL COLRSET(10,IWD,IERR)
      GO TO 2500
C
  260 CALL REVV
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process RATE average related commands
C     ------------------------------------------------------------------
C
  310 NDX=1
      IF(KMD.EQ.'RAV2') NDX=2
      IF(KMD.EQ.'RAV3') NDX=3
      IF(KMD.EQ.'RAV4') NDX=4
      IF(KMD.EQ.'RAV5') NDX=5
      IF(KMD.EQ.'RAV6') NDX=6
      IF(KMD.EQ.'RAV7') NDX=7
      IF(KMD.EQ.'RAV8') NDX=8
C
      IF(NF.EQ.1) THEN
      RATAVG(NDX)=5
      GO TO 2500
      ENDIF
C
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2210
      IF(IV.GT.100) GO TO 2220
      IF(IV.EQ.0) IV=5
      RATAVG(NDX)=IV
      GO TO 2500
C
  320 IF(NF.EQ.1) THEN
      DO 325 I=1,NRATE
      RATAVG(I)=5
  325 CONTINUE
      GO TO 2500
      ENDIF
C
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2210
      IF(IV.GT.100) GO TO 2220
      IF(IV.EQ.0) IV=5
C
      DO 330 I=1,NRATE
      RATAVG(I)=IV
  330 CONTINUE
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process RATE ON/OFF commands
C     ------------------------------------------------------------------
C
  350 NDX=1
      IF(KMD.EQ.'RAT2') NDX=2
      IF(KMD.EQ.'RAT3') NDX=3
      IF(KMD.EQ.'RAT4') NDX=4
      IF(KMD.EQ.'RAT5') NDX=5
      IF(KMD.EQ.'RAT6') NDX=6
      IF(KMD.EQ.'RAT7') NDX=7
      IF(KMD.EQ.'RAT8') NDX=8
C
      IF(KMX.EQ.'OFF ') THEN
      RATTYP(NDX)=KMX
      RATPAR(NDX)=0
      GO TO 2500
      ENDIF
C
      IF(KMX.EQ.'ON  ') THEN
      RATTYP(NDX)='SCAL'
      RATPAR(NDX)=NDX
      GO TO 2500
      ENDIF
C
      GO TO 1320
C
C     ------------------------------------------------------------------
C     Process Displays/sec command - DPS
C     ------------------------------------------------------------------
C
  400 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2210
      IF(IV.GT.20)  GO TO 2230
C
      IF(IV.LE.0) IV=10
C
      DISPSEC=IV
      MSPRED=1000/DISPSEC
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Open & process scaler specification file - snit-file
C     ------------------------------------------------------------------
C
  500 CALL GNITTER(IERR)
      IF(IERR.NE.0) GO TO 2500
C
      NRATE=NSCA
C
      WRITE(CWD,505)NSCA
  505 FORMAT('FIG ',I1)
C
      CALL NEWFIG(LU,CWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL DOMETER('INIT',0,0.0)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NCALLR   =0
C
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Clear all meter display windows
C     ------------------------------------------------------------------
C
  550 DO 560 I=1,9
      IF(WINFLG(1,I).EQ.0) GO TO 560
      CALL XX_WINMAN('ERAS',I)
      WINFLG(1,I)=0
  560 CONTINUE
      NCALLR=0
      NRATE=0
      NSCA=0
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Generate error messages
C     ------------------------------------------------------------------
C
 1290 WRITE(CMSSG,1295)
 1295 FORMAT('Requested RAV outside allowed range of 0 to 100')
      GO TO 2400
C
 1300 WRITE(CMSSG,1305)NSCA
 1305 FORMAT('Requested RATE PAR outside allowed range of 1 to ',I4)
      GO TO 2400
C
 1310 WRITE(CMSSG,1315)NSCA
 1315 FORMAT('Requested scaler# outside allowed range of 0 to ',I4)
      GO TO 2400
C
 1320 WRITE(CMSSG,1325)KMX
 1325 FORMAT(A4,' is an illegal rate specifier')
      GO TO 2400
C
 1330 WRITE(CMSSG,1335)
 1335 FORMAT('Cannot FIG - Scalers have not been defined')
      GO TO 2400
C
 2210 WRITE(CMSSG,2215)
 2215 FORMAT('Illegal command or syntax error - cmd Ignored')
      GO TO 2400
C
 2220 WRITE(CMSSG,2225)IV
 2225 FORMAT('Specified value',I6,'  .GT. max value of 100 - ignored')
      GO TO 2400
C
 2230 WRITE(CMSSG,2235)IV
 2235 FORMAT('Specified value',I6,'  .GT. max value of 20 - ignored')
      GO TO 2400
C
 2400 CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2500
C
C     ------------------------------------------------------------------
C     RETURN
C     ------------------------------------------------------------------
C
 2500 IDONE='YES '
      RETURN
      END
C$PROG COMNIT    - Defines/sets common
C
C     ******************************************************************
C     BY W.T. MILNER AT ORPH - LAST MODIFIED 01/25/2000
C     ******************************************************************
C
      SUBROUTINE COMNIT
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
      COMMON/DD03/ NCALLR
      INTEGER*4    NCALLR
      DATA         NCALLR/0/
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C
      DATA         RATFLG  /'RON '/
      DATA         RATAVG  /9*5/
      DATA         RATPAR  /1,2,3,4,5,6,7,8,9/
      DATA         RATTYP  /9*'SCAL'/
      DATA         RATDSP  /'LOG '/
CX    DATA         NRATE   /1/
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN,ISOPEN
C
      DATA         ISOPEN/'NO  '/
      DATA         NUMWIN/0/
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
C
      DATA         MODE_PL/'LIN '/
C     ------------------------------------------------------------------
      COMMON/SCD2/ MSPRED,DISPSEC
      INTEGER*4    MSPRED,DISPSEC
C
      DATA         MSPRED /100/
      DATA         DISPSEC/10/
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C
      DATA         DISPTYP/'GLIN'/
C     ------------------------------------------------------------------
C
      INTEGER*4    MODE,NN
C
      SAVE
C
C     ==================================================================
C
C
      RETURN
      END
C$PROG COMPRO    - Processes SYMBOL-OPERATOR list into POINTER-GOTO list
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE COMPRO
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                               VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 KI
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
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
      SAVE
C
C     ****************************************************************
C     ROUTINE TO PROCESS SYMBOL-OPERATOR LIST INTO POINTER-GOTO LIST
C     ****************************************************************
C
      IF(NSY.LE.0)  RETURN              !TST FOR EMPTY
      IF(NERR.NE.0) RETURN              !TST FOR ERROR STATE
C
      IF(LABNDX(SYM(1,1)).NE.0)GOTO 100 !ERROR IF DEFINED SYMBOL EXIST
C
      NT=NT+1                           !INC TOTAL # SYMBOLS
      IF(NT.GT.MAXT) GO TO 110          !TST FOR TABLE OVERFLOW
C
      DO 10 I=1,3                       !SAVE DEFINED SYMBOL
      LA(I,NT)=SYM(I,1)
   10 CONTINUE
      KI(NT)=DSPF
      IF(KI(NT).NE.'NONE') NDD=NDD+1    !INC # SCALERS TO DISPLAY?
      IF(NDD.GT.MAXD) GO TO 140         !TST FOR TOO MANY
C
      LO(NT)=NPO+1                      !SET LO-LIMIT INDEX IN POL-GOL
C
      DO 20 I=2,NSY                     !LOOP ON REMAINING SYMBOLS
      NPO=NPO+1                         !INC POL-GOL COUNTER
      IF(NPO.GT.MAXT) GO TO 120         !TST FOR TABLE OVERFLOW
      POL(NPO)=LABNDX(SYM(1,I))         !SET SYMBOL INDEX IN POL
      IF(POL(NPO).LE.0) GO TO 130       !TST FOR NON-EXIST
      GOL(NPO)=OPR(I)                   !SET OPERATOR NO. IN GOL
   20 CONTINUE
C
      HI(NT)=NPO                        !HI-LIMIT INDEX IN POL-GOL
C
      RETURN
C
  100 WRITE(6,105)(SYM(K,1),K=1,3)
  105 FORMAT(1H ,'ATTEMPT TO RE-DEFINE SYMBOL - ',3A4)
      GO TO 200
  110 WRITE(6,115)MAXT
  115 FORMAT(1H ,'MORE THAN',I4,' SYMBOLS DEFINED')
      GO TO 200
  120 WRITE(6,125)MAXT
  125 FORMAT(1H ,'COMPUTATION TABLE OVERFLOW AT',I4,' ENTRIES')
      GO TO 200
  130 WRITE(6,135)(SYM(K,I),K=1,3)
  135 FORMAT(1H ,'UNDEFINED SYMBOL REFERENCED = ',3A4)
      GO TO 200
C
  140 WRITE(6,145)MAXD
  145 FORMAT(1H ,'REQUEST DISPLAY OF .GT.',I4,' SCALERS - REJECTED')
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'sorry about that')
      CALL EXIT(2)
      END
C$PROG COMPUM    - Generates computed scaler values & rates
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE COMPUM
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      REAL*4 CVAL,DVAL,CV,DV,CVX,DVX
C
      REAL*4 VNF(512),VDF(512)
C
      EQUIVALENCE (VNF,VN),(VDF,VD)
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ROUTINE TO GENERATE COMPUTED SCALER VALUES & RATES
C     ------------------------------------------------------------------
C     LO(I)   = LO-INDEX IN POL & GOL FOR COMPUTING SCALER-I
C     HI(I)   = HI-INDEX IN POL & GOL FOR COMPUTING SCALER-I
C
C     POL(J)  - POINTS TO VALUE IN VN TO BE USED
C     GOL(J)  - CONTAINS GOTO LIST (1,2,3,4) FOR + - / * OPERATION
C     ------------------------------------------------------------------
C
C
      IF(NR.GE.NT) RETURN               !TST FOR NONE TO COMPUTE
C
      NA=NR+1                           !FIRST INDEX TO COMPUTE
C
      DO 200 I=NA,NT                    !LOOP ON COMPUTED VALUES
C
      VNF(I)=0.0
      VDF(I)=0.0
C
      CVAL=0.0                          !RESET CUMULATIVE VALUE
      DVAL=0.0                          !RESET DIFFERENCE VALUE
      JA=LO(I)                          !FIRST INDEX IN COMP-LIST
      JB=HI(I)                          !LAST  INDEX IN COMP-LIST
C
      DO 100 J=JA,JB                    !LOOP ON COMP-LIST ENTRIES
      JG=GOL(J)                         !OPERATION-TYPE
      JP=POL(J)                         !INDEX IN VN-ARRAY
      CV=0.0
      IF(JP.LE.NR) CV=VN(JP)            !RAW      VALUES ARE INTEGER
      IF(JP.GT.NR) CV=VNF(JP)           !COMPUTED VALUES ARE FLO6TING
      DV=VDF(JP)                        !DIFFERENCE VALUE
C
      GO TO(10,20,30,40),JG             !GO TO SPECIFIC OPERATION
      GO TO 100                         !SKIP IF NOT 1,2,3,4
C
   10 CVAL=CVAL+CV                      !ADD TO   ACCUMULATED VALUE
      DVAL=DVAL+DV
      GO TO 100
   20 CVAL=CVAL-CV                      !SUB FROM ACCUMULATED VALUE
      DVAL=DVAL-DV
      GO TO 100
   30 CVX=CV
      DVX=DV
      IF(CVX.LT.1.0) CVX=1.0
      IF(DVX.LT.1.0) DVX=1.0
      CVAL=CVAL/CVX                     !DIV INTO ACCUMULATED VALUE
      DVAL=DVAL/DVX
      GO TO 100
   40 CVAL=CVAL*CV                      !MUL BY   ACCUMULATED VALUE
      DVAL=DVAL*DV
C
  100 CONTINUE
C
      VNF(I)=CVAL                       !SAVE FLOATING VALUE
      VDF(I)=DVAL
C
  200 CONTINUE
      RETURN
      END
C$PROG COMSET    - Reads & processes input lines for computed scalers
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/26/2005
C     ******************************************************************
C
      SUBROUTINE COMSET
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MAXLIS=400)
C
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD05/ LISI(400),SYM(3,50),OPR(50),NLS, NSY, DSPF
      INTEGER*4    LISI,     SYM,      OPR,    NLS, NSY, DSPF
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      COMMON/SD13/ ISET,LULG
      CHARACTER*4  ISET
      INTEGER*4         LULG
C     ------------------------------------------------------------------
      INTEGER*4    IWD(20)
C
      INTEGER*4    X26
      DATA         X26/'26'X/
C
      CHARACTER*4  IDONE
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     COMSET - READS AND PROCESSES INPUT LINES FOR COMPUTED SCALERS
C     ------------------------------------------------------------------
C     LISI     - CONTAINS CONCATINATED INPUT LINES FOR SYMBOL DEF
C     NLS      = # OF WORDS SET IN LISI
C
C     SYM      - CONTAINS SYMBOL LIST FOR SINGLE COMPUTED VALUE
C     SYM(I,1) - CONTAINS SYMBOL BEING DEFINED
C     SYM(I,N) - CONTAINS N-1TH SYMBOL USED IN COMPUTATION
C     OPR(N)   = OPERATOR (1,2,3,4 FOR +-/*) ASSOCIATED WITH SYM(I,N)
C     NSY      = # OF SYMBOLS (INCLUDING DEFINED) FOR SINGLE VALUE
C
C     DSPF     = 'YES '/'NO  ' SAYS SYMBOL TO BE/NOT-BE DISPLAYED
C     ------------------------------------------------------------------
C
      NPO=0                             !ZERO COMP-LIST COUNTER
      NERR=0                            !ZERO # COMPILATION ERRORS
      IDONE='NO  '                      !RESET DONE-FLAG
C
   10 NSY=0                             !ZERO # OF SYMBS FOR THIS LIST
      NLS=0                             !ZERO CONCATINATED-LIST CNTR
      READ(LULG,20,END=100)IWD          !READ FIRST SOURCE LINE
   20 FORMAT(20A4)
C
      DO 30 I=1,20                      !SAVE IN CONCATINATED LIST
      NLS=NLS+1
      IF(NLS.GT.MAXLIS) GO TO 200
      LISI(NLS)=IWD(I)
   30 CONTINUE
C
   40 READ(LULG,20,END=60)IWD           !READ SUBSEQUENT LINES
C
      CALL ILBYTE(IT,IWD,0)             !TST FOR CONTINUATION &
C
      IF(IT.NE.X26) GO TO 70            !IF NOT, GO PROCESS LISI
C
      CALL ISBYTE('20'X,IWD,0)          !OTHERWISE, ZOT THE &
      DO 50 I=1,20                      !AND ACCUMULATE IT
      NLS=NLS+1
      IF(NLS.GT.MAXLIS) GO TO 200
      LISI(NLS)=IWD(I)
   50 CONTINUE
      GO TO 40                          !GO READ IN NEXT LINE
C
   60 IDONE='YES '
   70 CALL SYMBOP                       !CONSTRUCT SYMB-OPER LIST
      CALL COMPRO                       !PROCESS OUT TO POL-GOL LIST
      IF(IDONE.EQ.'YES ') GO TO 100     !TST FOR DONE
      BACKSPACE LULG                    !BACKSPACE OVER LAST RECORD
      GO TO 10                          !GO BACK FOR NEXT LIST
C
  100 RETURN
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'MORE THAN 19 CONTINUATION LINES - NOT ALLOWED')
      WRITE(6,210)
  210 FORMAT(1H ,'sorry about that')
      CALL EXIT(3)
      END
C$PROG CTBSNIT   - Enables CTRL-backslash interrupts at CBSAST
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/27/2005 - for gnu
C     ******************************************************************
C
      SUBROUTINE CTBSNIT
C
      IMPLICIT INTEGER*4 (A-Z)
C
      EXTERNAL CBSAST                        !CTRL/Z HANDLER ROUTINE
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO INIT AND RE-ENABLE CRTL-BACKSLASH  TRAPS
C     ------------------------------------------------------------------
C
C
      IR=SIGNAL(3,CBSAST)
C
      RETURN
      END
C$PROG DISP      - Displays table of scaler values & rates
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 05/22/2005
C     ******************************************************************
C
      SUBROUTINE DISP(MODE)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                               VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 KI
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      CHARACTER*4  KTERM
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
      COMMON/SDXX/ ISKIP(512),SKIPLAB(8,512),NSKIP
      INTEGER*4    ISKIP,     SKIPLAB,       NSKIP
C
      CHARACTER*32 SKIPLABC(512)
      EQUIVALENCE (SKIPLABC,SKIPLAB)
C     ------------------------------------------------------------------
      INTEGER*4    JV(3),KV(3),JV2(2),KV2(2)
C
      INTEGER*4    IBUF(4096),TMF(2)
C
      INTEGER*2    IBUH(8192),TMH(4)
C
      CHARACTER*16 CBUF
C
      INTEGER*2    KBUH(8)
C
      EQUIVALENCE (CBUF,KBUH)
C
      REAL*4       VNF(512),VOF(512),VDF(512)
C
      EQUIVALENCE (JV2,JV),(KV2,KV),(LV2,LV),(MV2,MV)
      EQUIVALENCE (VNF,VN),(VOF,VO),(VDF,VD)
      EQUIVALENCE (TMF,TMH),(IBUH,IBUF)
C
      SAVE
C
C     ------------------------------------------------------------------
C     DISPLAYS SCALER VALUES AND RATES
C     ------------------------------------------------------------------
C
      DO 5 I=1,4096
      IBUF(I)=0
    5 CONTINUE
C
      N=0
      NXH=1
      NBYLL=4
C
      IF(KTERM.EQ.'ANSI') NBYLL=8
C
      DO 50 I=1,NT
C
      IF(KI(I).EQ.'NONE') GO TO 50
C
      N=N+1
C
      IF(ISKIP(N).NE.0) THEN
      CALL SENDBUF(6,LALOC(1,N),NBYLL)
      CALL SENDBUF(6,SKIPLAB(1,N),27)
      N=N+1
      ENDIF
C
      CALL SENDBUF(6,LALOC(1,N),NBYLL)
C
      CALL SENDBUF(6,LA(1,I),11)
C
      IF(MODE.EQ.0) GO TO 10
C
   10 TMF(1)=DALOC(1,N)
      TMF(2)=DALOC(2,N)
      IBUH(NXH)  =TMH(1)
      IBUH(NXH+1)=TMH(2)
C
      IF(KTERM.NE.'ANSI') THEN
                          NXH=NXH+2
                          GO TO 12
                          ENDIF
C
      IBUH(NXH+2)=TMH(3)
      IBUH(NXH+3)=TMH(4)
      NXH=NXH+4
C
   12 IF(KI(I).NE.'FLOT') GO TO 20
C
      CALL FLO8(VNF(I),JV)
      CALL FLO8(VDF(I),KV)
      WRITE(CBUF,15)JV2,KV2
   15 FORMAT(2A4,2A4)
      GO TO 30
C
   20 CALL FLI8(VN(I), JV)
      CALL FLO8(VDF(I),KV)
C
      WRITE(CBUF,15)JV2,KV2
C
   30 JJ=0
      DO 40 II=1,8
      IBUH(NXH+JJ)=KBUH(II)
      JJ=JJ+1
   40 CONTINUE
C
      NXH=NXH+11
C
   50 CONTINUE
C
      NBY=2*(NXH-1)
      IF(NBY.LT.4) GO TO 60
C
      CALL SENDBUF(6,IBUF,NBY)
C
   60 NBYY=4
      IF(KTERM.EQ.'ANSI') NBYY=8
C
      CALL SENDBUF(6,HOMLOC,NBYY)
C
      RETURN
      END
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
C$PROG DOMETER   - Draws scaler display "meter face"
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/15/2002
C     ******************************************************************
C
      SUBROUTINE DOMETER(MODE,IDW,COUNT)
C
      INTEGER*4    DPY,WDID                             !STAR-8 on Alpha
C
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
C
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
      INTEGER*4    GCOR,   GCON                         !STAR-8 on Alpha
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      CHARACTER*80 CTITL
      EQUIVALENCE (CTITL,ITITL)
C     ------------------------------------------------------------------
      COMMON/ME00/SQSIZ(20),FACRAD(20),NUDSP(20),NUDEC(20)
      REAL*4      SQSIZ,    FACRAD
      INTEGER*4                        NUDSP,    NUDEC
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
C
      CHARACTER*4  MODE,NUFLG
C
      DATA         DTOR/0.017453292/
C
C     ------------------------------------------------------------------
C
      REAL*4       ALN,BLN,ALG,BLG,ADC,BDC
C     ------------------------------------------------------------------
      DATA         ALN,BLN/225.0,-22.5/         !Scaling derived from:
C                                               !ANG = A + B*CNT
C                                               !for CNT=0,  ANG=225 deg
C                                               !for CNT=12, ANG=-45 deg
C     ------------------------------------------------------------------
      DATA         ALG,BLG/225.0,-90.12828/     !Scaling derived from:
C                                               !ANG = A + B*ALOG(CNT)
C                                               !for CNT=1,  ANG=225 deg
C                                               !for CNT=20, ANG=-45 deg
C     ------------------------------------------------------------------
      DATA         ADC,BDC/225.0,-30.0/         !Scaling derived from:
C                                               !ANG = A + B*CNT
C                                               !for CNT=0,  ANG=225 deg
C                                               !for CNT=9,  ANG=-45 deg
C     ------------------------------------------------------------------
C
      INTEGER*2    XYP(2,3,20)
C
      REAL*8       CMIN(20),CMAX(20),CMUL(20),CNT
C
      DATA         CMIN,CMAX,CMUL/20*0.0,20*12.0,20*1.0/
C
      CHARACTER*8  ERRFLG(20),LASFLG(20)
C
      INTEGER*4    ITST
C
      INTEGER*4    JXX,JYY,NCL(20),NCALL(20)
C
      DATA         NCL,NCALL/20*0,20*0/
C
      CHARACTER*10 CCNT,LASCCNT(20)
C
      INTEGER*4    ITOG(20)
      DATA         ITOG/20*2/
C
      INTEGER*4    LGCL(2),LGCH(2),LGCZ
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(MODE.EQ.'RATE') GO TO 100
C
      IF(MODE.NE.'INIT') RETURN
C
      DO 20 N=1,NUMWIN
C
      NUDSP(N)=0
      NUDEC(N)=0
C
      CMIN(N) =0.8
      CMAX(N) =12.0
      CMUL(N) =1.0
C
      IF(DISPTYP.EQ.'GLOG') THEN
      CMIN(N)=1.0
      CMAX(N)=20.0
      ENDIF
C
      ERRFLG(N)=' '
      LASFLG(N)=' '
C
      CALL NUMETER(N,CMUL(N),ERRFLG(N))
C
      NCALL(N)=0
C
      NCL(N)=0
C
   20 CONTINUE
C
      CALL GETGCO('WHIT',LGCL(1))
      CALL GETGCO('ERAS',LGCL(2))
C
      CALL GETGCO('RDGR',LGCH(1))
      CALL GETGCO('ERAS',LGCH(2))
C
      CALL GETGCO('ERAS',LGCZ)
C
      CALL GETGCO('OWHI',LGC)
C
      RETURN
C
C
C     ------------------------------------------------------------------
C     Display rates
C     ------------------------------------------------------------------
C
  100 N=IDW
C
      NUFLG='NO  '
C
      CNT=COUNT
C
      IF(CNT.LT.0.0) CNT=0.0
C
      ERRFLG(N)=' '
C
      IF(DISPTYP.EQ.'GLIN')  GO TO 300
C
C     ------------------------------------------------------------------
C     Do it for LOG display
C     ------------------------------------------------------------------
C
      IF(CNT.LT.1.0) THEN
      CMUL(N)=1.0
      CMIN(N)=1.0
      CMAX(N)=20.0
      CNT=1.0
      IF(LASFLG(N).EQ.' ') NUFLG='YES '
      ERRFLG(N)='  cps<1'
      GO TO 500
      ENDIF
C
  200 IF(CNT.GE.CMIN(N).AND.CNT.LE.CMAX(N)) THEN
      ERRFLG(N)=' '
      IF(LASFLG(N).NE.' ') NUFLG='YES '
      GO TO 500
      ENDIF
C
      IF(CNT.GT.CMAX(N)) THEN
      CMUL(N)=0.10*CMUL(N)
      CMIN(N)=10.0*CMIN(N)
      CMAX(N)=10.0*CMAX(N)
      NUFLG='YES '
      GO TO 200
      ENDIF
C
      IF(CNT.LT.CMIN(N)) THEN
      CMUL(N)=10.0*CMUL(N)
      CMIN(N)=0.10*CMIN(N)
      CMAX(N)=0.10*CMAX(N)
      NUFLG='YES '
      GO TO 200
      ENDIF
C
C     ------------------------------------------------------------------
C     DO it for LINEAR display
C     ------------------------------------------------------------------
C
  300 IF(CNT.GE.CMIN(N).AND.CNT.LE.CMAX(N)) GO TO 500
C
      ITST=1.0/CMUL(N)+0.5
C
      IF(CNT.LT.CMIN(N).AND.ITST.EQ.1) GO TO 500
C
      IF(CNT.GT.CMAX(N)) THEN
      CMUL(N)=0.10*CMUL(N)
      CMIN(N)=10.0*CMIN(N)
      CMAX(N)=10.0*CMAX(N)
      NUFLG='YES '
      GO TO 300
      ENDIF
C
      IF(CNT.LT.CMIN(N)) THEN
      CMUL(N)=10.0*CMUL(N)
      CMIN(N)=0.10*CMIN(N)
      CMAX(N)=0.10*CMAX(N)
      NUFLG='YES '
      GO TO 300
      ENDIF
C

  500 IF(NUFLG.NE.'YES ') GO TO 600
C
C
C     ------------------------------------------------------------------
C     Draw new banner & decade pointer
C     ------------------------------------------------------------------
C
      CALL NUMETER(N,CMUL(N),ERRFLG(N))
C
      NCALL(N)=0
C
      RP=12.0
C
      IMUL=1.0/CMUL(N)+0.5
C
      ANGL=ADC+BDC*ALOG10(FLOAT(IMUL))
C
      ARGP=DTOR*(ANGL+90.0)
      ARGM=DTOR*(ANGL-90.0)
C
      PX1=RP*COS(ARGM)
      PY1=RP*SIN(ARGM)
C
      PX2=RP*COS(ARGP)
      PY2=RP*SIN(ARGP)
C
      N=IDW
C
      IF(NUDEC(N).NE.0) THEN
      CALL XX_FILLPOLYGON(DPY,WDID(N),GCOR(2),XYP(1,1,N),3,MODE_OR)
      CALL XX_SYNC(DPY,.TRUE.)
      ENDIF
C
      PX3=0.4*FACRAD(N)*COS(DTOR*ANGL)
      PY3=0.4*FACRAD(N)*SIN(DTOR*ANGL)
C
      XYP(1,1,N)=0.5*SQSIZ(N)+PX1+0.5
      XYP(2,1,N)=0.5*SQSIZ(N)-PY1+0.5
C
      XYP(1,2,N)=0.5*SQSIZ(N)+PX2+0.5
      XYP(2,2,N)=0.5*SQSIZ(N)-PY2+0.5
C
      XYP(1,3,N)=0.5*SQSIZ(N)+PX3+0.5
      XYP(2,3,N)=0.5*SQSIZ(N)-PY3+0.5
C
      CALL XX_FILLPOLYGON(DPY,WDID(N),GCOR(2),XYP(1,1,N),3,MODE_OR)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NUDEC(N)=1
C
C     ------------------------------------------------------------------
C     Draw count pointer
C     ------------------------------------------------------------------
C
  600 RP=0.6*FACRAD(N)
C
      IF(DISPTYP.EQ.'GLIN') ANGL=ALN+BLN*CMUL(N)*CNT
C
      IF(DISPTYP.EQ.'GLOG') ANGL=ALG+BLG*DLOG(CMUL(N)*CNT)
C
      ARGP=DTOR*(ANGL+12.0)
      ARGM=DTOR*(ANGL-12.0)
C
      PX1=RP*COS(ARGM)
      PY1=RP*SIN(ARGM)
C
      PX2=RP*COS(ARGP)
      PY2=RP*SIN(ARGP)
C
      N=IDW
C
      IF(NUDSP(N).NE.0) THEN
      CALL XX_FILLPOLYGON(DPY,WDID(N),GCOR(2),XYP(1,1,N),3,MODE_OR)
      CALL XX_SYNC(DPY,.TRUE.)
      ENDIF
C
      PX3=FACRAD(N)*COS(DTOR*ANGL)
      PY3=FACRAD(N)*SIN(DTOR*ANGL)
C
      XYP(1,1,N)=0.5*SQSIZ(N)+PX1+0.5
      XYP(2,1,N)=0.5*SQSIZ(N)-PY1+0.5
C
      XYP(1,2,N)=0.5*SQSIZ(N)+PX2+0.5
      XYP(2,2,N)=0.5*SQSIZ(N)-PY2+0.5
C
      XYP(1,3,N)=0.5*SQSIZ(N)+PX3+0.5
      XYP(2,3,N)=0.5*SQSIZ(N)-PY3+0.5
C
      CALL XX_FILLPOLYGON(DPY,WDID(N),GCOR(2),XYP(1,1,N),3,MODE_OR)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NUDSP(N)=1
C
      LASFLG(N)=ERRFLG(N)
C
C     ------------------------------------------------------------------
C     Display numeric rate in graphics window
C     ------------------------------------------------------------------
C
      NCL(N)=NCL(N)+1
C
      IF(NCL(N).GE.10)   THEN
C
      NCL(N)=0
      JYY=WINDAT(4,N)-8
      JXX=            90
C
      IF(NCALL(N).NE.0) THEN
      CALL XX_DRAWSTRING(DPY,WDID(N),LGC,JXX,JYY,LASCCNT(N))
      ENDIF
C
      WRITE(CCNT,605)CNT
  605 FORMAT(1PE10.2)
      CALL XX_DRAWSTRING(DPY,WDID(N),LGC,JXX,JYY,CCNT)
      LASCCNT(N)=CCNT
C
      IF(GLIMON(N).EQ.'ON  ') THEN
      ITOG(N)=3-ITOG(N)
      LGCX=LGCL(2)
      IF(CNT.LT.GLIMLO(N)) LGCX=LGCL(ITOG(N))
      IF(CNT.GT.GLIMHI(N)) LGCX=LGCH(ITOG(N))
C
      IF(NCALL(N).NE.0) THEN
      CALL FILLCIR(N,LGCZ,0,0,50,50)
      CALL FILLCIR(N,LGCZ,0,195,50,50)
      CALL FILLCIR(N,LGCZ,195,0,50,50)
      CALL FILLCIR(N,LGCZ,195,195,50,50)
      CALL XX_SYNC(DPY,.TRUE.)
      ENDIF
C
      IF(CNT.GT.GLIMHI(N)) THEN
      CALL FILLCIR(N,LGCX,0,0,50,50)
      CALL FILLCIR(N,LGCX,195,0,50,50)
      ENDIF
C
      IF(CNT.LT.GLIMLO(N)) THEN
      CALL FILLCIR(N,LGCX,0,195,50,50)
      CALL FILLCIR(N,LGCX,195,195,50,50)
      ENDIF
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      ENDIF
C
      NCALL(N)=1
C
      ENDIF
C
      RETURN
      END
C$PROG FILLCIR   - Draws filler circle
C
      SUBROUTINE FILLCIR(IDW,GCO,IX,JY,IWID,IHI)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      INTEGER*4    DPY,WDID,GCO                         !STAR-8 on Alpha
C     ------------------------------------------------------------------
C
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
C
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL XX_FILLARC(DPY,
     &                WDID(IDW),
     &                GCO,
     &                IX,
     &                JY,
     &                IWID,
     &                IHI,
     &                0,
     &                23040)
C
      RETURN
      END
C$PROG FLI8      - Converts INTEGER value to 8-character ASCII
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLI8(IV,JV)
C
      CHARACTER*12  JV
C
      INTEGER*4     X20
C
      DATA          X20/'20'X/
C
      SAVE
C
C     ****************************************************************
C     CONVERTS INTEGER# (IV) TO ASCII INTEGER (JV)
C     ****************************************************************
C
      IF(IV.LT.0.OR.IV.GT.9999999) GO TO 20
C
      WRITE(JV,10)IV
   10 FORMAT(I8,'    ')
      RETURN
C
   20 X=IV
      WRITE(JV,25)X
   25 FORMAT(5PE11.4,' ')
      CALL ISBYTE(X20,JV,6)
      CALL ISBYTE(X20,JV,7)
      CALL ISBYTE(X20,JV,9)
      CALL SQUEZL(JV,2,11)
      RETURN
      END
C$PROG FLO10     - Converts floating# to 10 digit ASCII (FOAT or INT)
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLO10(X,JV)
C
      CHARACTER*12  JV
C
      SAVE
C
C     ****************************************************************
C     CONVERTS FLOATING# (X) TO 10-DIGIT ASCII FLOAT OR INTEGER (JV)
C     ****************************************************************
C
      IF(X.EQ.0.0)      GO TO 40
      IF(X.LT.0.01)     GO TO 10
      IF(X.LT.999.998)  GO TO 20
      IF(X.LT.999998.0) GO TO 40
C
   10 WRITE(JV,15)X
   15 FORMAT(5PE12.4)
      RETURN
C
   20 WRITE(JV,25)X
   25 FORMAT(F12.4)
      RETURN
C
   40 IV=X
      WRITE(JV,45)IV
   45 FORMAT(I12)
      RETURN
      END
C$PROG FLO12     - Converts floating# to 12 digit ASCII FLOAT
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLO12(X,JV)
C
      CHARACTER*12  JV
C
      SAVE
C
C     ****************************************************************
C     CONVERTS FLOATING# (X) TO 12-DIGIT ASCII FLOAT
C     ****************************************************************
C
   20 WRITE(JV,25)X
   25 FORMAT(F12.3)
      RETURN
C
      END
C$PROG FLO6      - Converts floating# to 6-digits ASCII (FLOAT or INT)
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLO6(X,JV)
C
      CHARACTER*12  JV
C
      INTEGER*4     X20
C
      DATA          X20/'20'X/
C
      SAVE
C
C     ****************************************************************
C     CONVERTS FLOATING# (X) TO 6-DIGIT ASCII FLOAT OR INTEGER (JV)
C     ****************************************************************
C
      IF(X.EQ.0.0)      GO TO 50
      IF(X.LT.0.1)      GO TO 10
      IF(X.LT.9.999)    GO TO 20
      IF(X.LT.99.99)    GO TO 30
      IF(X.LT.999.9)    GO TO 40
      IF(X.LT.99999.0)  GO TO 50
C
   10 WRITE(JV,15)X
   15 FORMAT(3PE9.2,'   ')
      CALL ISBYTE(X20,JV,4)
      CALL ISBYTE(X20,JV,5)
      CALL ISBYTE(X20,JV,7)
      CALL SQUEZL(JV,2,9)
      RETURN
C
   20 WRITE(JV,25)X
   25 FORMAT(F6.3,'      ')
      RETURN
C
   30 WRITE(JV,35)X
   35 FORMAT(F6.2,'      ')
      RETURN
C
   40 WRITE(JV,45)X
   45 FORMAT(F6.1,'      ')
      RETURN
C
   50 IV=X
      WRITE(JV,55)IV
   55 FORMAT(I6,  '      ')
      RETURN
      END
C$PROG FLO8      - Converts floating# to 8-digits ASCII (FLOAT or INT)
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE FLO8(X,JV)
C
      CHARACTER*12  JV
C
      INTEGER*4     X20
C
      DATA          X20/'20'X/
C
      SAVE
C
C     ****************************************************************
C     CONVERTS FLOATING# (X) TO 8-DIGIT ASCII FLOAT OR INTEGER (JV)
C     ****************************************************************
C
      IF(X.EQ.0.0)      GO TO 40
      IF(X.LT.0.01)     GO TO 10
      IF(X.LT.99.998)   GO TO 20
      IF(X.LT.9998.0)   GO TO 30
      IF(X.LT.9999998.) GO TO 40
C
   10 WRITE(JV,15)X
   15 FORMAT(5PE11.4,' ')
      CALL ISBYTE(X20,JV,6)
      CALL ISBYTE(X20,JV,7)
      CALL ISBYTE(X20,JV,9)
      CALL SQUEZL(JV,2,11)
      RETURN
C
   20 WRITE(JV,25)X
   25 FORMAT(F8.4,'    ')
      RETURN
C
   30 WRITE(JV,35)X
   35 FORMAT(F8.2,'    ')
      RETURN
C
   40 IV=X
      WRITE(JV,45)IV
   45 FORMAT(I8,  '    ')
      RETURN
      END
C$PROG GLABNDX   - Searches for & returns LABEL index in current LIST
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      FUNCTION GLABNDX(LABL)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      INTEGER*4 LABL(3)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      DO 20 N=1,NSCA
      DO 10 I=1,3
      IF(LABL(I).NE.LAG(I,N)) GO TO 20
   10 CONTINUE
      GO TO 50
   20 CONTINUE
C
      GLABNDX=0
      RETURN
C
   50 GLABNDX=N
      RETURN
      END
C$PROG GLIMSET   - Sets up rate-limits for GRAPHIC display & test
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 12/01/2005
C     ******************************************************************
C
      SUBROUTINE GLIMSET
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C
      DATA         GLIMON/16*'NULL'/
      DATA         GLIMLO/16*0.0/
      DATA         GLIMHI/16*0.0/
C     ------------------------------------------------------------------
      INTEGER*4    LWD3(3,40),ITYP3(40),NF3,NTER3
C
      INTEGER*4    GLABNDX,NDX,IV,KIND,IERR,IT,N,I,J,IDX
C
      REAL*4       XLO,XHI
C
      CHARACTER*4  KMX,KMY
      EQUIVALENCE (KMX,LWD(1,2)),(KMY,LWD(1,3))
C
      INTEGER*4    SETW
      CHARACTER*4  CSETW
      EQUIVALENCE (CSETW,SETW)
      DATA         CSETW/'SETW'/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMX.EQ.'SHO ') GO TO 100
      IF(KMX.EQ.'OFF ') GO TO 200
      IF(KMX.EQ.'ON  ') GO TO 220
      IF(KMX.EQ.'NULL') GO TO 240
C
C
      CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,SETW,12,NTER3)
C
      CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,1,80,NTER3)
C
      IF(NTER3.NE.0)    GO TO 500
C
      NDX=GLABNDX(LWD3(1,2))
C
      IF(NDX.LE.0)     GO TO 510
C
C
      IF(KMY.EQ.'OFF ') THEN
      IF(GLIMON(NDX).EQ.'NULL') GO TO 540
      GLIMON(NDX)='OFF '
      GO TO 1000
      ENDIF
C
      IF(KMY.EQ.'ON  ') THEN
      IF(GLIMON(NDX).EQ.'NULL') GO TO 540
      GLIMON(NDX)='ON  '
      GO TO 1000
      ENDIF
C
      IF(KMY.EQ.'NULL') THEN
      GLIMON(NDX)='NULL'
      GLIMLO(NDX)=0.0
      GLIMHI(NDX)=0.0
      GO TO 1000
      ENDIF
C
      IF(NF3.NE.4) GO TO 500
C
      CALL MILV3(LWD3(1,3),IV,XLO,KIND,IERR)
C
      IF(IERR.NE.0)    GO TO 520
C
      CALL MILV3(LWD3(1,4),IV,XHI,KIND,IERR)
C
      IF(IERR.NE.0)    GO TO 530
C
      GLIMON(NDX)='ON  '
      GLIMLO(NDX)=XLO
      GLIMHI(NDX)=XHI
      GO TO 1000
C
C     ------------------------------------------------------------------
C     Display current limit table
C     ------------------------------------------------------------------
C
  100 DO 120 N=1,NSCA
      WRITE(6,105)(LAG(I,N),I=1,3),GLIMLO(N),GLIMHI(N),GLIMON(N)
  105 FORMAT(3A4,2F10.3,2X,A4)
  120 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Turn ON/OFF all limits
C     ------------------------------------------------------------------
C
  200 DO 210 I=1,NSCA
      IF(GLIMON(I).NE.'NULL') GLIMON(I)='OFF '
  210 CONTINUE
      RETURN
C
  220 DO 230 I=1,NSCA
      IF(GLIMON(I).NE.'NULL') GLIMON(I)='ON  '
  230 CONTINUE
      RETURN
C
  240 DO 250 I=1,NSCA
      GLIMON(I)='NULL'
      GLIMLO(I)=0.0
      GLIMHI(I)=0.0
  250 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Report error messages
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error in graphic rate limit def - cmd ignored')
      GO TO 900
C
  510 WRITE(CMSSG,515)(LWD3(I,2),I=1,3)
  515 FORMAT('Undefined igraphic scaler label = ',3A4,' - cmd ignored')
      GO TO 900
C
  520 WRITE(CMSSG,525)
  525 FORMAT('Syntax error in graphic rate lo-limit def - cmd ignored')
      GO TO 900
C
  530 WRITE(CMSSG,535)
  535 FORMAT('Syntax error in graphic rate hi-limit def - cmd ignored')
      GO TO 900
C
  540 WRITE(CMSSG,545)
  545 FORMAT('Specified limit is NULL - cmd ignored')
C
  900 CALL MESSLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,SETW,8,NTER3)
      RETURN
      END
C$PROG GNITTER   - Opens & processes graphic scaler init file
C
C     ******************************************************************
C     BY W.T. MILNER AT ORPH - LAST MODIFIED 05/22/2005
C     ******************************************************************
C
      SUBROUTINE GNITTER(IERR)
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
      COMMON/SCD1/ GNITNAM
      CHARACTER*80 GNITNAM
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
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
      DATA         MXSCA,LU/8,17/
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
      GNITNAM=CNAM
C
C     ------------------------------------------------------------------
C     Initialize scaler labels  & limits for whatever reason
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
      IF(CWD(1).EQ.' ')    GO TO 50          !Ignore blanks
C
      IF(COMBYT.EQ.'#')    GO TO 50          !Ignore comments
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
      CALL LODUP(IWD,1,IA,LAG(1,N),1)        !load up the label
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
  530 WRITE(CMSSG,535)NSCA
  535 FORMAT(I3,' scalers set up')
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
      GNITNAM='Undefined!'
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG KINOP     - Returns OPERATOR type for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      FUNCTION KINOP(IT)
C
      INTEGER*4   X2B,X2D,X2F,X2A
C
      DATA        X2B,X2D,X2F,X2A/'2B'X,'2D'X,'2F'X,'2A'X/
C
      SAVE
C
C     ****************************************************************
C     RETURNS OPERATOR-TYPE FOR BYTE CONTAINED IN - IT
C     ****************************************************************
C
      IV=0
C
      IF(IT.EQ.X2B) IV=1              !TST FOR +
      IF(IT.EQ.X2D) IV=2              !TST FOR -
      IF(IT.EQ.X2F) IV=3              !TST FOR /
      IF(IT.EQ.X2A) IV=4              !TST FOR *
C
      KINOP=IV
      RETURN
      END
C$PROG LABNDX    - Searches for & returns LABEL index in current LIST
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      FUNCTION LABNDX(LABL)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      INTEGER*4 LABL(3)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 20 N=1,NT
      DO 10 I=1,3
      IF(LABL(I).NE.LA(I,N)) GO TO 20
   10 CONTINUE
      GO TO 50
   20 CONTINUE
C
      LABNDX=0
      RETURN
C
   50 LABNDX=N
      RETURN
      END
C$PROG LIMDSP    - Displays rate limits
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LIMDSP(MODE)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
C     ------------------------------------------------------------------
      COMMON/SD14/ MXBEEP,MXGOOD
      INTEGER*4    MXBEEP,MXGOOD
C     ------------------------------------------------------------------
      COMMON/SD15/ LUAL,THUSH,TNOW,ALFLG
      INTEGER*4    LUAL,THUSH,TNOW
      CHARACTER*4                  ALFLG
C     ------------------------------------------------------------------
      CHARACTER*4  MODE,FLAG,STAT
C
      CHARACTER*4  FLGX(10)
C
      INTEGER*4    AVD(3,10),ALO(3,10),AHI(3,10)
C
      INTEGER*4    BELL,BEEP,TOGL,NDX,N,I
C
      INTEGER*4    NGOOD,NBEEP,BELLV
C
      REAL*4       VDF(512)
      EQUIVALENCE (VDF,VD)
C
      DATA         BELL,BELLV,BEEP/'07070707'X,'07070707'X,0/
C
      DATA         STAT/'GOOD'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      FLAG='   '
C
      IF(MODE.EQ.'INIT') GO TO 100
      IF(MODE.EQ.'CHEK') GO TO 200
      RETURN
C
C     ------------------------------------------------------------------
C     Executes this part subsequent to scaler read
C     ------------------------------------------------------------------
C
  100 ALFLG='OFF '
C
      IF(NLIM.LE.0) RETURN
C
      STAT='GOOD'                            !Init status to GOOD
C
      CALL SENDBUF(6,HOMLOC,8)
C
      WRITE(6,105)
  105 FORMAT(5('******'),'*')
  106 FORMAT(5('------'),'-')
C
      DO 120 N=1,NLIM
      NDX=SCNDX(N)
      FLAG='    '
      FLGX(N)='    '
C
      IF(VDF(NDX).LT.LIMLO(N)) FLGX(N)='LO  '
      IF(VDF(NDX).GT.LIMHI(N)) FLGX(N)='HI  '
C
      CALL FLO6(VDF(NDX),AVD(1,N))
      CALL FLO6(LIMLO(N),ALO(1,N))
      CALL FLO6(LIMHI(N),AHI(1,N))
C
      WRITE(6,110)(LA(I,NDX),I=1,3),
     &             (AVD(I,N),I=1,2),
     &             (ALO(I,N),I=1,2),
     &             (AHI(I,N),I=1,2),
     &              FLGX(N)
C
  110 FORMAT(2A4,A3,3(A4,A2),1X,A1)
C
      IF(FLGX(N).NE.'    ') STAT='BAD '      !Tst for any out of limits
C
  120 CONTINUE
      WRITE(6,105)
      TOGL=1
C
      IF(STAT.EQ.'BAD ') NGOOD=0             !Reset  GOOD cntr ?
C
      IF(STAT.EQ.'BAD ') ALFLG='ON  '        !Set Alarm flag ?
C
      RETURN
C
C     ------------------------------------------------------------------
C     Executes this part for display (blink/beep) only
C     ------------------------------------------------------------------
C
  200 IF(NLIM.LE.0) RETURN
C
      IF(STAT.NE.'GOOD') GO TO 205           !Tsfr blink/beep needed
C
      NGOOD=NGOOD+1                          !Otherwise, inc GOOD cntr?
      IF(NGOOD.GE.MXGOOD) THEN               !Tsfr beep enable
      BELLV=BELL
      NBEEP=0
      ENDIF
      RETURN
C
C
  205 CALL SENDBUF(6,HOMLOC,8)
C
      TOGL=3-TOGL
C
      IF(TOGL.EQ.2) GO TO 300
C
C     ------------------------------------------------------------------
C     This part writes the data and limit-flag
C     ------------------------------------------------------------------
C
      WRITE(6,105)
      DO 220 N=1,NLIM
      NDX=SCNDX(N)
      BEEP=BELLV
C
      WRITE(6,210)(LA(I,NDX),I=1,3),
     &             (AVD(I,N),I=1,2),
     &             (ALO(I,N),I=1,2),
     &             (AHI(I,N),I=1,2),
     &              FLGX(N),BEEP
C
  210 FORMAT(2A4,A3,3(A4,A2),1X,A1,A1)
  220 CONTINUE
C
      WRITE(6,106)
C
      IF(BEEP.EQ.BELL) THEN                  !Tst for beeping
      NBEEP=NBEEP+1                          !If yes, inc beep cntr
      NGOOD=0                                !and reset GOOD cntr
      ENDIF
      IF(NBEEP.GE.MXBEEP) BELLV=0            !Tsfr max & disable?
C
      RETURN
C
C     ------------------------------------------------------------------
C     This part writes the data and a blank limit-flag
C     ------------------------------------------------------------------
C
  300 WRITE(6,106)
      DO 320 N=1,NLIM
      FLAG='    '
      NDX=SCNDX(N)
      BEEP=BELLV
C
      WRITE(6,210)(LA(I,NDX),I=1,3),
     &             (AVD(I,N),I=1,2),
     &             (ALO(I,N),I=1,2),
     &             (AHI(I,N),I=1,2),
     &              FLAG,BEEP
  320 CONTINUE
C
      IF(BEEP.EQ.BELL) THEN                  !Tst for beeping
      NBEEP=NBEEP+1                          !If yes, inc beep cntr
      NGOOD=0                                !and reset GOOD cntr
      ENDIF
      IF(NBEEP.GE.MXBEEP) BELLV=0            !Tsfr max & disable?
C
      WRITE(6,105)
      RETURN
      END
C$PROG LIMSAV    - Saves/deletes entries in rate-limit table
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LIMSAV(NDX,KIND,XLO,XHI)
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
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
C     ------------------------------------------------------------------
C
      CHARACTER*4  KIND
C
      INTEGER*4    NDX,II,I
C
      REAL*4       XLO,XHI
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KIND.EQ.'SAV ') GO TO 100
      IF(KIND.EQ.'DEL ') GO TO 200
                         GO TO 500
C
C     ------------------------------------------------------------------
C     Save a new entry or replace an old entry
C     ------------------------------------------------------------------
C
  100 DO 110 I=1,NLIM
      IF(NDX.EQ.SCNDX(I)) GO TO 120
  110 CONTINUE
      IF(NLIM.GE.MXLIM) GO TO 510
C
      NLIM=NLIM+1
      SCNDX(NLIM)=NDX
      LIMLO(NLIM)=XLO
      LIMHI(NLIM)=XHI
      RETURN
C
  120 LIMLO(I)=XLO
      LIMHI(I)=XHI
      RETURN
C
C     ------------------------------------------------------------------
C     Remove an entry
C     ------------------------------------------------------------------
C
  200 II=0
      DO 220 I=1,NLIM
      IF(SCNDX(I).EQ.NDX) GO TO 220
      II=II+1
      SCNDX(II)=SCNDX(I)
      LIMLO(II)=LIMLO(I)
      LIMHI(II)=LIMHI(I)
  220 CONTINUE
      NLIM=II
      RETURN
C
C     ------------------------------------------------------------------
C     Report error messages
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Illegal CALL LIMSAV - cmd ignored')
      GO TO 1000
C
  510 WRITE(CMSSG,515)MXLIM
  515 FORMAT('Rate limit table overflow (max# = ',I2,') - cmd ignored')
      GO TO 1000
C
 1000 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG LIMSET    - Sets up rate-limits for display & test
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LIMSET
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
      DATA         NLIM,MXLIM/0,10/
C     ------------------------------------------------------------------
      INTEGER*4    LWD3(3,40),ITYP3(40),NF3,NTER3
C
      INTEGER*4    LABNDX,NDX,IV,KIND,IERR,IT,N,I,J,IDX
C
      REAL*4       XLO,XHI
C
      CHARACTER*4  KMX,KMY
      EQUIVALENCE (KMX,LWD(1,2)),(KMY,LWD(1,3))
C
      INTEGER*4    SETW
      CHARACTER*4  CSETW
      EQUIVALENCE (CSETW,SETW)
      DATA         CSETW/'SETW'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMX.EQ.'SHO ') GO TO 100
      IF(KMX.EQ.'OFF ') GO TO 200
C
C
      CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,SETW,12,NTER3)
C
      CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,1,80,NTER3)
C
      IF(NTER3.NE.0)    GO TO 500
C
      NDX=LABNDX(LWD3(1,2))
C
      IF(NDX.LE.0)     GO TO 510
C
C
      IF(KMY.EQ.'OFF ') THEN
      CALL LIMSAV(NDX,'DEL ',XLO,XHI)
      GO TO 1000
      ENDIF
C
      IF(NF3.NE.4) GO TO 500
C
      CALL MILV3(LWD3(1,3),IV,XLO,KIND,IERR)
C
      IF(IERR.NE.0)    GO TO 520
C
      CALL MILV3(LWD3(1,4),IV,XHI,KIND,IERR)
C
      IF(IERR.NE.0)    GO TO 530
C
C
      CALL LIMSAV(NDX,'SAV ',XLO,XHI)
      GO TO 1000
C
C     ------------------------------------------------------------------
C     Display current limit table
C     ------------------------------------------------------------------
C
  100 DO 120 N=1,NLIM
      IDX=SCNDX(N)
      WRITE(6,105)(LA(I,IDX),I=1,3),LIMLO(N),LIMHI(N)
  105 FORMAT(1H ,3A4,2F10.3)
  120 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Turn OFF all limits
C     ------------------------------------------------------------------
C
  200 NLIM=0
      DO 210 I=1,MXLIM
      SCNDX(I)=0
      LIMLO(I)=0.0
      LIMHI(I)=0.0
  210 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Report error messages
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error in rate limit definition - cmd ignored')
      GO TO 900
C
  510 WRITE(CMSSG,515)(LWD3(I,2),I=1,3)
  515 FORMAT('Undefined scaler label = ',3A4,' - cmd ignored')
      GO TO 900
C
  520 WRITE(CMSSG,525)
  525 FORMAT('Syntax error in rate lo-limit definition - cmd ignored')
      GO TO 900
C
  530 WRITE(CMSSG,535)
  535 FORMAT('Syntax error in rate hi-limit definition - cmd ignored')
      GO TO 900
C
  900 CALL MESSLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWDRAW,LWD3,ITYP3,NF3,SETW,8,NTER3)
      RETURN
      END
C$PROG LOGOPEN   - Opens SCALER specificatiom file for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LOGOPEN(LU,NAMF,IERR)    
C
C     ------------------------------------------------------------------
      COMMON/SD04/ SNITFIL(20)
      INTEGER*4    SNITFIL
C     ------------------------------------------------------------------
      INTEGER*4 NAMF(20),NAMFI(20)
C
      CHARACTER*80 CNAMFI
C
      EQUIVALENCE (CNAMFI,NAMFI)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      DO 10 I=1,20
      NAMFI(I)  =NAMF(I)
      SNITFIL(I)=NAMF(I)
   10 CONTINUE
C
      CLOSE(UNIT=LU)
      IA=LSNB(NAMFI,1,80)
      CALL ISBYTE(0,NAMFI,IA)
C
      OPEN(UNIT     = LU,
     &     FILE     = CNAMFI,
     &     STATUS   = 'OLD',
     &     IOSTAT   = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL OPENERR(ISTAT)
                     IERR=ISTAT
                     ENDIF
      RETURN
      END
C$PROG LOGUM     - Logs scalers (read by calling prog) for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LOGUM(LT,LP)
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
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,             PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      CHARACTER*4  KIN(512)
      EQUIVALENCE (KIN,KI)
C
      INTEGER*4    JV(3),KV(3),JV2(2),KV2(2)
      EQUIVALENCE (JV2,JV),(KV2,KV)
C
      REAL*4       VNF(512),VDF(512)
      EQUIVALENCE (VNF,VN),(VDF,VD)
C
      INTEGER*4    LT,LP,N
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     LOG SCALERS
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,10)
   10 FORMAT('CURRENT SCALER VALUES ARE LISTED BELOW')
      CALL MESSLOG(LT,LP)
      WRITE(CMSSG,20)
   20 FORMAT('======================================')
      CALL MESSLOG(LT,LP)
C
      DO 100 N=1,NT
C
      IF(KIN(N).NE.'FLOT') GO TO 50
C
      CALL FLO8(VNF(N),JV)
      CALL FLO8(VDF(N),KV)
      GO TO 60
C
   50 CALL FLI8(VN(N), JV)
      CALL FLO8(VDF(N),KV)
C
   60 IF(LT.NE.0) THEN
      WRITE(LT,70)(LA(J,N),J=1,3),JV2,KV2
      ENDIF
C
      WRITE(LP,70)(LA(J,N),J=1,3),JV2,KV2
C
   70 FORMAT(2A4,A3,1X,2A4,2X,2A4)
C
  100 CONTINUE
C
      WRITE(CMSSG,110)
  110 FORMAT('END SCALER LIST')
      CALL MESSLOG(LT,LP)
      WRITE(CMSSG,20)
      CALL MESSLOG(LT,LP)
C
      RETURN
      END
C$PROG NEWFIG    - Special NEWFIG for program SCUD & the like
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/31/90
C     ******************************************************************
C
C     DELETES ALL EXISTING WINDOWS AND CREATES A NEW WINDOW
C     CONFIGURATION
C     
C     IFIG    -  SPECIFIES WINDOW CONFIGURATION NUMBER
C
C     FIGDAT(1,J) = X-COOR(PIX) OF UL-CORNER OF JTH-WINDOW
C     FIGDAT(2,J) = Y-COOR(PIX) OF UL-CORNER OF JTH-WINDOW
C     FIGDAT(3,J) = WIDTH(PIX)               OF JTH-WINDOW
C     FIGDAT(4,J) = HEIGHT(PIX)              OF JTH-WINDOW
C
C     FIGLIM(1,K) - POINTS TO FIRST FIGDAT ENTRY FOR KTH  CONFIG
C     FIGLIM(2,K) - POINTS TO LAST  FIGDAT ENTRY FOR KTH  CONFIG
C     NFIG        = NUMBER OF WINDOW CONFIGURATIONS IN FIGDAT
C
C     FIGDATS     - CONTAINS STANDARD (DEFAULT) FIGDAT-DATA
C     FIGLIMS     - CONTAINS STANDARD (DEFAULT) FIGLIM-DATA
C
C     TMPDAT      - CONTAINS TEMP FIGDAT-DATA (WHILE READING FILE)
C     TMPIDN      - CONTAINS TEMP WINDOW-IDS  (WHILE READING FILE)
C     ------------------------------------------------------------------
C
      SUBROUTINE NEWFIG(LU,IWD,IERR)
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
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                                      !STAR-8 
C
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
C
      COMMON/XLDD/ FIGDAT(4,20)
C
      INTEGER*4 NAMF(20),IWD(20),JWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4 IV(5)
C
      CHARACTER*4 IUSE(100)
C
      INTEGER*4 FIGLIMS(2,8),FIGLIM(2,50),TMPIDN(100)
C
      INTEGER*4 FIGDATL(144)
C
      INTEGER*4 FIGDATS(4,36),FIGDATA(4,100),TMPDAT(4,100)
C
      REAL*4 XV,WINDAT
C
      CHARACTER*80 CNAMF
C
      CHARACTER*4 KMD
C
      EQUIVALENCE (FIGDATS,FIGDATL)
      EQUIVALENCE (KMD,LWD)
      EQUIVALENCE (CNAMF,NAMF)
C
      DATA FIGDATL/
     &         0,         0,       245,       245,   !- 1 -  1
     &         0,         0,       245,       245,   !- 2 -  2
     &       250,         0,       245,       245,   !- 2 -  3
     &         0,         0,       245,       245,   !- 3 -  4
     &       250,         0,       245,       245,   !- 3 -  5
     &       500,         0,       245,       245,   !- 3 -  6
     &         0,         0,       245,       245,   !- 4 -  7
     &       250,         0,       245,       245,   !- 4 -  8
     &       500,         0,       245,       245,   !- 4 -  9
     &       750,         0,       246,       245,   !- 4 - 10
     &         0,         0,       245,       245,   !- 5 - 11
     &       250,         0,       245,       245,   !- 5 - 12
     &       500,         0,       245,       245,   !- 5 - 13
     &       750,         0,       246,       245,   !- 5 - 14
     &         0,       250,       245,       245,   !- 5 - 15 
     &         0,         0,       245,       245,   !- 6 - 16
     &       250,         0,       245,       245,   !- 6 - 17
     &       500,         0,       245,       245,   !- 6 - 18
     &       750,         0,       246,       245,   !- 6 - 19
     &         0,       250,       245,       245,   !- 6 - 20
     &       250,       250,       245,       245,   !- 6 - 21
     &         0,         0,       245,       245,   !- 7 - 22
     &       250,         0,       245,       245,   !- 7 - 23
     &       500,         0,       245,       245,   !- 7 - 24
     &       750,         0,       246,       245,   !- 7 - 25
     &         0,       250,       245,       245,   !- 7 - 26
     &       250,       250,       245,       245,   !- 7 - 27
     &       500,       250,       245,       245,   !- 7 - 28
     &         0,         0,       245,       245,   !- 8 - 29
     &       250,         0,       245,       245,   !- 8 - 30
     &       500,         0,       245,       245,   !- 8 - 31
     &       750,         0,       246,       245,   !- 8 - 32 
     &         0,       250,       245,       245,   !- 8 - 33
     &       250,       250,       245,       245,   !- 8 - 34
     &       500,       250,       245,       245,   !- 8 - 35
     &       750,       250,       245,       245/   !- 8 - 36
C
C     ------------------------------------------------------------------
C
      DATA FIGLIMS/01,01, 02,03, 04,06, 07,10, 11,15, 16,21,
     &             22,28, 29,36/
C
      DATA NCALL/0/
      DATA NDATS,NDAT,NTAB/36,0,0/
      DATA NFIGS,NFIG,NLIM/8,0,0/
      DATA MAXFIG,MAXFID,MAXLIN,MAXWIN/8,50,100,20/
C
      INTEGER*4  IFIGI,STARS
C
      DATA       IFIGI,STARS/'FIGI','****'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0                                   !RESET ERROR FLAG
C
      IF(IWD(1).EQ.IFIGI) GO TO 10
      IF(NCALL.GT.0)      GO TO 50
C
C     ************************************************************
C     COPY STANDARD FIG-DATA TO CURRENT FIG-DATA IF NCALL=0 
C     ************************************************************
C
   10 DO 20 J=1,NDATS
      DO 15 I=1,4
      FIGDATA(I,J)=FIGDATS(I,J)
   15 CONTINUE
   20 CONTINUE
      DO 30 J=1,NFIGS
      FIGLIM(1,J)=FIGLIMS(1,J)
      FIGLIM(2,J)=FIGLIMS(2,J)
   30 CONTINUE
      MAXFIG=NFIGS
      NCALL=1
      IF(IWD(1).EQ.IFIGI) RETURN
C
   50 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMD.EQ.'FIGF') GO TO 100
      IF(KMD.EQ.'FIG ') GO TO 400
                        GO TO 510
C
C     ************************************************************
C     OPEN SCREEN-FIG FILE AND RE-DEFINE SCREEN-FIG ID'S 
C     ************************************************************
C
  100 CLOSE(UNIT=LU)
C
      CALL FINAME(IWD,5,80,NAMF,IERR)
      IF(IERR.NE.0) GO TO 520
C
      OPEN(UNIT    = LU,
     &     FILE    = CNAMF,
     &     STATUS  = 'OLD',
     &     ACCESS  = 'SEQUENTIAL',
     &     IOSTAT  = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL IOFERR(ISTAT)
                     IERR=1
                     CLOSE(UNIT=LU)
                     RETURN
                     ENDIF
C
      REWIND LU
C
      NL=0
  110 READ(LU,115,END=150,ERR=530)JWD
  115 FORMAT(20A4)
      IF(JWD(1).EQ.STARS) GO TO 110
      CALL GREAD(JWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 540
      IF(NF.NE.5)   GO TO 540
C
      DO 120 I=1,5
      CALL MILV(LWD(1,I),IV(I),XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 540
  120 CONTINUE
C
      NL=NL+1
      IF(NL.GT.MAXLIN)    GO TO 550
      IF(IV(1).LE.0)      GO TO 560
      IF(IV(1).GT.MAXFID) GO TO 560
      DO 130 I=2,5
      IF(IV(I).LT.0)      GO TO 570
      IF(IV(I).GT.1024)   GO TO 570
  130 CONTINUE
      TMPIDN(NL)  =IV(1)
      TMPDAT(1,NL)=IV(2)
      TMPDAT(2,NL)=IV(3)
      TMPDAT(3,NL)=IV(4)
      TMPDAT(4,NL)=IV(5)
      GO TO 110
C
  150 NDO=NL
      DO 160 I=1,NDO
      IUSE(I)='NO  '
  160 CONTINUE
      DO 165 I=1,50
      FIGLIM(1,I)=0
      FIGLIM(2,I)=0
  165 CONTINUE
      NN=0
      DO 200 KID=1,MAXFID
      DO 180 J=1,NDO
      IF(IUSE(J).EQ.'YES ') GO TO 180
      IF(TMPIDN(J).NE.KID) GO TO 180
      IUSE(J)='YES '
      MAXFIG=KID
      NN=NN+1
      FIGLIM(2,KID)=NN
      IF(FIGLIM(1,KID).EQ.0) FIGLIM(1,KID)=NN
      DO 170 I=1,4
      FIGDATA(I,NN)=TMPDAT(I,NN)
  170 CONTINUE
  180 CONTINUE
  200 CONTINUE
      GO TO 710
C
C     ************************************************************
C     PROCESS A - FIG ID# - REQUEST 
C     ************************************************************
C
  400 IF(NTER.NE.0) GO TO 500
      CALL MILV(LWD(1,2),IFIG,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 500
      IF(IFIG.LE.0.OR.IFIG.GT.MAXFIG)GO TO 500 !TST FOR LEGAL FIG
      IF(FIGLIM(1,IFIG).LE.0)        GO TO 500
C
      ILO=FIGLIM(1,IFIG)                       !LO FIGDATA POINTER
      IHI=FIGLIM(2,IFIG)                       !HI FIGDATA POINTER
      IF((IHI-ILO+1).GT.MAXWIN) GO TO 580      !TST TOO MANY WINS
C
      DO 410 I=1,20                            !LOOP TO DELETE ALL
      IF(WINFLG(1,I).EQ.0) GO TO 410           !EXISTING WINDOWS
      WINFLG(1,I)=0
  410 CONTINUE
C
      ID=0                                     !RESET WINDOW ID#
C
      DO 440 I=ILO,IHI                         !LOOP ON FIGDATA INDEXES
      ID=ID+1                                  !INC WINDOW ID#
C
      DO 420 J=1,4                             !LOOP ON FIGDATA DATA
      FIGDAT(J,ID)=FIGDATA(J,I)                !SAVE IN FIGDAT
  420 CONTINUE
C
  440 CONTINUE
      NUMWIN=ID
      RETURN
C
C     ************************************************************
C     SEND ERROR MESSAGES FROM - FIG  REQUEST 
C     ************************************************************
C
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR OR ILLEGAL FIG ID - CMD IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C
C     ************************************************************
C     SEND ERROR MESSAGES FROM - FIGF REQUEST
C     ************************************************************
C
  510 WRITE(CMSSG,515)
  515 FORMAT('ILLEGAL NEWFIG CALL - CMD IGNORED')
      GO TO 700
C
  520 WRITE(CMSSG,525)
  525 FORMAT('SYNTAX ERROR IN FILENAME SPECIFICATION - CMD IGNORED')
      GO TO 700
C
  530 WRITE(CMSSG,535)NL+1
  535 FORMAT('ERR READING SCREEN-FIG-FILE - LINE#',I3,' CMD IGNORED')
      GO TO 700
C
  540 WRITE(CMSSG,545)NL+1
  545 FORMAT('SYNTAX ERR IN SCREEN-FIG-DATA - LINE#',I3,' CMD IGNORED')
      GO TO 700
C
  550 WRITE(CMSSG,555)MAXLIN
  555 FORMAT('NO. SCREEN-FIG DATA LINES .GT.',I2,' - CMD IGNORED')
      GO TO 700
C
  560 WRITE(CMSSG,565)NL,MAXFID
  565 FORMAT('ILLEGAL FIG-ID (.GT.',I2,') ON LINE#',I3,' CMD IGNORED')
      GO TO 700
C
  570 WRITE(CMSSG,575)NL
  575 FORMAT('ILLEGAL SCREEN COOR ON LINE#',I3,' CMD IGNORED')
      GO TO 700
C
  580 WRITE(CMSSG,585)MAXWIN
  585 FORMAT('NO. WINDOWS REQUESTED .GT.',I2,' - CMD IGNORED')
C
  700 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
  710 CLOSE(UNIT=LU)
      RETURN
      END
C$PROG NORMAN    - Saves & recalls normalization values for process SCAD
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C

      SUBROUTINE NORMAN
C
      IMPLICIT INTEGER*4 (A-Z)
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
C
      CHARACTER*4  LCMD
C
      REAL*4 NORFSAV
C
      DATA NORISAV,NORFSAV,LCMD/-1,1.0,'NONE'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'NORS') GO TO 100
      IF(KMD.EQ.'NORT') GO TO 200
                        RETURN
C
  100 IF(LCMD.NE.'NORT') RETURN
      NORI=NORISAV
      NORF=NORFSAV
      LCMD='NORS'
      RETURN
C
  200 IF(LCMD.EQ.'NORT') RETURN
      NORISAV=NORI
      NORFSAV=NORF
      NORI=-1
      NORF=1.0
      LCMD='NORT'
      RETURN
      END
C$PROG NUMETER   - Creates a new meter window & draws scale, etc
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/19/2002
C     ******************************************************************
C
      SUBROUTINE NUMETER(N,CMUL,ERRFLG)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                             !STAR-8 on Alpha
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
      INTEGER*4    GCOR,   GCON                         !STAR-8 on Alpha
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL,    IBANNER
      CHARACTER*80 CTITL
      EQUIVALENCE (CTITL,ITITL)
C     ------------------------------------------------------------------
      COMMON/ME00/SQSIZ(20),FACRAD(20),NUDSP(20),NUDEC(20)
      REAL*4      SQSIZ,    FACRAD
      INTEGER*4                        NUDSP,    NUDEC
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C     ------------------------------------------------------------------
      REAL*4       CNTLN(13),CNTLG(12),CNTDEC(10)
C
      REAL*4       ANG(50),X1(50),X2(50),Y1(50),Y2(50)
C
      REAL*4       TICLN(25),TICLG(21)
C
      INTEGER*4    IX1(50),IX2(50),IY1(50),IY2(50)
C
      INTEGER*4    N,I,IMUL,K,NDO
C
      REAL*4       W,H
C
      CHARACTER*1  LALN1(10),LALG1(9),LADEC(10),CNUM1
C
      CHARACTER*2  LALN2(3), LALG2(3),CNUM2
C
      DATA LADEC/'0','1', '2', '3', '4', '5', '6', '7', '8', '9'/
C
      DATA LALN1/'0','1', '2', '3', '4', '5', '6', '7', '8', '9'/
      DATA LALN2/'10','11','12'/
C
      DATA LALG1/'1', '2', '3', '4', '5', '6', '7', '8', '9'/
      DATA LALG2/'10','15','20'/
C
      DATA CNTLN/0,1,2,3,4,5,6,7,8,9,10,11,12/
C
      DATA CNTLG/1,2,3,4,5,6,7,8,9,10,15,20/
C
      DATA CNTDEC/0,1,2,3,4,5,6,7,8,9/
C
      DATA TICLN/0.0, 0.5,
     &           1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,
     &           5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5,
     &           10.0, 10.5, 11.0, 11.5, 12.0/
C
      DATA TICLG/1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,
     &     5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5,
     &     10., 15., 20./
C
      REAL*4       DTOR,ALN,BLN,ALG,BLG,ADC,BDC
C
      DATA         DTOR/0.017453292/
C
C     ------------------------------------------------------------------
      DATA         ALN,BLN/225.0,-22.5/         !Scaling derived from:
C                                               !ANG = A + B*CNT
C                                               !for CNT=0,  ANG=225 deg
C                                               !for CNT=12, ANG=-45 deg
C     ------------------------------------------------------------------
      DATA         ALG,BLG/225.0,-90.12828/     !Scaling derived from:
C                                               !ANG = A + B*ALOG(CNT)
C                                               !for CNT=1,  ANG=225 deg
C                                               !for CNT=20, ANG=-45 deg
C     ------------------------------------------------------------------
      DATA         ADC,BDC/225.0,-30.0/         !Scaling derived from:
C                                               !ANG = A + B*CNT
C                                               !for CNT=1,  ANG=225 deg
C                                               !for CNT=9,  ANG=-45 deg
C     ------------------------------------------------------------------
C
C
      INTEGER*2    PXY(2,3)
C
      INTEGER*4    FACSIZ(20)
C
      CHARACTER*12 CLA(16),CHMUL
C
      EQUIVALENCE (CLA,LAG(1,1))
C
      REAL*8       CMUL
C
      CHARACTER*8  ERRFLG
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
C     Draw meter face
C     ------------------------------------------------------------------
C
      W=WINDAT(3,N)                       !Window width (pixels)
      H=WINDAT(4,N)                       !Window height(pixels)
      SQSIZ(N)=W                          !Size of contained square
      IF(H.LT.SQSIZ(N)) SQSIZ(N)=H        !Size of contained square
      FACSIZ(N)=SQSIZ(N)-50               !Face diameter (pixels)
      FACRAD(N)=0.5*FACSIZ(N)             !Face radius   (pixels)
C
      IF(ERRFLG.NE.' ') THEN
      CHMUL=ERRFLG
      GO TO 100
      ENDIF
C
      IMUL=1.0/CMUL+0.5
C
      CHMUL='******'
C
      IF(IMUL.EQ.1)         CHMUL='  x1'
      IF(IMUL.EQ.10)        CHMUL='  x10'
      IF(IMUL.EQ.100)       CHMUL='  x10**2'
      IF(IMUL.EQ.1000)      CHMUL='  x10**3'
      IF(IMUL.EQ.10000)     CHMUL='  x10**4'
      IF(IMUL.EQ.100000)    CHMUL='  x10**5'
      IF(IMUL.EQ.1000000)   CHMUL='  x10**6'
      IF(IMUL.EQ.10000000)  CHMUL='  x10**7'
      IF(IMUL.EQ.100000000) CHMUL='  x10**8'
      IF(IMUL.GT.100000000) CHMUL='  ov-rng'
C
C
  100 WRITE(CTITL,105)CLA(N),CHMUL
  105 FORMAT(A,A)
C
      WINFLG(1,N)=0
      WINFLC(2,N)='OFF '
C
      CALL XX_WINMAN('WIN ',N)
C
      CALL FILLCIR(N,GCOR(2),25,25,FACSIZ(N),FACSIZ(N))
C
      NDO=12
C
      IF(DISPTYP.EQ.'GLIN') NDO=13
C
      DO 120 I=1,NDO
C
      IF(DISPTYP.EQ.'GLIN') ANG(I)=ALN+BLN*CNTLN(I)
C
      IF(DISPTYP.EQ.'GLOG') ANG(I)=ALG+BLG*ALOG(CNTLG(I))
C
      X1(I)= FACRAD(N)*COS(DTOR*ANG(I))
      Y1(I)= FACRAD(N)*SIN(DTOR*ANG(I))
      X2(I)=(FACRAD(N)+8)*COS(DTOR*ANG(I))
      Y2(I)=(FACRAD(N)+8)*SIN(DTOR*ANG(I))
C
      X1(I)=0.5*SQSIZ(N)+X1(I)
      X2(I)=0.5*SQSIZ(N)+X2(I)
      Y1(I)=0.5*SQSIZ(N)-Y1(I)
      Y2(I)=0.5*SQSIZ(N)-Y2(I)
C
      IX1(I)=X1(I)+0.5
      IX2(I)=X2(I)+0.5
      IY1(I)=Y1(I)+0.5
      IY2(I)=Y2(I)+0.5
C
  120 CONTINUE
C
C     ------------------------------------------------------------------
C     Draw tic labels
C     ------------------------------------------------------------------
C
      K=0
C
      NDO=9
C
      IF(DISPTYP.EQ.'GLIN') NDO=10
C
      DO 310 I=1,NDO
      K=K+1
      CNUM1=LALN1(I)
      IF(DISPTYP.EQ.'GLOG') CNUM1=LALG1(I)
      CALL XX_DRAWSTRING(DPY,WDID(N),GCON(2),IX2(K),IY2(K),CNUM1)
  310 CONTINUE
C
      DO 320 I=1,3
      K=K+1
      CNUM2=LALN2(I)
      IF(DISPTYP.EQ.'GLOG') CNUM2=LALG2(I)
      CALL XX_DRAWSTRING(DPY,WDID(N),GCON(2),IX2(K),IY2(K),CNUM2)
  320 CONTINUE
C
      CALL XX_SYNC(DPY,.TRUE.)
C
C     ------------------------------------------------------------------
C     Locate & draw tic-marks
C     ------------------------------------------------------------------
C
      NDO=21
C
      IF(DISPTYP.EQ.'GLIN') NDO=25
C
      DO 350 I=1,NDO
C
      IF(DISPTYP.EQ.'GLIN') ANG(I)=ALN+BLN*TICLN(I)
C
      IF(DISPTYP.EQ.'GLOG') ANG(I)=ALG+BLG*ALOG(TICLG(I))
C
      X1(I)= FACRAD(N)*COS(DTOR*ANG(I))
      Y1(I)= FACRAD(N)*SIN(DTOR*ANG(I))
      X2(I)=(FACRAD(N)+8)*COS(DTOR*ANG(I))
      Y2(I)=(FACRAD(N)+8)*SIN(DTOR*ANG(I))
C
      X1(I)=0.5*SQSIZ(N)+X1(I)
      X2(I)=0.5*SQSIZ(N)+X2(I)
      Y1(I)=0.5*SQSIZ(N)-Y1(I)
      Y2(I)=0.5*SQSIZ(N)-Y2(I)
C
  350 CONTINUE
C
      DO 360 I=1,NDO
C
      IX1(I)=X1(I)+0.5
      IX2(I)=X2(I)+0.5
      IY1(I)=Y1(I)+0.5
      IY2(I)=Y2(I)+0.5
C
      CALL XX_DRAWLINE(DPY,WDID(N),GCON(2),IX1(I),IY1(I),IX2(I),IY2(I))
C
  360 CONTINUE
C
      CALL XX_SYNC(DPY,.TRUE.)
C
C     ------------------------------------------------------------------
C     Locate & draw decade labels
C     ------------------------------------------------------------------
C
      NDO=10
C
      DO 370 I=1,NDO
C
      ANG(I)=ADC+BDC*CNTDEC(I)
C
      X1(I)= 0.4*FACRAD(N)*COS(DTOR*ANG(I))
      Y1(I)= 0.4*FACRAD(N)*SIN(DTOR*ANG(I))
      X2(I)=(0.4*FACRAD(N)+8)*COS(DTOR*ANG(I))
      Y2(I)=(0.4*FACRAD(N)+8)*SIN(DTOR*ANG(I))
C
      X1(I)=0.5*SQSIZ(N)+X1(I)
      X2(I)=0.5*SQSIZ(N)+X2(I)
      Y1(I)=0.5*SQSIZ(N)-Y1(I)
      Y2(I)=0.5*SQSIZ(N)-Y2(I)
C
  370 CONTINUE
C
      DO 380 I=1,NDO
C
      IX1(I)=X1(I)+0.5
      IX2(I)=X2(I)+0.5
      IY1(I)=Y1(I)+0.5
      IY2(I)=Y2(I)+0.5
C
      CNUM1=LADEC(I)
C
      CALL XX_DRAWSTRING(DPY,WDID(N),GCOR(2),IX2(I),IY2(I),CNUM1)
C
  380 CONTINUE
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      NUDSP(N)=0
C
      NUDEC(N)=0
C
      RETURN
C
      END
C$PROG NXSYM     - Picks up one operator & symbol at a time for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE NXSYM(ILO,IHI,KOP,SYMB,STAT)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD05/ LISI(400),SYM(3,50),OPR(50),NLS, NSY, DSPF
      INTEGER*4    LISI,     SYM,      OPR,    NLS, NSY, DSPF
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      INTEGER*4    SYMB(3)
C
      INTEGER*4    X20
      DATA         X20/'20'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     PICKS UP ONE OPERATOR & SYMBOL AT A TIME
C     ------------------------------------------------------------------
C
      STAT=0                            !SET GOOD STATUS
      KOP=1                             !SET DEFAULT OPERATOR +
      JT=1                              !SET DEFAULT OPERATOR +
      IF(ILO.GT.IHI) GO TO 100          !TST FOR OVERSHOOT
C
      DO 10 I=1,3                       !BLANK OUT THE SYMBOL
      SYMB(I)='20202020'X
   10 CONTINUE
C
      IA=ILO                            !FIRST BYTE TO EXAM
      DO 20 I=IA,IHI                    !LOOP ON REMAINING BYTES
      CALL ILBYTE(IT,LISI,I-1)          !PICK UP BYTE FROM LISI
      IF(IT.EQ.X20) GO TO 20            !IGNORE IF BLANK
      JT=KINOP(IT)                      !GET OPERATOR TYPE
      IF(JT.NE.0)   GO TO 30            !OPERATOR IF NON-ZERO
      GO TO 40                          !FIRST BYTE OF SYMBOL
   20 CONTINUE
C
   30 KOP=JT                            !SAVE OPERATOR TYPE
      I=I+1                             !INC SCAN PNTR
C
   40 IS=I                              !FIRST BYTE # FOR SYMBOL SCAN
      IA=NXNB(LISI,IS,IHI)              !FIND NEXT NON-BLANK
      IF(IA.LE.0) GO TO 100             !ERROR IF NOT FOUND
C
      DO 50 I=IA,IHI                    !LOOP ON REMAINING BYTES
      CALL ILBYTE(IT,LISI,I-1)          !PICK UP BYTE
      IF(IT.EQ.X20) GO TO 70            !DONE THIS SCAN IF BLANK
      JT=KINOP(IT)                      !GET OPERATOR TYPE
      IF(JT.NE.0)   GO TO 70            !DONE THIS SCAN IF OPERATOR
   50 CONTINUE
      I=IHI+1                           !FRIG INDEX
C
   70 IB=I-1                            !MAX BYTE # TO LOAD IN SYMB
C
      IF(IB-IA.GT.11) GO TO 200
C
      CALL LODUP(LISI,IA,IB,SYMB,1)     !LOAD UP SYMBOL
C
      ILO=IB+1                          !LO-BYTE FOR NEXT CALL
      RETURN
C
  100 STAT=1                            !ERROR RETURN
      RETURN
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'MORE THAN 12 CHARACTERS IN SYMBOL - NOT ALLOWED')
      WRITE(6,210)
  210 FORMAT(1H ,'NO COMPUTED SCALERS PROVIDED')
      NERR=1
      RETURN
      END
C$PROG OPENERR   - Displays OPEN-ERROR message for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE OPENERR(IOS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(IOS.EQ.0) RETURN
C
      WRITE(CMSSG,10)IOS
   10 FORMAT('ERROR OPENING FILE - ZSTAT =',Z10)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG READUM    - Reads scalers via call to CAMRED for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE READUM
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 TY
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      INTEGER*4    KMD,SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD09/ CC(512),NN(512),AA(512),FF(512),VBUF(512),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
      COMMON/SD10/ MODEGO
      CHARACTER*4  MODEGO
C     ------------------------------------------------------------------
      REAL*4   RAN
C
      REAL*4 VNF(512),VOF(512),VDF(512),VDNOR,DENO
C
      EQUIVALENCE (VNF,VN),(VOF,VO),(VDF,VD)
C
      DATA ISEED/-1/
C
      SAVE
C
C     ------------------------------------------------------------------
C     READUM - READS THE SCALERS (VIA CALL TO CAMRED), COPIES ANY
C     ECL-SCALERS FROM SPECIAL INPUT ARRAYS TO NORMAL ARRAYS, COMPUTES
C     DIFFERENCES, NORMALIZES, AND CALLS COMPUM TO DO COMPUTED SCALERS
C     ------------------------------------------------------------------
C
      DO 10 I=1,NT                      !SAVE PREVIOUS SCALER VALUES
      VO(I)=VN(I)
   10 CONTINUE
C
      IF(MODEGO.EQ.'TST ') GO TO 30     !TEST FOR TST-MODE
C
      CALL CAMRED                       !READ NEW VALUES
C
      N=0
      DO 15 I=1,NR                      !COPY NON-ECL TYPES
      IF(TY(I).EQ.'ECL ') GO TO 15      !SKIP IF ECL TYPE
      N=N+1
      VN(I)=VBUF(N)                     !SAVE IN VN
   15 CONTINUE
C
      DO 20 I=1,NR                      !COPY ANY ECL-TYPES
      IF(PV(I).EQ.0) GO TO 20           !TST FOR ECL-TYPE
      NDX=PV(I)                         !GET INDEX IN VBUF
      VN(I)=VBUF(NDX)                   !SAVE IN NEW-VALUE ARRAY
   20 CONTINUE
      GO TO 50
C
   30 DO 40 I=1,NR                      !GENERATE RANDOM VALUES
      VN(I)=9200.0+1000*RAN(ISEED)     !FOR TEST ONLY
   40 CONTINUE
C
   50 DO 60 I=1,NR                      !COMPUTE INCREMENTS
      VDF(I)=VN(I)-VO(I)                !SAVE IN DIFFERENCE ARRAY
   60 CONTINUE
C
      IF(NORI.EQ.0) GO TO 100           !TST FOR NO NORMALIZATION
      IF(NORI.GT.0) GO TO 80            !TST FOR NORM TO SCALER 
C
      DO 70 I=1,NR                      !OTHERWISE, NORM TO CTS/SEC
      VDF(I)=VDF(I)/SEC                 !VIA INTERNAL CLOCK
   70 CONTINUE
      GO TO 100  
C
   80 DENO=VDF(NORI)                    !COMPUTE NORMALIZATION FACTOR
      IF(DENO.LT.1.0) DENO=1.0          !FROM SPECIFIED SCALER
      VDNOR=NORF/DENO
C
      DO 85 I=1,NR                      !LOOP TO NORMALIZE
      VDF(I)=VDNOR*VDF(I)               !DIFFERENCES (RATES)
   85 CONTINUE
C
  100 CALL COMPUM                       !DO ANY "COMPUTED VALUES"
C
      RETURN
      END
C$PROG RESETX    - Resetx window dimensions to 24 x 80
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE RESETX(LU,ISET)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SD04/ SNITFIL(20)
      INTEGER*4    SNITFIL
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
C
      CHARACTER*4  ISET
C
      SAVE
C
C     ------------------------------------------------------------------
C
      ISET='NO  '
C
      MAXR=24
      MAXC=80
C
      CALL LOGOPEN(LU,SNITFIL,IERR)
C
      CALL SETUP(ISET)
C
      RETURN
C
      END
C$PROG SCADMSG   - Displays message & provides for abort
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/31/2004
C     ******************************************************************
C
      SUBROUTINE SCADMSG
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
      CHARACTER*4   ITST
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL DINGER(4)
C
      WRITE(6,10)
      WRITE(6,20)
      WRITE(6,30)
      WRITE(6,40)
      WRITE(6,50)
      WRITE(6,60)
      WRITE(6,10)
C
   10 FORMAT(
     &'===============================================================')
C
   20 FORMAT('This is a new version of scad'/)
   30 FORMAT('It supports features of earlier scad, scadd & scudd'/)
   40 FORMAT('These are available via the names',
     &' oldscad, oldscadd & oldscudd'/)
   50 FORMAT('Type: [RETURN] to continue with this one or'/)
   60 FORMAT('Type: anything else to abort')
C
      READ(5,70)ITST
   70 FORMAT(A)
C
      IF(ITST.NE.' ') STOP
C
      RETURN
      END
C$PROG SCADNIT   - Initializing routine for SCAD
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/22/2005
C     ******************************************************************
C
      SUBROUTINE SCADNIT
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
      COMMON/HEP/  IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C
      DATA         MAXR, MAXC, MAXD, MAXT, MAXS/24,80,0,500,50/
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD13/ ISET,LULG
      CHARACTER*4  ISET
      INTEGER*4         LULG
C
      DATA              LULG/15/
C     ------------------------------------------------------------------
      COMMON/SD15/ LUAL,THUSH,TNOW,ALFLG
      INTEGER*4    LUAL,THUSH,TNOW
      CHARACTER*4                  ALFLG
C     ------------------------------------------------------------------
      COMMON/SCD1/ GNITNAM
      CHARACTER*80 GNITNAM
C
      DATA         GNITNAM/'Undefined'/
C     ------------------------------------------------------------------
C
      CHARACTER*4  CIWD(20)
      EQUIVALENCE (CIWD,IWD)
C
      DATA         NAMPROG/'scad','    '/
C
      INTEGER*4    IERR,STAT,IOS,LHEP
C
      CHARACTER*24 NAMHEP
C
      DATA         LHEP/16/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
C
      CMSSG=' '
C
      NAMHEP='scad.hep'
C
      MSGF='    '
      LISFLG='LON '
      LOGUT=6
      LOGUP=7
C
      CALL COMNIT
C
C     ------------------------------------------------------------------
C     Open log-file, help-file, etc
C     ------------------------------------------------------------------
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'scad.log',
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = IOS)
C
      CLOSE(UNIT=LOGUP,DISP='DELETE')
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'scad.log',
     &     STATUS     = 'NEW',
     &     IOSTAT     = IOS)
C
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   LOGUP=0
                   WRITE(LOGUT,20)
                   ENDIF
C
   20 FORMAT(1H ,'OUTPUT TO LOG-FILE IS DISABLED')
C
C
C     ------------------------------------------------------------------
C     Open alarm file
C     ------------------------------------------------------------------
C
      LUAL=11
      THUSH=0
      TNOW=0
      ALFLG='OFF '
      OPEN(UNIT   = LUAL,
     &     FILE   = 'scadalarm.dat',
     &     STATUS = 'UNKNOWN',
     &     RECL   = 12,
     &     ACCESS = 'DIRECT',
     &     IOSTAT = STAT)
C
      WRITE(LUAL,REC=1)TNOW
      CALL FLUSH(LUAL)
C
C     ------------------------------------------------------------------
C     Pick up snit-file name for tabular display
C     ------------------------------------------------------------------
C
   50 WRITE(6,55)
   55 FORMAT('Enter snit-filename for tabular display->',$)
      READ(5,60)IWD
   60 FORMAT(20A4)
      KMD=CIWD(1)
C
      IF(KMD.EQ.'end ') CALL EXIT
      IF(KMD.EQ.'END ') CALL EXIT
C
      CALL LOGOPEN(LULG,IWD,IERR)
      IF(IERR.NE.0) GO TO 50
C
      CALL CTCNIT
C
      CALL CTBSNIT
C
      KMD='NORT'
      CALL NORMAN
C
      ISET='NEW '
C
      CALL SETUP(ISET)
C
C     ------------------------------------------------------------------
C     Open helpfile
C     ------------------------------------------------------------------
C
      CALL HELPOPEN(LHEP,NAMHEP,IHEPF)
C
      IF(IHEPF.NE.'YES ') RETURN
C
      WRITE(LOGUT,100)
      WRITE(LOGUT,110)
      WRITE(LOGUT,120)
  100 FORMAT(1H ,
     &'Type: h       - for list of HELP code-words & subjects')
  110 FORMAT(1H ,
     &'Type: h all   - for a more detailed help directory')
  120 FORMAT(1H ,
     &'Type: h code  - for command list for associated subject')
C
C
      RETURN
      END
C$PROG SCLR      - Clears display screen for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE SCLR
C
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      CHARACTER*4  KTERM
C     ------------------------------------------------------------------
C
      INTEGER*4 ICLR(2),NBY(2)
C
      DATA ICLR/'00002B1B'X,'4A325B1B'X/
C
      DATA NBY/2,4/
C
      SAVE
C
C     ------------------------------------------------------------------
C     CLEARS TERMINAL SCREEN
C     ------------------------------------------------------------------
C
      IDX=1
      IF(KTERM.EQ.'ANSI') IDX=2
C
      CALL SENDBUF(6,ICLR(IDX),NBY(IDX))
C
      RETURN
      END
C$PROG SCRNIT    - Clears SCAD screen & writes labels in proper location 
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 05/22/2005
C     ******************************************************************
C
      SUBROUTINE SCRNIT
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                               VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 KI
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      CHARACTER*4  KTERM
C     ------------------------------------------------------------------
      COMMON/SDXX/ ISKIP(512),SKIPLAB(8,512),NSKIP
      INTEGER*4    ISKIP,     SKIPLAB,       NSKIP
C
      CHARACTER*32 SKIPLABC(512)
      EQUIVALENCE (SKIPLABC,SKIPLAB)
C     ------------------------------------------------------------------
      INTEGER*4 STAT
C
      SAVE
C
C     ------------------------------------------------------------------
C     CLEARS SCREEN AND WRITES LABELS IN THEIR PROPER LOCATION
C     ------------------------------------------------------------------
C
      CALL SCLR
C
      NBY=4
      IF(KTERM.EQ.'ANSI') NBY=8
C
      N=0
      NSK=0
C
      DO 20 I=1,NT
C
      IF(KI(I).EQ.'NONE') GO TO 20
      N=N+1
C
      IF(ISKIP(N).NE.0) THEN
      CALL SENDBUF(6,LALOC(1,N),NBY)
      CALL SENDBUF(6,SKIPLAB(1,N),27)
      N=N+1
      ENDIF
C
      CALL SENDBUF(6,LALOC(1,N),NBY)
C
      CALL SENDBUF(6,LA(1,I),11)
C
   20 CONTINUE
C
      CALL DISP(0)
C
      RETURN
      END
C$PROG SENDBUF   - Sends buffer for SCAD display
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE SENDBUF(LU,IBUF,NBY)
C
      CHARACTER*1 IBUF(*)
C
      SAVE
C
      WRITE(LU,10)(IBUF(I),I=1,NBY)
CX 10 FORMAT('+',$,120A1)
   10 FORMAT($,120A)
      RETURN
      END
C$PROG SETECL    - Determines which scalers (in any) are ECL-type
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE SETECL
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 TY
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     SETECL - DETERMINES WHICH SCALERS (IF ANY) ARE ECL-TYPE AND
C     SETS UP AUXILIARY ARRAYS TO DIRECT THE READING OF SUCH SCALERS
C     ------------------------------------------------------------------
C
      NONEC=0
      DO 10 N=1,NR
      IF(TY(N).EQ.'ECL ') GO TO 10
      NONEC=NONEC+1
   10 CONTINUE
C
      NEC=0
      DO 50 N=1,NR
      PV(N)=0
C
      IF(TY(N).NE.'ECL ') GO TO 50
C
      NEI=0
      DO 20 I=1,NEC
      NEI=NEI+1
      IF(ECN(I).EQ.CN(N).AND.ESN(I).EQ.SN(N)) GO TO 40
   20 CONTINUE
C
      NEC=NEC+1
      ECN(NEC)=CN(N)
      ESN(NEC)=SN(N)
      PV(N)=32*(NEC-1)+A(N)+1+NONEC+NEC
      GO TO 50
C
   40 PV(N)=32*(NEI-1)+A(N)+1+NONEC+NEC
C
   50 CONTINUE
      RETURN
      END
C$PROG SETPAR    - Sets up C,N,A,F array for reading scalers via CAMLIST
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE SETPAR
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 TY
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD09/ CC(512),NN(512),AA(512),FF(512),VBUF(512),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     SETPAR - SETS UP C,N,A,F ARRAYS FOR READING ALL SCALERS
C     IN ONE FELL-SWOOP VIA CALL TO CAMLIST
C     ------------------------------------------------------------------
C
      EDAT='1FA0'X                      !ECL ENABLE DATA
C
      N=0                               !INIT DCW COUNTER
C
C     ------------------------------------------------------------------
C     SET UP LISTS FOR NON-ECL SCALERS (READ 1 AT A TIME)
C     ------------------------------------------------------------------
C
      DO 40 I=1,NR                      !LOOP ON # SCALERS TO READ
C
      IF(TY(I).EQ.'ECL ') GO TO 40      !SKIP IF ECL-TYPE
C
      N=N+1                             !INC ARRAY INDEX
C
      CC(N)=CN(I)                       !CRATE#
      NN(N)=SN(I)                       !SLOT#
      AA(N)=A(I)                        !SUB-ADDRESS
      FF(N)=0                           !F(0) READ
C
   40 CONTINUE
C
C     ------------------------------------------------------------------
C     SET UP LISTS FOR ECL SCALERS - READ 32 AT A TIME
C     ------------------------------------------------------------------
C
      DO 50 I=1,NEC                     !LOOP ON # ECL SCALER MODS
C
      N=N+1                             !INC DCW-CNTR
C
      CC(N)=ECN(I)                      !CRATE#
      NN(N)=ESN(I)                      !SLOT#
      AA(N)=0                           !A(0)
      FF(N)=16                          !F(16)
      VBUF(N)=EDAT                      !DATA TO ENABLE ECL READ
C
      DO 45 J=1,32                      !LOOP TO READ ALL 32
      N=N+1                             !INC DCW-CNTR
      CC(N)=ECN(I)                      !CRATE#
      NN(N)=ESN(I)                      !SLOT#
      AA(N)=0                           !A(0)
      FF(N)=2                           !F(2)
   45 CONTINUE      
C
   50 CONTINUE
      NLIST=N
      RETURN
      END
C$PROG SETUP     - Screen setup for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 05/22/2005
C     ******************************************************************
C
      SUBROUTINE SETUP(ISETX)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,
     &                               VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 TY,KI
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      CHARACTER*4  KTERM
C     ------------------------------------------------------------------
      COMMON/SD09/ CC(512),NN(512),AA(512),FF(512),VBUF(512),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
C     ------------------------------------------------------------------
      COMMON/SD13/ ISET,LULG
      CHARACTER*4  ISET
      INTEGER*4         LULG
C     ------------------------------------------------------------------
      COMMON/SDXX/ ISKIP(512),SKIPLAB(8,512),NSKIP
      INTEGER*4    ISKIP,     SKIPLAB,       NSKIP
C
      CHARACTER*32 SKIPLABC(512)
      EQUIVALENCE (SKIPLABC,SKIPLAB)
C     ------------------------------------------------------------------
      INTEGER*4    WINCOL,WINROW,IERR
C
      CHARACTER*4  ISETX,NORMF,ITMP,LASTYP
C
      CHARACTER*320 CLWDL
      CHARACTER*4   CLWD(2,40),CIWD(20)
      EQUIVALENCE  (CLWD,LWD),(CIWD,IWD),(CLWDL,LWD)
C
      CHARACTER*1   COMBYT
      EQUIVALENCE  (COMBYT,IWD)
C
      CHARACTER*32  COMLAB
      EQUIVALENCE  (COMLAB,IWD)
C
      REAL*4       XV
C
      INTEGER*4    LOCOD(2),COLWID
C
      BYTE         BYCOD(4,2),BTMP(4)
C
      EQUIVALENCE (BYCOD,LOCOD),(BTMP,ITMP)
C
      INTEGER*4    X20
      DATA         X20/'20'X/
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     SETUP - READS THE SCA-FILE AND SETS UP THE REQUIRED ARRAYS, ETC
C     NEEDED TO DIRECT THE READING AND DISPLAY
C     ------------------------------------------------------------------
C     LA(I,J)  = LABEL  FOR SCALER-J
C     CN(J)    = CRATE# FOR SCALER-J
C     SN(J)    = SLOT#  FOR SCALER-J
C      A(J)    = SUBAD  FOR SCALER-J
C      F(J)    = F-CLR  FOR SCALER-J
C     TY(J)    = TYPE   FOR SCALER-J  ('ECL ' OR '    ')
C     KI(J)    = KIND   FOR SCALER-J  ('    ' OR 'FLOT' OR 'NONE')
C     VN(J)    = NEW-V  FOR SCALER-J
C     VO(J)    = OLD-V  FOR SCALER-J
C     VD(J)    = VN-VO  FOR SCALER-J
C     PV(J)    = V-PNTR FOR SCALER-J  (POINTS TO VALUE IN VBUF)
C     LO(J)    = C-PNTR FOR SCALER-J  (FIRST ENTRY IN POL-OPR FOR CAL)
C     HI(J)    = C-PNTR FOR SCALER-J  (LAST  ENTRY IN POL-OPR FOR CAL)
C
C     NR       = # OF RAW SCALERS
C     NC       = # OF CAL SCALERS
C     NT       = TOTAL # OF SCALERS (NR+NC)
C     NDD      = TOTAL # SCALERS TO BE DISPLAYED
C
C     VBUF     = VALUE-BUFFER FOR ECL SCALERS (READ 32/CNAF)
C     POL      - POINTS TO SYMBOLS REFERENCED IN SCALER COMPUTATION
C     OPR      - OPERAND LIST ASSOCIATED WITH REFERENCED SYMBOLS
C
C     ECN(K)   = CRATE# FOR ECL SCALER-K
C     ESN(K)   = SLOT#  FOR ECL SCALER-K
C     NEC      = NUMBER OF  ECL SCALERS
C
C     LALOC(J) = PRE-CODED SCREEN LOCATION FOR SCALER-J LABEL
C     DALOC(J) = PRE-CODED SCREEN LOCATION FOR SCALER-J DATA
C
C     COLWID   = Col-width for scaler (name, count, rate)
C     ------------------------------------------------------------------
C
      CALL WINSIZE(WINCOL,WINROW,IERR)
C
      IADD=0
      IF(NLIM.GT.0) IADD=2
      MAXR=WINROW-NLIM-IADD-2
      MAXC=WINCOL
C
      ISETX='NO  '
      LASTYP=' '
      N=0
      NR=0
      NC=0
      NT=0
      NDD=0
      NSKIP=0
      LINO=0
      COLWID=31
C
      MAXCC=(MAXC+1)/COLWID
      MAXD=MAXR*MAXCC-1
      MAXDD=MAXD+1
C
      DO 20 J=1,500
      DO 10 I=1,3
      LA(I,J)='20202020'X
   10 CONTINUE
      KI(J)='    '
      TY(J)='    '
      SKIPLABC(J)=' '
      ISKIP(J)=0
   20 CONTINUE
C
   50 LINO=LINO+1
C
      READ(LULG,55,END=210)IWD
   55 FORMAT(20A4)
C
      IF(CIWD(1).EQ.'$END') GO TO 200
C
      IF(CIWD(1).EQ.' ') THEN
      IF(LASTYP.NE.' ') GO TO 330
      ISKIP(NSKIP+N+1)=1
      NSKIP=NSKIP+1
      LASTYP='HED'
      GO TO 50
      ENDIF
C
      IF(COMBYT.EQ.'#') THEN
      IF(LASTYP.NE.' ') GO TO 330
      ISKIP(NSKIP+N+1)=1
      SKIPLABC(NSKIP+N+1)=COMLAB
      NSKIP=NSKIP+1
      LASTYP='HED'
      GO TO 50
      ENDIF
C
      LASTYP=' '
C
      IA=NXBL(IWD,1,80)
      IF(IA.GT.12) GO TO 300
      IF(IA.LE.0)  GO TO 300
C
      N=N+1
      IF(N.GT.MAXT) GO TO 310
C
      CALL LODUP(IWD,1,IA,LA(1,N),1)
C
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER)
      IF(NTER.NE.0) GO TO 300
C
      READ(CLWDL,60)CN(N),SN(N),A(N),F(N),TY(N)
   60 FORMAT(4I8,A4)
C
      IA=IFIND(IWD,'3B'X,1,80)
C
      IF(IA.LE.0) THEN
                  NDD=NDD+1
                  IF(NDD+NSKIP.GT.MAXD) GO TO 320
                  GO TO 50
                  ENDIF
C
      IA=IA+1
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER)
      IF(NTER.NE.0) GO TO 300
C
      NORMF='NO  '
      DO 70 I=1,NF
      IF(CLWD(1,I).EQ.'NOD ') KI(N)='NONE'
      IF(CLWD(1,I).EQ.'NOR ') THEN
                              NORI=N
                              NORMF='YES '
                              NORF=1.0
                              ENDIF
   70 CONTINUE
C
      IF(KI(N).NE.'NONE') NDD=NDD+1
      IF(NDD+NSKIP.GT.MAXD) GO TO 320
C
      IF(NORMF.EQ.'NO  ') GO TO 50
C
      DO 80 I=1,NF
      IF(ITYP(I).NE.2) GO TO 80
      CALL MILV(LWD(1,I),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 300
      NORF=XV
   80 CONTINUE
C
      GO TO 50
C
  200 NR=N
      NT=NR
      CALL COMSET
C
      GO TO 220
C
  210 NR=N
      NT=N
C
  220 NROW=0
      NCOL=1
C
      IF(KTERM.EQ.'ANSI') GO TO 250
C
      DO 230 I=1,MAXDD
      NROW=NROW+1
      IF(NROW.GT.MAXR) THEN
                       NROW=1
                       NCOL=NCOL+COLWID
                       ENDIF
C
      LALOC(1,I)='00003D1B'X
      DALOC(1,I)='00003D1B'X
C
      IT=X20+NROW-1
      CALL ISBYTE(IT,LALOC(1,I),2)
      IT=X20+NCOL-1
      CALL ISBYTE(IT,LALOC(1,I),3)
      IT=X20+NROW-1
      CALL ISBYTE(IT,DALOC(1,I),2)
      IT=X20+NCOL+11-1
      CALL ISBYTE(IT,DALOC(1,I),3)
  230 CONTINUE
      GO TO 290
C
  250 DO 270 I=1,MAXDD
      NROW=NROW+1
      IF(NROW.GT.MAXR) THEN
                       NROW=1
                       NCOL=NCOL+COLWID
                       ENDIF
C
      LOCOD(1)='00005B1B'X
      LOCOD(2)='4800003B'X
C
      WRITE(ITMP,255)NROW
  255 FORMAT(I2.2)
      BYCOD(3,1)=BTMP(1)
      BYCOD(4,1)=BTMP(2)
      LALOC(1,I)=LOCOD(1)
      DALOC(1,I)=LOCOD(1)
C
      WRITE(ITMP,255)NCOL
      BYCOD(2,2)=BTMP(1)
      BYCOD(3,2)=BTMP(2)
      LALOC(2,I)=LOCOD(2)
C
      MCOL=NCOL+11
      WRITE(ITMP,255)MCOL
      BYCOD(2,2)=BTMP(1)
      BYCOD(3,2)=BTMP(2)
      DALOC(2,I)=LOCOD(2)
C
  270 CONTINUE
C
      LOCOD(1)='00005B1B'X
      LOCOD(2)='4800003B'X
      WRITE(ITMP,255)MAXR
      BYCOD(3,1)=BTMP(1)
      BYCOD(4,1)=BTMP(2)
C
      NKOL=1
      WRITE(ITMP,255)NKOL
      BYCOD(2,2)=BTMP(1)
      BYCOD(3,2)=BTMP(2)
      DATLOC(1)=LOCOD(1)
      DATLOC(2)=LOCOD(2)
C
      LOCOD(1)='00005B1B'X
      LOCOD(2)='4800003B'X
      WRITE(ITMP,255)MAXR+2
      BYCOD(3,1)=BTMP(1)
      BYCOD(4,1)=BTMP(2)
C
      NKOL=1
      WRITE(ITMP,255)NKOL
      BYCOD(2,2)=BTMP(1)
      BYCOD(3,2)=BTMP(2)
      HOMLOC(1)=LOCOD(1)
      HOMLOC(2)=LOCOD(2)
C
  290 CALL SETECL
C
      CALL SETPAR
C
      CLOSE(UNIT=1)
C
      ISETX='YES '
C
      RETURN
C
  300 WRITE(6,305)
  305 FORMAT(1H ,'SYNTAX ERROR')
      GO TO 400
C
  310 WRITE(6,315)MAXT
  315 FORMAT(1H ,'MORE THAN',I4,' RAW SCALERS REQUESTED')
      GO TO 400
C
  320 CALL DINGER(3)
      WRITE(6,325)MAXD,NLIM
  325 FORMAT(1H ,'Window too small to display ',I3,' scalers & ',I3,
     &           ' limits')
      GO TO 400
C
  330 CALL DINGER(3)
      WRITE(6,335)LINO
  335 FORMAT('Multiple Heading Lines defined on ',I3,' Not allowed')
      STOP
C
  400 RETURN
      END
C$PROG SNAP      - Logs scalers & rates to file scad.snap
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 05/22/2005
C     ******************************************************************
C
      SUBROUTINE SNAP
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
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD16/ PRNAM
      CHARACTER*76 PRNAM
C     ------------------------------------------------------------------
      COMMON/SDXX/ ISKIP(512),SKIPLAB(8,512),NSKIP
      INTEGER*4    ISKIP,     SKIPLAB,       NSKIP
C
      CHARACTER*32 SKIPLABC(512)
      EQUIVALENCE (SKIPLABC,SKIPLAB)
C     ------------------------------------------------------------------
      CHARACTER*4  KIN(512)
      EQUIVALENCE (KIN,KI)
C
      INTEGER*4    JV(3),KV(3),JV2(2),KV2(2)
      EQUIVALENCE (JV2,JV),(KV2,KV)
C
      REAL*4       VNF(512),VDF(512)
      EQUIVALENCE (VNF,VN),(VDF,VD)
C
      INTEGER*4    NAMF(20),DATIM(6),DATIME(5),JTIME(2),YR,MO,DA,LU,N
C
      INTEGER*4    STRAPPNS,STRAPPEND,JSTAT
C
      CHARACTER*80 CNAMF,LPRCMD
C
      EQUIVALENCE (CNAMF,NAMF)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      LU=37
C
      CALL MILYMDHMS(DATIM)
C
      CALL MILTIME(JTIME)
C
      YR=DATIM(1)
      MO=DATIM(2)
      DA=DATIM(3)
C
      CNAMF=' '
C
      WRITE(CNAMF,10)YR,MO,DA,JTIME
   10 FORMAT('scad-',I4.4,'-',I2.2,'-',I2.2,'_',2A4,'.snap')
C
C
      OPEN(UNIT       = LU,
     &     FILE       = CNAMF,
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = IOS)
C
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   WRITE(LOGUT,20)
                   RETURN
                   ENDIF
C
   20 FORMAT('OUTPUT TO SNAP-FILE NOT DONE!!')
C
C     ------------------------------------------------------------------
C
      CALL MILDATIM(DATIME)
C
      WRITE(LU,30)
   30 FORMAT('CURRENT SCALER VALUES ARE LISTED BELOW ')
      WRITE(LU,40)DATIME
   40 FORMAT('================= ',5A4)
C
      M=0
C
      DO 100 N=1,NT
C
      M=M+1
C
      IF(ISKIP(M).NE.0) THEN
      WRITE(LU,45)SKIPLABC(M)
   45 FORMAT(A)
      M=M+1
      ENDIF
C
      IF(KIN(N).NE.'FLOT') GO TO 50
C
      CALL FLO8(VNF(N),JV)
      CALL FLO8(VDF(N),KV)
      GO TO 60
C
   50 CALL FLI8(VN(N), JV)
      CALL FLO8(VDF(N),KV)
C
   60 WRITE(LU,70)(LA(J,N),J=1,3),JV2,KV2
   70 FORMAT(2A4,A3,1X,2A4,2X,2A4)
C
  100 CONTINUE
C
      WRITE(LU,105)
  105 FORMAT('END SCALER LIST')
C
      WRITE(LU,40)DATIME
C
      CLOSE(UNIT=LU)
C
      LPRCMD='enscript -fCourier-Bold8'
C
      IF(PRNAM.NE.' ') THEN
      ISTAT=STRAPPNS(LPRCMD,'-P')
      ISTAT=STRAPPEND(LPRCMD,PRNAM)
      ENDIF
C
      ISTAT=STRAPPNS(LPRCMD,CNAMF)
C
      CALL SYSTEM(LPRCMD,ISTAT)
C
      IF(ISTAT.NE.0) THEN
      CALL SENDBUF(6,DATLOC,8)
      WRITE(6,110)ISTAT
  110 FORMAT(/,'SNAP ERROR - LPR STAT =',Z8)
      CALL WAIT(2,2,JSTAT)
      CALL SENDBUF(6,DATLOC,8)
      WRITE(6,115)
  115 FORMAT(/,'                                   ')
      RETURN
      ENDIF
C
      CALL SENDBUF(6,DATLOC,8)
C
      WRITE(6,120)
  120 FORMAT(/,'SNAP DONE***********')
      CALL WAIT(1,2,JSTAT)
C
      RETURN
      END
C$PROG STATMAN   - Status display routine for SCAD
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 11/10/99
C     ******************************************************************
C
      SUBROUTINE STATMAN
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
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
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
      COMMON/SCD1/ GNITNAM
      CHARACTER*80 GNITNAM
C     ------------------------------------------------------------------
      COMMON/SCD2/ MSPRED,DISPSEC
      INTEGER*4    MSPRED,DISPSEC
C     ------------------------------------------------------------------
      COMMON/SD04/ SNITFIL(20)
      INTEGER*4    SNITFIL
C
      CHARACTER*80 SNITNAM
      EQUIVALENCE  (SNITNAM,SNITFIL)
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
      INTEGER*4    I,J,N
C
      CHARACTER*4  STATE
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     Display tabular related status
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 FORMAT('===================================================',
     &       '=============')
C
  102 FORMAT('---------------------------------------------------',
     &       '-------------')
C
      WRITE(CMSSG,105)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
  105 FORMAT('TABULAR DISPLAY INFORMATION')
C
      WRITE(CMSSG,115)SNITNAM
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,120)SEC
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,125)LSEC
      CALL MESSLOG(LOGUT,LOGUP)
C
  115 FORMAT('Snit-file name   = ',A)
  120 FORMAT('Display interval = ',I5,' seconds')
  125 FORMAT('Log     interval = ',I5,' seconds')
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,130)
  130 FORMAT('NAME            C   N   A   F')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 150 N=1,NR
      WRITE(CMSSG,140)(LA(I,N),I=1,3),CN(N),SN(N),A(N),F(N)
  140 FORMAT(3A4,1X,4I4)
      CALL MESSLOG(LOGUT,LOGUP)
  150 CONTINUE
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Display tabular-limit related status
C     ------------------------------------------------------------------
C
      IF(NLIM.LE.0) GO TO 200
C
      WRITE(CMSSG,160)
  160 FORMAT('Tabular Limit Specifications    LOW        HIGH')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 170 J=1,NLIM
      N=SCNDX(J)
      WRITE(CMSSG,165)(LA(I,N),I=1,3),LIMLO(J),LIMHI(J)
  165 FORMAT(3A4,11X,2F12.1)
      CALL MESSLOG(LOGUT,LOGUP)
  170 CONTINUE
C
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Display graphic related status
C     ------------------------------------------------------------------
C
  200 IF(NSCA.LE.0) RETURN
C
      WRITE(CMSSG,210)
  210 FORMAT('GRAPHIC DISPLAY INFORMATION')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,220)GNITNAM
  220 FORMAT('Gnit-file name = ',A)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,230)DISPSEC
  230 FORMAT('Displays/sec   = ',I2)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,240)
  240 FORMAT('NAME            C   N   A   F  NAGV  STATE')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 300 N=1,NSCA
C
      STATE=' OFF'
C
      IF(RATTYP(N).EQ.'SCAL') STATE='  ON'
C
      WRITE(CMSSG,250)(LAG(I,N),I=1,3),CC(N),NN(N),AA(N),FF(N),
     &                RATAVG(N),STATE
  250 FORMAT(3A4,1X,4I4,I6,3X,A)
      CALL MESSLOG(LOGUT,LOGUP)
C
  300 CONTINUE
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Display Meter limit related status
C     ------------------------------------------------------------------
C
CX    IF(NLIMG.LE.0) RETURN
C
      WRITE(CMSSG,310)
  310 FORMAT('Meter Limit Specifications      LOW        HIGH')
C
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 320 J=1,NSCA
      WRITE(CMSSG,315)(LAG(I,J),I=1,3),GLIMLO(J),GLIMHI(J),GLIMON(J)
  315 FORMAT(3A4,11X,2F12.1,2X,A4)
      CALL MESSLOG(LOGUT,LOGUP)
  320 CONTINUE
C
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
      RETURN
C
      END
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
C$PROG TABO      - Lists arrays for diagnostics only for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE TABO
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD09/ CC(512),NN(512),AA(512),FF(512),VBUF(512),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     THIS ROUTINE IS FOR DIAGNOSTICS ONLY
C     NORMALLY LOGICAL UNIT-7 IS ASSIGNED TO NULL:
C     ------------------------------------------------------------------
C
      DO 20 I=1,NT
C
      WRITE(7,10)(LA(J,I),J=1,3),CN(I),SN(I),A(I),F(I),TY(I),KI(I),
     &                           VN(I),VO(I),VD(I),PV(I),LO(I),HI(I)
   10 FORMAT(1H ,3A4,4I5,2X,A4,2X,A4,6I5)
   20 CONTINUE
C
      DO 50 I=1,NEC
      WRITE(7,30)ECN(I),ESN(I)
   30 FORMAT(1H ,2I5)
   50 CONTINUE
C
      DO 120 I=1,NPO
      WRITE(7,110)POL(I),GOL(I)
  110 FORMAT(1H ,2I10)
  120 CONTINUE
C
      DO 150 I=1,NLIST
      WRITE(7,140)CC(I),NN(I),AA(I),FF(I),VBUF(I)
  140 FORMAT(1H ,'C,N,A,F,DAT =',4I8,Z10)
  150 CONTINUE  
      RETURN
      END
C$PROG ZOTUM     - Zeros all scalers for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE ZOTUM
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
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 TY
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
C
      CHARACTER*4  KXQ
C
      SAVE
C
C     ------------------------------------------------------------------
C     ZERO THE SCALERS
C     ------------------------------------------------------------------
C
      DO 10 I=1,NT
      VN(I)=0
      VO(I)=0
      VD(I)=0
   10 CONTINUE
C
      DO 50 I=1,NR
      if ((cn(i).eq. 0) .and. 
     &    (sn(i).eq. 0) .and. 
     &    (a(i).eq.0)) then
          goto 50
      endif
      IF(TY(I).EQ.'ECL ') GO TO 50
      CALL BHIO(1,LCA,CN(I),SN(I),A(I),9,IDUM,0,STAT)
      IF(STAT.EQ.0.OR.STAT.EQ.1) GO TO 50
      KXQ='????'
      IF(STAT.EQ.2) KXQ='X   '
      IF(STAT.EQ.3) KXQ='X&Q '
      WRITE(CMSSG,20)KXQ,CN(I),SN(I),A(I)
   20 FORMAT('BAD-',A,' ZEROING SCALER - C,N,A =',3I4)
      CALL MESSLOG(LOGUT,LOGUP)
   50 CONTINUE
C
      IF(NEC.LE.0) RETURN
C
      DO 100 I=1,NEC
      CALL BHIO(1,LCA,ECN(I),ESN(I),0,16,'40'X,1,STAT)
      IF(STAT.EQ.0.OR.STAT.EQ.1) GO TO 100
      KXQ='????'
      IF(STAT.EQ.2) KXQ='X   '
      IF(STAT.EQ.3) KXQ='X&Q '
      WRITE(CMSSG,60)KXQ,ECN(I),ESN(I)
   60 FORMAT('BAD-',A,' ZEROING ECL SCALER - C,N =',2I4)
      CALL MESSLOG(LOGUT,LOGUP)
  100 CONTINUE
      RETURN
      END
