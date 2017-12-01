C$PROG SCATCMP
C
C     ******************************************************************
C     SCALER DISPLAY PROGRAM - FOR CAMAC SCALERS
C     ******************************************************************
C
      SUBROUTINE SCATCMP(JWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SCAT0/ SCATBUF(8000,2),NSCAT,SCATDMP,SCATCLR,SCATERR
      CHARACTER*4                         SCATDMP,SCATCLR
      INTEGER*4     SCATBUF,        NSCAT,                SCATERR
C
      DATA          NSCAT,SCATDMP,SCATCLR,SCATERR/0,'OFF ','CLR ',0/
C     ------------------------------------------------------------------
      real*8 vn, vo, vd
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
      CHARACTER*4   TY,KI
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SCATA/ MAXT,MAXS,SEC,ISET,NERR,MODEGO
      CHARACTER*4                 ISET,     MODEGO
      INTEGER*4     MAXT,MAXS,SEC,     NERR
C
      DATA          MAXT,MAXS,SEC,ISET,MODEGO/790,50,0,'NO  ','RUN '/
C     ------------------------------------------------------------------
      COMMON/SCATB/ READERR,ZEROERR,LOGERR,NDUMPS
      CHARACTER*4                   LOGERR
      INTEGER*4     READERR,ZEROERR,       NDUMPS
C
      DATA          READERR,ZEROERR,LOGERR,NDUMPS/0,0,'OFF ',0/
C     ------------------------------------------------------------------
      COMMON/SCATE/ NSECI
      INTEGER*4     NSECI
C
      DATA          NSECI/1000/
C     ------------------------------------------------------------------
C
      INTEGER*4     BUF(20,400)
      EQUIVALENCE  (BUF,SCATBUF)
C
      INTEGER*4     IWD(20),LWD(2,40),ITYP(40),NF
C
      CHARACTER*4   CLWD(2,40)
C
      EQUIVALENCE  (CLWD,LWD)
C
      INTEGER*4     NAMFI(20),IWDRAW(20),JWD(20)
      CHARACTER*80  CNAMFI
      CHARACTER*4   KMD
      EQUIVALENCE  (KMD,LWD(1,1)),(CNAMFI,NAMFI)
C
      INTEGER*4     TMIN,TMAX,LSNIT
      DATA          TMIN,TMAX,LSNIT/10,1000,1/
C
      INTEGER*4     IHELP(15,22)
      CHARACTER*60  CHELP(22)
      EQUIVALENCE  (CHELP,IHELP)
      DATA          CHELP/
     &'SCAT  filename      ;Initializes SCAT from filename         ',
     &'                                                            ',
     &'SCAT  ON            ;Dumps scalers before EOF only          ',
     &'SCAT  ON  CLR       ;Dumps scalers before EOF & clears post ',
     &'SCAT  ON  TSEC      ;Dumps scalers every TSEC & before EOF  ',
     &'SCAT  ON  TSEC  CLR ;Dumps scalers every TSEC & before EOF  ',
     &'SCAT  OFF           ;Disables scaler dumps                  ',
     &'                                                            ',
     &'SCAT  NORT          ;Normalize count rates to internal clock',
     &'SCAT  NORS          ;Normalize count rates as per snit-file ',
     &'SCAT  UNOR          ;Count rates un-normalized              ',
     &'                                                            ',
     &'SCAT  STAT          ;Displays/logs setup status             ',
     &'SCAT  HELP          ;Display on-line help                   ',
     &'SCAT  H             ;Display on-line help                   ',
     &'                                                            ',
     &'SCAT  LERR ON       ;Turns error display/logging ON         ',
     &'SCAT  LERR OFF      ;Turns error display/logging OFF        ',
     &'                                                            ',
     &'SCAT  ZERR          ;Zeros error counters                   ',
     &'SCAT  ZDUM          ;Zeros scaler dump counter              ',
     &'SCAT  ZERO          ;Zeros all scalers                      '/
C
C
      INTEGER*4  BLANK
C
      DATA       BLANK/'20202020'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 I=1,19
      IWD(I)=JWD(I+1)
      IWDRAW(I)=IWD(I)
   10 CONTINUE
      IWD(20)=BLANK
      IWDRAW(20)=BLANK
      CALL CASEUP(IWD)
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMD.EQ.'HELP') GO TO 80
      IF(KMD.EQ.'H   ') GO TO 80
      IF(KMD.EQ.'ON  ') GO TO 100
      IF(KMD.EQ.'OFF ') GO TO 120
      IF(KMD.EQ.'ZERR') GO TO 130
      IF(KMD.EQ.'ZDUM') GO TO 135
      IF(KMD.EQ.'LERR') GO TO 140
      IF(KMD.EQ.'ZERO') GO TO 150
      IF(KMD.EQ.'NORT') GO TO 160
      IF(KMD.EQ.'NORS') GO TO 160
      IF(KMD.EQ.'UNOR') GO TO 160
      IF(KMD.EQ.'STAT') GO TO 220
      IF(KMD.EQ.'    ') GO TO 1000
                        GO TO 300
C
   80 DO 90 J=1,22
      WRITE(CMSSG,85)(IHElP(I,J),I=1,15)
   85 FORMAT(15A4)
      CALL MESSLOG(LOGUT,0)
   90 CONTINUE
      RETURN
C
C
C     ******************************************************************
C     Process ON/OFF commands
C     ******************************************************************
C
  100 IF(ISET.NE.'YES ') GO TO 990
C
      CALL TIMEKEEP('GET ',NSECI)
C
      SCATDMP='NO  '
      SCATCLR='NO  '
      SEC=0
      IF(NTER.NE.0) GO TO 1000
C
      IF(NF.EQ.1) THEN
      SCATDMP='YES '
      RETURN
      ENDIF
C
      IF(NF.EQ.2.AND.CLWD(1,2).EQ.'CLR ') THEN
      SCATDMP='YES '
      SCATCLR='YES '
      RETURN
      ENDIF
C
      IF(NF.EQ.2) THEN
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1010
      IF(IV.LT.TMIN.OR.IV.GT.TMAX) GO TO 1020
      SEC=IV
      SCATDMP='YES '
      RETURN
      ENDIF
C
      IF(NF.EQ.3) THEN
      IF(CLWD(1,3).NE.'CLR ') GO TO 1000
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1010
      IF(IV.LT.TMIN.OR.IV.GT.TMAX) GO TO 1020
      SEC=IV
      SCATDMP='YES '
      SCATCLR='YES '
      RETURN
      ENDIF
C
      GO TO 1000
C
  120 SCATDMP='NO  '
      SCATCLR='NO  '
      SEC=0
      RETURN
C
C
C     ******************************************************************
C     Process ZERR and ZERO
C     ******************************************************************
C
  130 READERR=0
      ZEROERR=0
      RETURN
C
  135 NDUMPS=0
      RETURN
C
  140 LOGERR='OFF '
      IF(CLWD(1,2).EQ.'ON  ') LOGERR='ON  '
      RETURN
C
  150 CALL ZOTUM
      RETURN
C
  160 CALL NORMAN(KMD)
      RETURN
C
C     ******************************************************************
C     Process  -  STAT command
C     ******************************************************************
C
  220 IF(ISET.EQ.'YES ') THEN
      WRITE(CMSSG,225)(NAMFI(I),I=1,12)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(ISET.NE.'YES ') THEN
      WRITE(CMSSG,230)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(SCATDMP.EQ.'YES ') THEN
      IF(SEC.EQ.0) THEN
      WRITE(CMSSG,235)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
      ENDIF
C
      IF(SCATDMP.EQ.'YES ') THEN
      IF(SEC.GT.0) THEN
      WRITE(CMSSG,240)SEC
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
      ENDIF
C
C     IF(SCATDMP.EQ.'YES ') THEN
C     WRITE(CMSSG,245)
C     CALL MESSLOG(LOGUT,LOGUP)
C     ENDIF
C
      IF(SCATDMP.NE.'YES ') THEN
      WRITE(CMSSG,250)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(SCATDMP.EQ.'YES ') THEN
      IF(SCATCLR.EQ.'YES ') THEN
      WRITE(CMSSG,255)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
      ENDIF
C
      IF(SCATDMP.EQ.'YES ') THEN
      IF(SCATCLR.NE.'YES ') THEN
      WRITE(CMSSG,260)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
      ENDIF
C
      WRITE(CMSSG,265)LOGERR
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,270)READERR
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,275)ZEROERR
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,280)NDUMPS
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(NORI.LT.0)THEN
      WRITE(CMSSG,282)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(NORI.GT.0)THEN
      WRITE(CMSSG,284)NORF,(LA(I,NORI),I=1,3)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(NORI.EQ.0)THEN
      WRITE(CMSSG,286)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      RETURN
C
  225 FORMAT('SCAT initialized with file - ',12A4)     
  230 FORMAT('SCAT has not been properly initialized')
  235 FORMAT('SCAT set to dump before file-marks, only')
  240 FORMAT('SCAT set to dump at ',I4,' second intervals')
  245 FORMAT('SCAT dump is enabled')
  250 FORMAT('SCAT dump is disabled')
  255 FORMAT('SCAT set to zero scalers after each dump')
  260 FORMAT('SCAT not set to zero scalers after each dump')
  265 FORMAT('SCAT error logging is turned ',5X,A4)
  270 FORMAT('SCAT number of read errors  =',I8)
  275 FORMAT('SCAT number of zero errors  =',I8)
  280 FORMAT('SCAT number of scaler dumps =',I8)
  282 FORMAT('SCAT count rates normalized to internal clock')
  284 FORMAT('SCAT count rates normalized to ',F8.2,' times ',3A4)
  286 FORMAT('SCAT count rates unnormalized')
C
C
C     ******************************************************************
C     Process  -  SCAT filename 
C     ******************************************************************
C
  300 ISET='NO  '
      CLOSE(UNIT=LSNIT)
C
      DO 305 I=1,20
      NAMFI(I)=IWDRAW(I)
  305 CONTINUE
C
      CALL SQUEZL(NAMFI,1,80)
C
      IA=LSNB(NAMFI,1,76)
      IF(IA.LE.0) GO TO 1000
      IF(IA.GT.76) IA=76
      CALL ISBYTE(0,NAMFI,IA)
C
      OPEN(UNIT     = LSNIT,
     &     FILE     = CNAMFI,
     &     STATUS   = 'OLD',
     &     IOSTAT   = IERR)
C
      IF(IERR.NE.0) THEN
                    CALL IOFERR(IERR)
                    RETURN
                    ENDIF
C
      CALL SETUP(LSNIT)
      IF(ISET.NE.'YES ') GO TO 1050
C
      DO 310 I=1,8000
         SCATBUF(I,1)=BLANK
  310 CONTINUE

      DO 330 N=1,NT
         DO 320 I=1,3
            BUF(I,N+1)=LA(I,N)
  320    CONTINUE
  330 CONTINUE
*     write(6,*) "NT=", nt
*     do n=1,nt
*        write(6,"(i3,A,1x,':',3A4,':')") n,"LA=",(LA(i,n),i=1,3)
*        write(6,"(i3,A,1x,':',20A4,':')") n,"BUF=",(BUF(i,n+2),i=1,20)
*     enddo
C
      CALL TIMEKEEP('INIT',NSEC)
      RETURN
C
  990 WRITE(CMSSG,995)
  995 FORMAT('SCAT - FILENAME UNDEFINED - COMMAND IGNORED')
      GO TO 1500
C 
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('SCAT - SYNTAX ERROR  - COMMAND IGNORED')
      GO TO 1500
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('SCAT - ILLEGAL DISPLAY INTERVAL - LEGAL RANGE = 1,20')
      GO TO 1500
C
 1020 WRITE(CMSSG,1025) TMIN, TMAX
 1025 FORMAT('SCAT - ILLEGAL LOG INTERVAL - LEGALs = ',I4,'to ',I4)
      GO TO 1500
C
 1050 WRITE(CMSSG,1055)
 1055 FORMAT('SCAT - SCAD NOT PROPERLY INITIALIZED - TRY AGAIN')
C
 1500 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
