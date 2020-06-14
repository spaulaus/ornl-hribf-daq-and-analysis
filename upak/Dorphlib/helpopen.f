C$PROG HELPOPEN  - Opens filename.hep - does path serch
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE HELPOPEN(LU,NAME,IHEPF)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      BYTE          NAME(*)         !PATH/NAME SUPPLIED VIA ARG
C
      CHARACTER*4   IHEPF
C
      BYTE          X20,X00,X2F
C
      DATA          X20,X00,X2F/Z'20',Z'00',Z'2F'/
C
      INTEGER*4     PASSAV(20)      !FIRST PATH TO TRY
      INTEGER*4     PASNAM(20)      !FIRST PATH TO TRY
      BYTE          PASBYT(80)
      CHARACTER*80 CPASNAM,CPASSAV
      EQUIVALENCE (CPASNAM,PASNAM),(PASBYT,PASNAM),(CPASSAV,PASSAV)
C
      INTEGER*4     DFLPATH(15)     !DFLT PATH-NAME TO HELP-FILES 
      CHARACTER*60 CDFLPATH
      EQUIVALENCE (CDFLPATH,DFLPATH)
      DATA         CDFLPATH/
     &'/usr/hhirf/                                                 '/
C
      INTEGER*4     HOMPATH(15)     !HELP-FILE PATH-NAME READ FROM
      CHARACTER*60 CHOMPATH         !HOME-DIRECTORY 
      EQUIVALENCE (CHOMPATH,HOMPATH)
C
      INTEGER*4     HOMNAM(20)      !HOME-DIRECTORY NAME
      CHARACTER*80 CHOMNAM          !TO BE RETURNED BY "GETHOME"
      EQUIVALENCE (CHOMNAM,HOMNAM)  !IF "DFLPATH" FAILS
C
      INTEGER*4     HOMFIL(4)       !FILE-NAME IN HOME DIRECTORY
      CHARACTER*16 CHOMFIL          !WHICH CONTAINS PATH-NAME TO
      EQUIVALENCE (CHOMFIL,HOMFIL)  !HELP-FILES
      DATA         CHOMFIL/'/upak_help_path '/
C
      INTEGER*4     HEPFIL(20)      !FINAL CONSTRUCTED HELP-FILE
      CHARACTER*80 CHEPFIL          !NAME 
      EQUIVALENCE (CHEPFIL,HEPFIL)
C
      INTEGER*4     HEPFLA(20)      !FINAL CONSTRUCTED HELP-FILE
      CHARACTER*80 CHEPFLA          !NAME 
      EQUIVALENCE (CHEPFLA,HEPFLA)
C
      SAVE
C
C     ------------------------------------------------------------------
C     THIS ROUTINE DEFINES THE PATH TO HELP-FILES AND OPENS A
C     SPECIFIC HELP-FILE DEFINED BY "PASNAM"
C
C     TO CUSTOMIZE FOR YOUR LAB, CHANGE THE PATH NAME /usr/hhirf/
C     TO THE PATH APPROPRIATE FOR YOUR HELP FILES
C     ------------------------------------------------------------------
C
      IHEPF='NO  '               !SET SUCCESS-FLAG TO 'NO  '
C
C     ------------------------------------------------------------------
C     TRY TO OPEN HELP-FILE USING ONLY NAME PASSED IN PASNAM
C     ------------------------------------------------------------------
C
      CPASNAM=' '
C
      DO 10 I=1,80
      IF(NAME(I).EQ.X20) GO TO 15
      IF(NAME(I).EQ.X00) GO TO 15
      PASBYT(I)=NAME(I)
   10 CONTINUE
C
   15 CPASSAV=CPASNAM
C
      OPEN(UNIT       = LU,      !TRY TO OPEN USING PASSED NAME
     &     FILE       = CPASNAM,
     &     STATUS     = 'OLD',
     &     IOSTAT     = IOS)
C
      IF(IOS.EQ.0) THEN          !IF OPEN OK
                   IHEPF='YES '  !SET FLAG
                   RETURN        !AND RETURN
                   ENDIF
C
C     ------------------------------------------------------------------
C     Strip off any specific path name - up thru last "/"
C     ------------------------------------------------------------------
C
      LASSL=0
      DO 20 I=1,80
      IF(PASBYT(I).EQ.X2F) LASSL=I
   20 CONTINUE
C
      IF(LASSL.EQ.0) GO TO 30
C
      DO 25 I=1,LASSL
      PASBYT(I)=X20
   25 CONTINUE
C
      CALL SQUEZL(PASNAM,1,80)
C
C     ------------------------------------------------------------------
C     TRY TO OPEN HELP-FILE USING DEFAULT PATH
C     ------------------------------------------------------------------
C
   30 DO 35 I=1,15               !CONSTRUCT DEFAULT HELP-FILE NAME
      HEPFIL(I)=DFLPATH(I)       !LOAD DEFAULT-PATH PART
   35 CONTINUE
      DO 40 I=1,5
      HEPFIL(I+15)=PASNAM(I)     !LOAD SPECIFIC FILE-NAME PART
   40 CONTINUE
C
      CALL SQUEZL(HEPFIL,1,80)   !SQUEEZE OUT ANY BLANKS
C
      OPEN(UNIT       = LU,      !TRY TO OPEN USING DEFAULT PATH
     &     FILE       = CHEPFIL,
     &     STATUS     = 'OLD',
     &     IOSTAT     = IOS)
C
      IF(IOS.EQ.0) THEN          !IF OPEN OK
                   IHEPF='YES '  !SET FLAG
                   RETURN        !AND RETURN
                   ENDIF
C
C     ------------------------------------------------------------------
C     TRY TO OPEN HELP-FILE USING "upak_help_path" FILE IN THE
C     HOME DIRECTORY
C     ------------------------------------------------------------------
C
      CLOSE(UNIT=LU)
C                                !TRY TO OPEN UPAK_HELP_PATH
C
      CALL GETHOME(CHOMNAM,LEN)  !GET "HOME PATH NAME"
C
      IF(LEN.LE.0) GO TO 300     !TEST/REPORT FAILURE
C
      DO 50 I=1,4
      HOMNAM(I+16)=HOMFIL(I)     !ANNEX "upak_help_path"
   50 CONTINUE
C
      CALL SQUEZL(HOMNAM,1,80)   !SQUEEZE OUT ANY SPACES
C
      OPEN(UNIT       = LU,
     &     FILE       = CHOMNAM, !TRY TO OPEN IT
     &     STATUS     = 'OLD',
     &     IOSTAT     = IOS)
C
      IF(IOS.EQ.0) GO TO 80      !IF SUCCESS, GO TRY TO READ IT
C
      WRITE(LOGUT,55)
   55 FORMAT(1H ,
     &'Following files not found - Run-time help unavailable!')
      WRITE(LOGUT,60)PASSAV
      WRITE(LOGUT,60)HEPFIL
      WRITE(LOGUT,60)HOMNAM
   60 FORMAT(1H ,19A4,A3)
      RETURN
C
   80 READ(LU,85,END=200,ERR=200)HEPFLA
   85 FORMAT(20A4)
C
      DO 90 I=1,4                !ANNEX HELP-FILE NAME
      HEPFLA(I+16)=PASNAM(I)
   90 CONTINUE
C
      CALL SQUEZL(HEPFLA,1,80)   !SQUEEZE OUT ANY BLANKS 
C
      CLOSE(UNIT=LU)
C
      OPEN(UNIT       = LU,      !TRY TO OPEN IT
     &     FILE       = CHEPFLA,
     &     STATUS     = 'OLD',
     &     IOSTAT     = IOS)
C
      IF(IOS.EQ.0) THEN          !IF SUCCESS
                   IHEPF='YES '  !SET FLAG 
                   RETURN        !AND RETURN
                   ENDIF
C
      WRITE(LOGUT,55)
      WRITE(LOGUT,60)PASSAV
      WRITE(LOGUT,60)HEPFIL
      WRITE(LOGUT,60)HEPFLA
      RETURN
C
  200 WRITE(LOGUT,205)           !REPORT ANY READ OR EOF ERRORS
  205 FORMAT(1H ,
     &'Error reading following file - Run-time help unavailable!')
C
      WRITE(LOGUT,60)HEPFLA
      RETURN
C
  300 WRITE(LOGUT,305)
  305 FORMAT(1H ,'Unable to open following files:')
      WRITE(LOGUT,60)PASSAV
      WRITE(LOGUT,60)HEPFIL
      WRITE(LOGUT,310)
  310 FORMAT(1H ,'Unable to determine "home directory path"')
      WRITE(LOGUT,320)
  320 FORMAT(1H ,'Run-time help unavailable!')
      RETURN   
      END
