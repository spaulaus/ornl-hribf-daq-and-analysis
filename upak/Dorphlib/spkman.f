C$PROG SPKMAN    - Creates and/or opens spk-files for I/O
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE SPKMAN(MODE,NAMF,LUS,IACP,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      CHARACTER*4 JEX,CRW,CCREA
      INTEGER*4        RW, CREA
      EQUIVALENCE (CRW,RW),(CCREA,CREA)
      DATA         CRW,CCREA/'RW  ','CREA'/
C
      INTEGER*4    RECLVALU
      INTEGER*4    NAM(20),NAMF(20)
      CHARACTER*80 CNAM
      EQUIVALENCE (CNAM,NAM)
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO OPEN/CREATE SPK-FILES FOR INPUT/OUTPUT
C     ------------------------------------------------------------------
C
      IERR=0                                 !RESET ERROR FLAG
      CLOSE(UNIT=LUS)                        !CLOSE SPK-FILE
C
      LD=LEXT(NAMF,1,80)                     !LOCATE .SPK
      IF(LD.LE.0) GO TO 200                  !TST FOR ERROR
      JEX='    '
      LB=LD+3
      CALL LODUP(NAMF,LD,LB,JEX,1)           !PICK UP .EXT
      IF(JEX.EQ.'.SPK') GO TO 10             !TST FOR .SPK EXT
      IF(JEX.EQ.'.spk') GO TO 10             !OR  FOR .spk ext
                        GO TO 200            !OTHERWISE, ERROR
C
   10 DO 20 I=1,20                           !COPY FILENAME
      NAM(I)=NAMF(I)
   20 CONTINUE
C
      IF(MODE.EQ.CREA) GO TO 100
      IF(IACP.EQ.RW  ) GO TO 50
C
C     ------------------------------------------------------------------
C     OPEN EXISTING SPK-FILE
C     ------------------------------------------------------------------
C
      OPEN(UNIT       = LUS,                 !OPEN SPK-FILE
     &     FILE       = CNAM,                !FOR INPUT
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(2048),
     &     IOSTAT     = IOS)
C
      GO TO 150
C
   50 OPEN(UNIT       = LUS,                 !OPEN SPK-FILE
     &     FILE       = CNAM,                !FOR OUTPUT
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(2048),
     &     IOSTAT     = IOS)
C
  150 CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 220
      RETURN
C
C     ------------------------------------------------------------------
C     CREATE A NEW SPK-FILE AND OPEN FOR OUTPUT
C     ------------------------------------------------------------------
C
  100 OPEN(UNIT          = LUS,              !CREATE NEW SPK-FILE
     &     FILE          = CNAM,             !FOR OUTPUT
     &     STATUS        = 'NEW',
     &     ACCESS        = 'DIRECT',
     &     RECL          = RECLVALU(2048),
     &     IOSTAT        = IOS)
C
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 220
C
      CALL SPKIO(0,LUS,0,0,0,0,0,0,IERR)     !INIT SPK-FILE
      CALL SPKERR(IERR)                      !TST FOR ERROR
      RETURN
C
  200 WRITE(CMSSG,205)
  205 FORMAT('SYNTAX ERROR IN FILENAME OR EXTENSION NOT .SPK')
      CALL MESSLOG(LOGUT,LOGUP)
  220 IERR=1
      RETURN
      END
