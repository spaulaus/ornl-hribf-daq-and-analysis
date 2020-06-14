C$PROG PROJALL   - Projects all bananas in a ban-file
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PROJALL
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      CHARACTER*4  CIWD(20),CLWD(2,40)
      EQUIVALENCE (CIWD,IWD),(CLWD,LWD)
C
      INTEGER*4    TITI(20),KPARI(9),FILI(6),MSG(7)
C
      INTEGER*4    IDIRB(880),KX(64),KY(64),LFIL(6),DG
C   
      INTEGER*4    MESS(8,4)
      CHARACTER*32 CMESS(4)
C   
      EQUIVALENCE (CMESS(1),MESS(1,1))
C   
      DATA CMESS/'ERROR READING BAN-FILE DIRECTORY',
     2           'FILENAME EXTENSION NOT .HIS     ',
     3           'ERROR FROM ROUTINE PROJ         ',
     4           'ERROR FROM ROUTINE SPKOUT       '/
C
      CHARACTER*4  IDONE,JEXT
C
      INTEGER*4    BLANK
      character*4  cBLANK
      equivalence  (cBLANK,BLANK)
C
      DATA         cBLANK/'    '/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO "PROJECT" A COMPLETE BAN-FILE USING HIS-FILENAMES
C     AND HIS-ID'S CONTAINED IN THE BAN-FILE ITSELF
C     ------------------------------------------------------------------
C     READ AND SAVE BAN-FILE DIR
C     ------------------------------------------------------------------
C   
      IERR=0
      CALL BANIO(5,20,FILI,TITI,IH,IDB,DG,IDIRB,KY,NP,NID,KPARI,
     &MSG,IERR)
C   
      IF(IERR.NE.0) GO TO 510
      NIDS=NID
C   
      DO 20 I=1,6
      LFIL(I)=BLANK
   20 CONTINUE
C   
C     ------------------------------------------------------------------
C     NOW DO ALL ID'S IN DIRECTORY - CHECKING HIS-FILENAME EACH TIME
C     ------------------------------------------------------------------
C   
      DO 200 K=1,NIDS
      IDB=IDIRB(K)
C   
      CALL BANIO(1,20,FILI,TITI,IH,IDB,DG,KX,KY,NP,NID,KPARI,
     &MSG,IERR)
C   
C                                           !CHECK FOR NEW HIS-FILE
      DO 30 I=1,6
      IF(FILI(I).NE.LFIL(I)) GO TO 40
   30 CONTINUE
      GO TO 100                             !IF NO, GO DO PROJECT
C   
C                                           !OTHERWISE, OPEN NEW FILE
C   
   40 IA=IFIND(FILI,Z'2E',1,24)             !LOOK FOR "."
      IF(IA.LE.0) GO TO 520
      IB=NXBL(FILI,IA,24)-1
      IF(IB.LE.0) GO TO 520
      IS=IFIND(FILI,Z'2F',IA,IB)            !LOOK FOR "/"
      IF(IS.GT.0) IB=IS-1                   !DROP ANY ACT #
      JEXT='    '
      NXT=IB-IA+1
      IF(NXT.NE.4) GO TO 520
      CALL LODUP(FILI,IA,IB,JEXT,1)         !GET EXTENT
      IF(JEXT.NE.'.HIS'.AND.                !CHECK FOR .HIS
     &   JEXT.NE.'.his') GO TO 520          !OR .his EXTENSION
C   
      DO 50 I=1,20                          !CLEAR IWD
      CIWD(I)='    '
   50 CONTINUE
C   
      CALL LODUP(FILI,1,IB,CIWD,5)          !LOAD FILNAME IN IWD
      CIWD(1)='IN  '                         !SET KMD TO 'IN  '
      CLWD(1,1)='IN  '                       !SET KMD TO 'IN  '
C
      DO 60 I=1,20
      IWDRAW(I)=IWD(I)
   60 CONTINUE
C   
      IDONE='    '
      CALL MANOPEN(IDONE,IERR)              !OPEN HIS- & DIR-FILES
      IF(IERR.NE.0) RETURN                  !TST FOR ERROR
C   
  100 MID=IDIRB(K)
      DEGA=DG
      CALL PROJE(IH,MID,DEGA,IERR)          !DO THE "MIGHTY" PROJ
      IF(IERR.NE.0) GO TO 530
      CALL SPKOUT(1,IERR)                   !AND WRITE IT OUT
      IF(IERR.NE.0) GO TO 540
C   
      DO 110 I=1,6                          !SAVE HIS-FILE NAME
      LFIL(I)=FILI(I)
  110 CONTINUE
  200 CONTINUE
      RETURN
C   
  510 JJ=1
      GO TO 600
  520 JJ=2
      GO TO 600
  530 JJ=3
      GO TO 600
  540 JJ=4
C   
  600 WRITE(CMSSG,610)(MESS(I,JJ),I=1,8)
      CALL MESSLOG(LOGUT,LOGUP)
  610 FORMAT(8A4)
      RETURN
      END
