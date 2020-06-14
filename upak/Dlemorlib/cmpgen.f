C$PROG CMPGEN    - Command processor for LEMOR - General
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CMPGEN(IDONE,IERR)
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      COMMON/LM06/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF(16384,2,3)
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2                                OUBUF
C     ------------------------------------------------------------------
      COMMON/LM15/ LPHED(64),NDX(3)
      INTEGER*4    LPHED,    NDX
C     ------------------------------------------------------------------
      INTEGER*4    NAMHEP(6),IHELP(20,400)
      CHARACTER*24 CNAMHEP
      EQUIVALENCE  (CNAMHEP,NAMHEP)
C     ------------------------------------------------------------------
      INTEGER*4    NAMBAN(20),LBAN
      CHARACTER*80 CNAMBAN
      EQUIVALENCE (CNAMBAN,NAMBAN)
      DATA         LBAN/27/
C     ------------------------------------------------------------------
      INTEGER*4    IERR,NCALL,LHEP
C
      INTEGER*4    NXNB,LSNB
C
      INTEGER*4    NPARU,IA,IB,NB,KS,ISTAT,I
C
      INTEGER*4    NAML(20)
      CHARACTER*80 CNAML
      EQUIVALENCE (CNAML,NAML)
C
      INTEGER*4    RECLVALU
C
      CHARACTER*4  IDONE,KMD
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      DATA         NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
      IF(NCALL.GT.0) GO TO 10
C
      NCALL=1
      NPARU=0
      LHEP=8
   10 CONTINUE
C     
C     ------------------------------------------------------------------
C     Process commands
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'BAN ') GO TO 50
      IF(KMD.EQ.'BANZ') GO TO 55
C
      IF(KMD.EQ.'ELDF') GO TO 60
      IF(KMD.EQ.'STAT') GO TO 65
C
      IF(KMD.EQ.'MILF') GO TO 1600
      IF(KMD.EQ.'UCOM') GO TO 2700
C
      IF(KMD.EQ.'WO  ') GO TO 110
C
      IF(KMD.EQ.'H   ') GO TO 100
C
      IF(KMD.EQ.'GATE') GO TO 200
      IF(KMD.EQ.'GATZ') GO TO 200
      IF(KMD.EQ.'GAT ') GO TO 200
C
      IF(KMD.EQ.'END ') GO TO 5000
C
      RETURN
C
   50 IA=NXNB(IWDRAW,4,80)
      IF(IA.LE.0) GO TO 4010
      IB=LSNB(IWDRAW,IA,80)
      IF(IB.LE.0) GO TO 4010
      CALL LODUP(IWDRAW,IA,IB,NAMBAN,1)
      NB=IB-IA+1
      CALL ISBYTE(0,NAMBAN,NB)
C
      CALL FREG2(LBAN,CNAMBAN,IERR)
      GO TO 5500
C
   55 CALL ZFREG2()
      GO TO 5500
C
   60 CALL LDFNUOUT
      GO TO 5500
C
   65 CALL STATMAN
      GO TO 5500
C
  100 CALL HELPMANU(IWD,LHEP,IHELP,400,20,IHEPF)
      GO TO 5500
C
  110 WRITE(LOGUT,115)
  115 FORMAT(1H ,'Type: [RETURN] to continue ->',$)
      READ(5,120)I
  120 FORMAT(A4)
      GO TO 5500
C
  200 CALL GATEMAN
      GO TO 5500
C
C
C     ------------------------------------------------------------------
C     IT WAS A "MILF COMMAND", OPEN FILE, CALL INITUM, ETC
C     ------------------------------------------------------------------
C
 1600 CLOSE(UNIT=LML)
      LMLO='NO  '
      IA=NXNB(IWDRAW,5,80)
      IF(IA.LE.0) GO TO 4010
      IB=LSNB(IWDRAW,IA,80)
      IF(IB.LE.0) GO TO 4010
      DO 1602 I=1,20
      NAML(I)=0
 1602 CONTINUE
      CALL LODUP(IWDRAW,IA,IB,NAML,1)
C
      OPEN(UNIT       = LML,
     &     FILE       = CNAML,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     FORM       = 'UNFORMATTED',
     &     RECL       = RECLVALU(256),
     &     IOSTAT     = ISTAT)
C
      IF(ISTAT.EQ.0) GO TO 1610
C
      WRITE(CMSSG,1605)ISTAT
 1605 FORMAT('ERROR OPENING MIL-FILE - ISTAT = ',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 6000
C
 1610 LMLO='YES '
C
      CALL INITUM
C
      CALL UPRSCRUB
      CALL UPVSETUP(NPARU)
      CALL  PVSETUP(NPAR)
C
      DO 1620 KS=1,3
      OUPTR(KS)=0
      OOFSET(KS)=32768*(KS-1)
      NDX(KS)=1
 1620 CONTINUE
      GO TO 5500
C
C     ------------------------------------------------------------------
C     DELETE "UCOM", LEADING BLANKS AND CALL USERS CMD PROCESSOR
C     ------------------------------------------------------------------
C
 2700 CALL USEND(IWDRAW,IERR)
      IF(IERR.NE.0) GO TO 4000
      GO TO 5500
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 4000 WRITE(CMSSG,4005)
      CALL MESSLOG(LOGUT,LOGUP)
 4005 FORMAT('SYNTAX ERROR - COMMAND IGNORED')
      GO TO 6000
C
 4010 WRITE(CMSSG,4015)
      CALL MESSLOG(LOGUT,LOGUP)
 4015 FORMAT('ILLEGAL VALUE OR SYNTAX ERROR - CMD IGNORED')
      GO TO 6000
C
C     ------------------------------------------------------------------
C     END program
C     ------------------------------------------------------------------
C
 5000 CALL CKCLI('DMOU')
      CALL CKCLO('DMOU',1)
      CALL CKCLO('DMOU',2)
      CALL CKCLO('DMOU',3)
      STOP
C
C     ------------------------------------------------------------------
C     NORMAL RETURN
C     ------------------------------------------------------------------
C
 5500 IDONE='YES '
      IERR=0
      RETURN
C
C     ------------------------------------------------------------------
C     ERROR RETURN
C     ------------------------------------------------------------------
C
 6000 IDONE='YES '
      IERR=1
      RETURN
      END
