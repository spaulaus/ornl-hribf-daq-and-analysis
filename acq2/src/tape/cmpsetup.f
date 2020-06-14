C$PROG CMPSETUP  - Command processor for LEMOR setup commands
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CMPSETUP(IDONE,IERR)
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
      COMMON/LM18/ MOCMO
      CHARACTER*4  MOCMO
C     ------------------------------------------------------------------
      COMMON/LM19/ JCNF,IREBF
      CHARACTER*4  JCNF,IREBF
C     ------------------------------------------------------------------
      COMMON/LM24/ LAUTO,INRECL
      CHARACTER*4  LAUTO
      INTEGER*4          INRECL
C     ------------------------------------------------------------------
      COMMON/LM29/ FMTI,FMTO
      CHARACTER*4  FMTI,FMTO
C     ------------------------------------------------------------------
      INTEGER*4    IERR
C
      INTEGER*4    IV,NPARU,KOUSIZ,KS
C
      CHARACTER*4  KMD,KMX,IDONE
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(1,2))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C     
C     ------------------------------------------------------------------
C     Process commands
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'HTIT') GO TO 140
      IF(KMD.EQ.'HNUM') GO TO 140
      IF(KMD.EQ.'HOUT') GO TO 140
C
      IF(KMD.EQ.'LON ') GO TO 145
      IF(KMD.EQ.'LOF ') GO TO 145
      IF(KMD.EQ.'RBON') GO TO 150
      IF(KMD.EQ.'RBOF') GO TO 155
C
      IF(KMD.EQ.'SWAB') GO TO 1720
      IF(KMD.EQ.'SWOF') GO TO 1730
      IF(KMD.EQ.'SHON') GO TO 1732
      IF(KMD.EQ.'SHOF') GO TO 1734
C
      IF(KMD.EQ.'FMTI') GO TO 1650
      IF(KMD.EQ.'FMTO') GO TO 1650
      IF(KMD.EQ.'L001') GO TO 1700
      IF(KMD.EQ.'L002') GO TO 1710
      IF(KMD.EQ.'L003') GO TO 1715
C
      IF(KMD.EQ.'UPOF') GO TO 1740
      IF(KMD.EQ.'UPON') GO TO 1750
      IF(KMD.EQ.'RECO') GO TO 1800
      IF(KMD.EQ.'RECI') GO TO 1820
C
      RETURN
C
  140 CALL HEDMAN(kmd,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
C
  145 LISFLG=KMD
      GO TO 5500
C
  150 IREBF='YES '
      GO TO 5500
  155 IREBF='NO  '
      GO TO 5500
C
C
C     ------------------------------------------------------------------
C     PROCESS  -  FMTI L001 NSKIP,MAXIP (SPECIFY L001 INPUT)
C              -  FMTI L002             (SPECIFY L002 INPUT)
C              -  FMTI L003             (SPECIFY L003 INPUT)
C              -  FMTO L001             (SPECIFY L001 OUTPUT)
C              -  FMTO L002             (SPECIFY L002 OUTPUT)
C              -  FMTO L003             (SPECIFY L003 OUTPUT)
C     ------------------------------------------------------------------
C
 1650 IF(KMX.EQ.'L001'.OR.
     &   KMX.EQ.'L002'.OR.
     &   KMX.EQ.'L003')    GO TO 1655
C
      GO TO 4000
C
 1655 IF(KMD.EQ.'FMTO') THEN
      FMTO=KMX
      GO TO 5500
      ENDIF
C
      IF(KMX.EQ.'L002'.OR.
     &   KMX.EQ.'L003')    THEN
      FMTI= KMX
      LFORM=KMX
      GO TO 5500
      ENDIF
C
      IF(NF.LT.4) GO TO 4010
      CALL LIMIV(LWD(1,3),0,500,NSKIP,IERR)
      IF(IERR.NE.0) GO TO 4010
      CALL LIMIV(LWD(1,4),1,500,MAXIP,IERR)
      IF(IERR.NE.0) GO TO 4010
      LFORM='L001'
      FMTI= 'L001'
      GO TO 5500

C 
C     ------------------------------------------------------------------
C     PROCESS  -  L001 NSKIP,MAXIP      (SPECIFY L001 INPUT)
C              -  L002                  (SPECIFY L002 INPUT)
C              -  L003                  (SPECIFY L003 INPUT)
C     ------------------------------------------------------------------
C
 1700 IF(NF.LT.3) GO TO 4010
      CALL LIMIV(LWD(1,2),0,500,NSKIP,IERR)
      IF(IERR.NE.0) GO TO 4010
      CALL LIMIV(LWD(1,3),1,500,MAXIP,IERR)
      IF(IERR.NE.0) GO TO 4010
      LFORM='L001'
      FMTI= 'L001'
      GO TO 5500
C
 1710 LFORM='L002'
      FMTI= 'L002'
      GO TO 5500
C
 1715 LFORM='L003'
      FMTI= 'L003'
      GO TO 5500
C
C     ------------------------------------------------------------------
C     PROCESS  -  SWAB               (REQUEST    BYTE-SWAP)
C              -  SWOF               (REQUEST NO BYTE-SWAP)
C     ------------------------------------------------------------------
C
 1720 ISWAB='YES '
      GO TO 5500
C
 1730 ISWAB='NO  '
      GO TO 5500
C
 1732 ISWAH='YES '
      GO TO 5500
C
 1734 ISWAH='NO  '
      GO TO 5500
C
 1740 MOCMO='UPOF'
      GO TO 5500
C
 1750 IF(NF.LT.2)   GO TO 1760
      CALL LIMIV(LWD(1,2),1,2000,NPARU,IERR)
      IF(IERR.NE.0) GO TO 4010
      IF(NF.LT.3)   GO TO 1760
      CALL LIMIV(LWD(1,3),1024,32768,LNBY,IERR)
      IF(IERR.NE.0) GO TO 4010
      LSTL=LNBY/2
 1760 CALL UPVSETUP(NPARU)
      CALL UPRSCRUB
      DO 1765 KS=1,3
      OUPTR(KS)=0
      OOFSET(KS)=32768*(KS-1)
      NDX(KS)=1
 1765 CONTINUE
      MOCMO='UPON'
      GO TO 5500
C
C     ------------------------------------------------------------------
C     PROCESS - RECO OUSIZ - SPECIFY OUTPUT-TAPE RECORD LENGTH
C     ------------------------------------------------------------------
C
 1800 KOUSIZ=16384
      IF(NF.EQ.1) GO TO 1805
C
      CALL LIMIV(LWD(1,2),2048,32768,IV,IERR)
      IF(IERR.NE.0) GO TO 4010
      KOUSIZ=2*(IV/4)
C
 1805 DO 1810 KS=1,3
      OUSIZ(KS)=KOUSIZ
      OUPTR(KS)=0
      OOFSET(KS)=32768*(KS-1)
      NDX(KS)=1
 1810 CONTINUE
      GO TO 5500
C
C     ------------------------------------------------------------------
C     Process - RECI RECL ;Specifies in-tape record length = RECL
C     Process - RECI      ;Says auto-detect in-tape data record length
C     ------------------------------------------------------------------
C
 1820 LAUTO='YES '
C
      IF(NF.EQ.1) GO TO 5500
      IF(NF.NE.2) GO TO 4010
      CALL LIMIV(LWD(1,2),2048,32768,IV,IERR)
      IF(IERR.NE.0) GO TO 4010
      INRECL=4*(IV/4)
      LAUTO='NO  '
      LNBY=INRECL
      LSTL=LNBY/2
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
