C$PROG CMPSETUP  - Executes general & setup commands for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/04/99
C     ******************************************************************
C
      SUBROUTINE CMPSETUP(IDONE,RETN)
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
      COMMON/SC00/ IHEPF 
      INTEGER*4    IHEPF
C     ------------------------------------------------------------------
      COMMON/SC02/ NAMH(20)
      INTEGER*4    NAMH
C     ------------------------------------------------------------------
      COMMON/SC04/ JCNF,IHEDN,MBFL
      INTEGER*4         IHEDN,MBFL
      CHARACTER*4  JCNF
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC13/ LCON,LCMD,LIN,LBAN,LHEP
      INTEGER*4    LCON,LCMD,LIN,LBAN,LHEP
C     ------------------------------------------------------------------
      COMMON/SC14/ NBRED,NBTOP,ICNF
      INTEGER*4    NBRED,NBTOP
      CHARACTER*4              ICNF
C     ------------------------------------------------------------------
      COMMON/SC15/ NCEOF,LAUTO
      INTEGER*4    NCEOF
      CHARACTER*4        LAUTO
C     ------------------------------------------------------------------
      COMMON/SC22/ IPCNAM(20),LDFNAM(20),TAPNAM(20),BANNAM(20)
      INTEGER*4    IPCNAM,    LDFNAM,    TAPNAM,    BANNAM
C     ------------------------------------------------------------------
      COMMON/SC28/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC,CLONOF
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
      CHARACTER*4                                             CLONOF
C     ------------------------------------------------------------------
      INTEGER*4    IHELP(20,300)
C   
      CHARACTER*80 NAMLOG,CNAMBAN
C
      EQUIVALENCE (CNAMBAN,BANNAM)
C     ------------------------------------------------------------------
      INTEGER*4    RETN
C
      INTEGER*4    IA,IB,NB,IV,JV,LTST,IERR,JERR,NXNB,LSNB
C
      INTEGER*4    SETW,KIND
      character*4  csetw
      equivalence  (csetw,setw)
      DATA         cSETW/'SETW'/
C
      INTEGER*4    LWD3(3,40),ITYP3(40),NF3,NTER3
C
      REAL*4       XV
C
      CHARACTER*4  KMD,IDONE
      EQUIVALENCE (KMD,LWD(1,1))
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IDONE='NO  '
      RETN =0
C
      IF(KMD.EQ.'LON ') GO TO 125
      IF(KMD.EQ.'LOF ') GO TO 125
      IF(KMD.EQ.'MSG ') GO TO 2000
      IF(KMD.EQ.'UCOM') GO TO 700
C
      IF(KMD.EQ.'BAN ') GO TO 130
      IF(KMD.EQ.'BANZ') GO TO 135
C
      IF(KMD.EQ.'GATE') GO TO 150
      IF(KMD.EQ.'GAT ') GO TO 150
      IF(KMD.EQ.'GATZ') GO TO 150
C
      IF(KMD.EQ.'ZBUC') GO TO 210
C
      IF(KMD.EQ.'H   ') GO TO 120
      IF(KMD.EQ.'HELP') GO TO 120
C   
      IF(KMD.EQ.'RECL') GO TO 200
C
      IF(KMD.EQ.'STAT') GO TO 220
C
      IF(KMD.EQ.'L001') GO TO 950
      IF(KMD.EQ.'L002') GO TO 960
      IF(KMD.EQ.'L003') GO TO 960
      IF(KMD.EQ.'SWAB') GO TO 962
      IF(KMD.EQ.'SWOF') GO TO 964
C
      IF(KMD.EQ.'CLID') GO TO 980
      IF(KMD.EQ.'CLIM') GO TO 1000
      IF(KMD.EQ.'CLLO') GO TO 1100
      IF(KMD.EQ.'CLON') GO TO 1200
      IF(KMD.EQ.'CLOF') GO TO 1200
C
      RETURN
C
  120 CALL HELPMANU(IWD,8,IHELP,300,20,IHEPF)
      GO TO 2000
C   
  125 LISFLG=KMD
      IF(LISFLG.EQ.'LOF ') GO TO 2000
      WRITE(CMSSG,126)NAMH
  126 FORMAT('SCAN LOG FROM HIS-FILE - ',20A4)
      CALL MESSLOG(0,LOGUP)
      GO TO 2000
C   
C     ------------------------------------------------------------------
C     READ IN AND PROCESS BAN-FILES
C     ------------------------------------------------------------------
C
  130 IA=NXNB(IWDRAW,4,80)
      IF(IA.LE.0) GO TO 1500
      IB=LSNB(IWDRAW,IA,80)
      IF(IB.LE.0) GO TO 1500
      CALL LODUP(IWDRAW,IA,IB,BANNAM,1)
      NB=IB-IA+1
      CALL ISBYTE(0,BANNAM,NB)
C
      CALL FREG2(LBAN,CNAMBAN,IERR)
C
      IF(IERR.NE.0) GO TO 2050
      GO TO 2000
C
  135 CALL ZFREG2()
      GO TO 2000   
C
C     ------------------------------------------------------------------
C     Process gate-related commands (simple 1-d gates)
C     ------------------------------------------------------------------
C   
  150 CALL GATEMAN
      GO TO 2000
C
C     ------------------------------------------------------------------
C     RESET TAPE RECORD LENGTH
C     ------------------------------------------------------------------
C
  200 LAUTO='YES '
      CALL IVALU(LWD(1,2),IV,JERR)
      IF(JERR.NE.0) GO TO 1500
C
      IF(IV.EQ.0) THEN
                  ICNF='NO  '
                  JCNF='NO  '
                  WRITE(CMSSG,201)
                  CALL MESSLOG(LOGUT,LOGUP)
                  GO TO 2000
                  ENDIF
C
  201 FORMAT('SCANOR set to auto-detect data RECL')
C
      LTST=2*(IV/2)
      IF(LTST.NE.IV)    GO TO 1500
      IF(LTST.GT.32768) GO TO 1500
      IF(LTST.LT.4)     GO TO 1500
      LNBY=LTST
      LSTL=LTST/2
      WRITE(CMSSG,202)LNBY
  202 FORMAT('Data record length set to',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      LAUTO='NO  '
      GO TO 2000
C   
C     ------------------------------------------------------------------
C     ZERO THE BUFFER COUNTER 
C     ------------------------------------------------------------------
C   
  210 NBRED=0
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Display status information
C     ------------------------------------------------------------------
C
  220 CALL STATMAN
      GO TO 2000
C
C     ------------------------------------------------------------------
C     PROCESS "UCOM" - DELETE "UCOM" & LEADING BLANKS - CALL USERCMP
C     ------------------------------------------------------------------
C   
  700 CALL USEND(IWDRAW,IERR)
      IF(IERR.EQ.0) GO TO 2000
      GO TO 1500
C   
C     ------------------------------------------------------------------
C     PROCESS  -  L001 NSKIP,MAXIP   (SPECIFY L001 TAPE)
C              -  LOO2               (SPECIFY L002 TAPE)
C              -  SWAB               (REQUEST    BYTE-SWAP)
C              -  SWOF               (REQUEST NO BYTE-SWAP)
C     ------------------------------------------------------------------
C   
  950 IF(NF.LT.3)   GO TO 1500
      CALL LIMIV(LWD(1,2),0,500,NSKIP,IERR)
      IF(IERR.NE.0) GO TO 1500
      CALL LIMIV(LWD(1,3),1,500,MAXIP,IERR)
      IF(IERR.NE.0) GO TO 1500
      LFORM='L001'
      GO TO 2000
C   
  960 LFORM=KMD
      GO TO 2000
C   
  962 ISWAB='YES '
      GO TO 2000
  964 ISWAB='NO  '
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Process - CLID IDHI IDLO
C     ------------------------------------------------------------------
C
  980 CALL IVALU(LWD(1,2),IV,JERR)
      IF(JERR.NE.0)              GO TO 1500
      IF(IV.LE.0.OR.IV.GE.32768) GO TO 1500
      CALL IVALU(LWD(1,3),JV,JERR)
      IF(JERR.NE.0)              GO TO 1500
      IF(JV.LE.0.OR.JV.GE.32768) GO TO 1500
      IF(IV.EQ.JV)               GO TO 1500
      CLIDHI=IV
      CLIDLO=JV
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Process - CLIM MIN MAX
C     ------------------------------------------------------------------
C
 1000 CALL CLIMFIX(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER)
C
      CALL GREAD(IWD,LWD3,ITYP3,NF3,1,80,NTER3)
C
      CALL MILV3(LWD3(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1050
C
      CALL MILV3(LWD3(1,3),JV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1050
C
      IF(IV.GT.JV)  GO TO 1050
C
      VMETLO=IV
      VMETHI=JV
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
C
      GO TO 2000
C
 1050 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
C
      GO TO 1500
C
C     ------------------------------------------------------------------
C     Process - CLLO VMET
C     ------------------------------------------------------------------
C
 1100 CALL CLIMFIX(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER)
C
      CALL GREAD(IWD,LWD3,ITYP3,NF3,1,80,NTER3)
C
      CALL MILV3(LWD3(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1150
C
      CALL VMETFIND(IV)
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
C
      GO TO 2000
C
 1150 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
C
      GO TO 1500
C
C     ------------------------------------------------------------------
C     Process CLON & CLOF commands
C     ------------------------------------------------------------------
C
 1200 CLONOF=KMD
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Error Messages
C     ------------------------------------------------------------------
C
 1500 WRITE(CMSSG,1505)
      CALL MESSLOG(LOGUT,LOGUP)
 1505 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - CMD IGNORED')
      RETN=50
      GO TO 2000
C
C     ------------------------------------------------------------------
C     RETURNS
C     ------------------------------------------------------------------
C
 2000 IDONE='YES '
      RETURN
C
 2050 IDONE='YES '
      RETN=50
      RETURN
      END
