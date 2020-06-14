C$PROG CMPREAD   - Reads and displays records for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/04/99
C     ******************************************************************
C
      SUBROUTINE CMPREAD(IDONE,RETN)
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
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC06/ LIST(16384,2)
      INTEGER*2    LIST
C     ------------------------------------------------------------------
      COMMON/SC14/ NBRED,NBTOP,ICNF
      INTEGER*4    NBRED,NBTOP
      CHARACTER*4              ICNF
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      COMMON/SC28/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC,CLONOF
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
      CHARACTER*4                                             CLONOF
C     ------------------------------------------------------------------
      INTEGER*4    RETN
C
      INTEGER*4    NDO,IERR,NFW,NBYRED,N,I
C
      INTEGER*4    ILO,IHI,MAXI,TIM
C
      INTEGER*4    IBUF(16384)
      EQUIVALENCE (IBUF,LIST)
C
      REAL*4       XV
C
      CHARACTER*4  KMD,IDONE,STAT,KIND

      INTEGER*4    LUT
C
      EQUIVALENCE (KMD,LWD(1,1)),(LUT,LUC(1))
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IDONE='NO  '
      RETN =0
C
      IF(KMD.EQ.'RDI ') GO TO 100
C
      IF(KMD.EQ.'PEV ') GO TO 200
      IF(KMD.EQ.'PEVZ') GO TO 200
      IF(KMD.EQ.'PA  ') GO TO 200
      IF(KMD.EQ.'PZ  ') GO TO 200
      IF(KMD.EQ.'PI  ') GO TO 200
      IF(KMD.EQ.'PIF ') GO TO 200
C
      IF(KMD.EQ.'DEV ') GO TO 200
      IF(KMD.EQ.'DEVZ') GO TO 200
      IF(KMD.EQ.'DA  ') GO TO 200
      IF(KMD.EQ.'DZ  ') GO TO 200
      IF(KMD.EQ.'DI  ') GO TO 200
      IF(KMD.EQ.'DIF ') GO TO 200
C
      RETURN
C
C     ------------------------------------------------------------------
C     Process RDI request
C     ------------------------------------------------------------------
C
  100 CALL MILV(LWD(1,2),NDO,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      IF(NDO.LE.0) NDO=1
C
      IF(INTYP.EQ.'LDF ') GO TO 110
      IF(INTYP.EQ.'TAPE') GO TO 130
      IF(INTYP.EQ.'SHM ') GO TO 150
      IF(INTYP.EQ.'UDF ') GO TO 170
      GO TO 520
C
C     ------------------------------------------------------------------
C     READ IN ONE OR MORE RECORDS FROM LDF FILE
C     ------------------------------------------------------------------
C
  110 DO 120 N=1,NDO
      IF(MSGF.NE.'    ') GO TO 1000
      CALL LDFREAD(LUINF,INRECI,IBUF,KIND,NBYRED,IERR)
      CALL IOERR(IERR,STAT)
C
      CALL VMETGET(LIST,TIM)
C
      IF(TIM.NE.0) THEN
      CALL VMETFOR
      WRITE(CMSSG,112)INRECI,NBYRED,STAT,VMETIMC
      ENDIF
C
      IF(TIM.EQ.0) THEN
      WRITE(CMSSG,115)INRECI,NBYRED,STAT
      ENDIF
C
  112 FORMAT('Rec#, #bytes, stat =',2I8,4X,A4,'  VME-time=',A)
  115 FORMAT('Rec#, #bytes, stat =',2I8,4X,A4)
C
      CALL MESSLOG(LOGUT,LOGUP)
      INRECI=INRECI+1
      IF(STAT.EQ.'EOF ') GO TO 1000
      IF(STAT.NE.'GOOD') GO TO 1000
  120 CONTINUE
      GO TO 1000
C
C     ------------------------------------------------------------------
C     READ IN ONE OR MORE RECORDS FROM INPUT TAPE
C     ------------------------------------------------------------------
C
  130 ICNF='NO  '
C
      DO 140 N=1,NDO
      IF(MSGF.NE.'    ') GO TO 1000
C
      CALL TAPREAD(LUT,IBUF,65536,NBYRED,STAT)
C
      WRITE(CMSSG,135)N,NBYRED,STAT
      CALL MESSLOG(LOGUT,0)
  135 FORMAT('RECORD, # BYTES READ =',2I8,'    STAT =',A4)
      IF(STAT.NE.'GOOD') GO TO 1000
  140 CONTINUE
C
      IF(ISWAB.EQ.'YES ') CALL SWAPPER(IBUF,NBYRED)
C
      GO TO 1000
C
C
C     ------------------------------------------------------------------
C     READ IN ONE OR MORE RECORDS FROM INPUT SHM
C     ------------------------------------------------------------------
C
  150 ICNF='NO  '
 
      DO 160 N=1,NDO
      IF(MSGF.NE.'    ') GO TO 1000
 
      CALL READIPC(IBUF,32768,NBYRED,IERR,MSGF)
 
      STAT='GOOD'
 
      IF(NBYRED.EQ.0.OR.IERR.NE.0) STAT='ABNO'
 
      WRITE(CMSSG,155)N,NBYRED,STAT
      CALL MESSLOG(LOGUT,0)
  155 FORMAT('RECORD, # BYTES READ =',2I8,'    STAT =',A4)
      IF(STAT.NE.'GOOD') GO TO 1000
  160 CONTINUE
 
      IF(ISWAB.EQ.'YES ') CALL SWAPPER(IBUF,NBYRED)
 
      GO TO 1000
C
C
C     ------------------------------------------------------------------
C     READ IN ONE OR MORE RECORDS FROM UDF FILE
C     ------------------------------------------------------------------
C
  170 DO 180 N=1,NDO
      IF(MSGF.NE.'    ') GO TO 1000
      CALL UDFREAD(IBUF,KIND,NBYRED,IERR)
      CALL IOERR(IERR,STAT)
      WRITE(CMSSG,175)INRECI-1,NBYRED,STAT
  175 FORMAT('Record, # bytes read =',2I8,'    STAT =',A4)
      CALL MESSLOG(LOGUT,LOGUP)
      IF(STAT.EQ.'EOF ') GO TO 1000
      IF(STAT.NE.'GOOD') GO TO 1000
  180 CONTINUE
      GO TO 1000
C
C     ------------------------------------------------------------------
C     SET UP TO PRINT OR DISPLAY DATA
C     ------------------------------------------------------------------
C
  200 IF(NF.NE.3) GO TO 500
      CALL IVALU(LWD(1,2),ILO,IERR)
      IF(IERR.NE.0) GO TO 500
      CALL IVALU(LWD(1,3),IHI,IERR)
      IF(IERR.NE.0) GO TO 500
      IF(ILO.LT.1)   GO TO 510
      MAXI=NBYRED/2
      IF(KMD.EQ.'PIF '.OR.KMD.EQ.'DIF ') MAXI=NBYRED/4
      IF(IHI.GT.MAXI) GO TO 510
      IF(ILO.GT.IHI)  GO TO 510
C
      CALL DDAT(KMD,ILO,IHI)
      GO TO 1000
C
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error - CMD ignored')
      GO TO 990
C
  510 WRITE(CMSSG,515)
  515 FORMAT('Illegal value or syntax error - CMD ignored')
      GO TO 990
C
  520 WRITE(CMSSG,525)
  525 FORMAT('Requested input tape, ldf-file or SHM nor assigned',
     &       ' - CMD ignored')
      GO TO 990
C
  990 CALL MESSLOG(LOGUT,LOGUP)
C
 1000 IDONE='YES '
      RETN=0
      RETURN
      END
