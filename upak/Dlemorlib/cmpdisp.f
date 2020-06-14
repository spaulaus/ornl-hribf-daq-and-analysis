C$PROG CMPDISP   - Command processor for LEMOR (read & display)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CMPDISP(IDONE,IERR)
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
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      COMMON/LM05/ IBUF(16384)
      INTEGER*4    IBUF
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM09/ KINPUT,INRECN
      CHARACTER*4  KINPUT
      INTEGER*4           INRECN
C     ------------------------------------------------------------------
      COMMON/LM22/ NBRED,ICNF,INFLG
      INTEGER*4    NBRED
      CHARACTER*4        ICNF,INFLG
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      COMMON/LM32/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
C     ------------------------------------------------------------------
      COMMON/LM33/ UDFNAM(20),UDFRECL,UDFNPAR,UDFRECI
      INTEGER*4    UDFNAM,    UDFRECL,UDFNPAR,UDFRECI
C     ------------------------------------------------------------------
      INTEGER*4    KIND,NFW,NDO,N,IERR
C
      INTEGER*4    KOUTN
C
      INTEGER*4    LUC,KL,ILO,IHI,MAXI,TIM
C
      REAL*4       XV
C
      CHARACTER*4  IDONE,KMD,STAT
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      INTEGER*2    LIST(32768)
C
      EQUIVALENCE (LIST,IBUF)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C     
C     ------------------------------------------------------------------
C     Process commands
C     ------------------------------------------------------------------
C
C
      IF(KMD.EQ.'RDI ') GO TO 100
      IF(KMD.EQ.'RDO ') GO TO 310
C
      IF(KMD.EQ.'LISI') GO TO 510
      IF(KMD.EQ.'LISO') GO TO 510
      IF(KMD.EQ.'LISF') GO TO 515
C
      IF(KMD.EQ.'PEV ') GO TO 950
      IF(KMD.EQ.'PEVZ') GO TO 950
      IF(KMD.EQ.'PA  ') GO TO 950
      IF(KMD.EQ.'PZ  ') GO TO 950
      IF(KMD.EQ.'PI  ') GO TO 950
      IF(KMD.EQ.'PIF ') GO TO 950
C
      IF(KMD.EQ.'DEV ') GO TO 950
      IF(KMD.EQ.'DEVZ') GO TO 950
      IF(KMD.EQ.'DA  ') GO TO 950
      IF(KMD.EQ.'DZ  ') GO TO 950
      IF(KMD.EQ.'DI  ') GO TO 950
      IF(KMD.EQ.'DIF ') GO TO 950
C
      RETURN
C
C     ------------------------------------------------------------------
C     Process RDI request
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     Do it for LDF
C     ------------------------------------------------------------------
C
  100 IF(INTYP.EQ.'LDF ') THEN
      CALL MILV(LWD(1,2),NDO,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 6000
      IF(NDO.LE.0) NDO=1
      DO 120 N=1,NDO
      INRECI=INRECI+1
      CALL LDFREAD(LUINF,INRECI,IBUF,KIND,NFW,STAT)
      IF(IERR.NE.0) GO TO 6000
C
      NBRED=4*NFW
C
      CALL VMETGET(LIST,TIM)
C
      IF(TIM.NE.0) THEN
      CALL VMETFOR
      WRITE(CMSSG,105)INRECI,NBRED,STAT,VMETIMC
      ENDIF
C
      IF(TIM.EQ.0) THEN
      WRITE(CMSSG,110)INRECI,NBRED,STAT
      ENDIF
C
  105 FORMAT('Rec#, #bytes, stat =',2I8,4X,A4,'  VME-time=',A)
  110 FORMAT('Rec#, #bytes, stat =',2I8,4X,A4)
C
      CALL MESSLOG(LOGUT,LOGUP)
      IF(STAT.EQ.'EOF ') GO TO 5500
C
  120 CONTINUE
      GO TO 5500
      ENDIF
C
C     ------------------------------------------------------------------
C     Do it for UDF
C     ------------------------------------------------------------------
C
      IF(INTYP.EQ.'UDF ') THEN
      CALL MILV(LWD(1,2),NDO,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 6000
      IF(NDO.LE.0) NDO=1
      DO 140 N=1,NDO
      CALL UDFREAD(IBUF,KIND,NBRED,IERR)
      CALL IOERR(IERR,STAT)
      IF(IERR.NE.0) GO TO 6000
      WRITE(CMSSG,110)UDFRECI,NBRED,STAT
      CALL MESSLOG(LOGUT,LOGUP)
  140 CONTINUE
      GO TO 5500
      ENDIF
C
C     ------------------------------------------------------------------
C     Do it for TAPE or EVEL FILE
C     ------------------------------------------------------------------
C
  308 LUC=LUCI
      IF(LINO.NE.'YES '.AND.KINPUT.NE.'FILE') GO TO 4020
      ICNF='NO  '
      GO TO 320
C
C     ------------------------------------------------------------------
C     Process RDO request
C     ------------------------------------------------------------------
C
  310 IF(OUTYP.NE.'LDF ') GO TO 318
      CALL MILV(LWD(1,2),NDO,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 6000
      IF(NDO.LE.0) NDO=1
      DO 316 N=1,NDO
      OURECI=OURECI+1
      CALL LDFREAD(LUOUF,OURECI,IBUF,KIND,NFW,STAT)
      IF(IERR.NE.0) GO TO 6000
      NBRED=4*NFW
      WRITE(CMSSG,312)OURECI,NBRED,STAT
  312 FORMAT('Record, # bytes read =',2I8,'    STAT =',A4)
      CALL MESSLOG(LOGUT,LOGUP)
      IF(STAT.EQ.'EOF ') GO TO 5500
  316 CONTINUE
      GO TO 5500
C
  318 KL=KOUTN(KMD,4,IERR)
      IF(IERR.NE.0)          GO TO 3500
      IF(LOUO(KL).NE.'YES ') GO TO 4020
      LUC=LUCO(KL)
C
C     ------------------------------------------------------------------
C     READ IN ONE OR MORE RECORDS FROM INPUT OR OUTPUT TAPE
C     ------------------------------------------------------------------
C
  320 NDO=1
      IF(NF.GT.2) GO TO 4000
      IF(NF.NE.2) GO TO 330
      CALL IVALU(LWD(1,2),NDO,IERR)
      IF(IERR.NE.0) GO TO 4000
  330 DO 340 N=1,NDO
      IF(MSGF.NE.'    ') GO TO 6000
C
      IF(KMD.EQ.'RDI ') CALL READUM1(LUC,IBUF,65536,NBRED,STAT)
      IF(KMD.NE.'RDI ') CALL READUM( LUC,IBUF,65536,NBRED,STAT)
C
      WRITE(CMSSG,335)N,NBRED,STAT
      CALL MESSLOG(LOGUT,0)
  335 FORMAT('RECORD, # BYTES READ =',2I8,'    STAT =',A4)
      IF(STAT.NE.'GOOD') GO TO 6000
  340 CONTINUE
C
      IF(ISWAB.EQ.'YES ') CALL SWAPPER(IBUF,NBRED)
      IF(ISWAH.EQ.'YES ') CALL SWAPHED(IBUF,NBRED)
C
      GO TO 5500
C
C     ------------------------------------------------------------------
C     LOG/DISPLAY AUXILLARY RECORDS FROM HRIBF TAPES or LDF files
C     ------------------------------------------------------------------
C
  510 ICNF='NO  '
  515 CALL RECLOG(KMD,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
C
C     ------------------------------------------------------------------
C     SET UP TO PRINT OR DISPLAY DATA
C     ------------------------------------------------------------------
C
  950 IF(NF.NE.3) GO TO 4000
      CALL IVALU(LWD(1,2),ILO,IERR)
      IF(IERR.NE.0) GO TO 4000
      CALL IVALU(LWD(1,3),IHI,IERR)
      IF(IERR.NE.0) GO TO 4000
      IF(ILO.LT.1)   GO TO 4010
      MAXI=NBRED/2
      IF(KMD.EQ.'PIF '.OR.KMD.EQ.'DIF ') MAXI=NBRED/4
      IF(IHI.GT.MAXI) GO TO 4010
      IF(ILO.GT.IHI)  GO TO 4010
C
      CALL DDAT(KMD,ILO,IHI)
      GO TO 5500
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 3500 WRITE(CMSSG,3505)
      CALL MESSLOG(LOGUT,LOGUP)
 3505 FORMAT('Illegal command - Ignored')
      GO TO 6000
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
 4020 WRITE(CMSSG,4025)
 4025 FORMAT('REQUESTED TAPE OR FILE NOT ASSIGNED - CMD IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
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
