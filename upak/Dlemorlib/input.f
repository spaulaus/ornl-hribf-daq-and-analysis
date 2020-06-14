C$PROG INPUT     - Input routine for MOC-mode processing
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE INPUT(LUT,IGO,JFIR,JLAS,STAT)
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
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      COMMON/LM03/ LBUF(24576)
      INTEGER*4    LBUF
C     -----------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     -----------------------------------------------------------------
      COMMON/LM13/ NUMINT,NUMOUT(3),NOUT(3),MTIN(6),MTOUT(6,3),NOSTR
      INTEGER*4    NUMINT,NUMOUT,   NOUT,   MTIN,   MTOUT,     NOSTR
C     -----------------------------------------------------------------
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF(16384,2,3)
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2                                OUBUF
C     -----------------------------------------------------------------
      COMMON/LM15/ LPHED(64),NDX(3)
      INTEGER*4    LPHED,    NDX
C     -----------------------------------------------------------------
      COMMON/LM19/ JCNF,IREBF
      CHARACTER*4  JCNF,IREBF
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      COMMON/LM24/ LAUTO,INRECL
      CHARACTER*4  LAUTO
      INTEGER*4          INRECL
C     ------------------------------------------------------------------
      INTEGER*4    LUT,IGO,JFIR,JLAS
C
      INTEGER*2    LIST(16384,3)
C
      INTEGER*4    LISTF(8192,3)
C
      EQUIVALENCE (LIST(1,1),LBUF(1)),(LISTF(1,1),LBUF(1))
C
      INTEGER*4    LP,LR,I,J,IERR,LTM,NBYT,NNBYT
C
      INTEGER*4    KERR,JS,NFW,NDO,N,IA
C
      DATA         LR,LP/1,2/
C
      CHARACTER*4  STAT,MODE,KIND
C
      INTEGER*4    HHIR
      character*4  chhir
      equivalence  (chhir, hhir)
      DATA         cHHIR/'HHIR'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     LR = LIST BUFFER # (2ND INDEX OF LIST) BEING READ
C     LP = LIST BUFFER # (2ND INDEX OF LIST) BEING PROCESSED
C     ------------------------------------------------------------------
C
      STAT='GOOD'                               !RESET ERROR CODE
C
C     ==================================================================
C     Test-for/read input from LDF
C     ==================================================================
C
      IF(INFOP.NE.'YES ') GO TO 80
C
      LSTL=16384
      LNBY=32768
C
   50 INRECI=INRECI+1
C
      CALL LDFREAD(LUINF,INRECI,LIST(1,1),KIND,NFW,STAT)
C
      JFIR=1
      JLAS=2*NFW
      NBYT=4*NFW
      LP=1
C
      IF(STAT.NE.'GOOD') RETURN
C
      IF(KIND.NE.'DATA') GO TO 200
C
      IF(ISWAB.EQ.'YES ') CALL SWAPPER(LIST,NBYT)  !TST FOR BYTE-SWAP 
C
      RETURN
C
C     ==================================================================
C     Buffered read input from tape
C     ==================================================================
C
   80 IF(IGO.NE.1) GO TO 100                    !IGO=1 FOR 1ST CALL
C                                               !IGO=2 FOR SUB CALLS
C
      IF(LAUTO.NE.'YES ') THEN
      WRITE(CMSSG,90)LNBY
   90 FORMAT('Using user-specified input tape RECL =',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(LAUTO.EQ.'YES ') THEN                  !Tst for auto RECL
                          LNBY=32768            !detection
                          LSTL=16384            !enabled
                          ENDIF
C
  100 IF(MSGF.NE.'    ') RETURN                 !Test for Ctrl/C
C
      LR=1                                      !SET TO READ BUF-1
      LP=1                                      !SET TO PROC BUF-2
C
      CALL MT_READ(LUCI,LIST(1,LR),LNBY,NBYT,IERR)
C
      IF(IERR.NE.0) THEN
      CALL IOSERR(IERR)
      GO TO 500
      ENDIF
C
      IF(IGO.NE.1.OR.LAUTO.NE.'YES ') GO TO 150 !Tst for auto-recl
C
      IF(NBYT.EQ.256)   GO TO 150               !Skip for header
      IF(NBYT.EQ.128)   GO TO 150               !Skip for deadtime
      IF(NBYT.LT.2048)  GO TO 150               !Skip for PAC-records
      IF(NBYT.EQ.32000) GO TO 150               !Skip scaler records
      IF(MOD(NBYT,2).NE.0) GO TO 100            !Skip odd #bytes
C
      LNBY=NBYT                                 !RECL in bytes
      LSTL=NBYT/2                               !List length in half-wds
C
      WRITE(CMSSG,110)LNBY
  110 FORMAT('Using auto-detected input tape RECL =',I6)
      CALL MESSLOG(LOGUT,LOGUP)
C
  150 IF(NBYT.NE.LNBY) GO TO 200                !TST FOR DATA REC
C
C     ------------------------------------------------------------------
C     DO ANY BUFFER MODIFYING OPERATIONS
C     ------------------------------------------------------------------
C
      JFIR=16384*(LP-1)+1                       !FIRST LBUF INDEX
      JLAS=JFIR+LSTL-1                          !LAST  LBUF INDEX
C
      IF(ISWAB.EQ.'YES ') THEN                  !TST FOR BYTE-SWAP 
      CALL SWAPPER(LIST(1,LP),NBYT)             !REQUEST
                          ENDIF
C
      RETURN
C
C
  200 IF(ISWAB.EQ.'YES ') CALL SWAPPER(LISTF(1,LP),NBYT)
      IF(ISWAH.EQ.'YES ') CALL SWAPHED(LISTF(1,LP),NBYT)
C
C     ==================================================================
C     Copy non-data records directly to output Tape or LDf
C     ==================================================================
C
      IF(OUFOP.EQ.'YES ') GO TO 400
C
      IF(NBYT.NE.256)           GO TO 300    !TST FOR PRIMARY HEADER
      IF(LISTF(1,LP).NE.HHIR)   GO TO 300    !TST FOR PRIMARY HEADER
      LISTF(40,LP)=NPAR
      LISTF(41,LP)=2*OUSIZ(1)
C
C     ------------------------------------------------------------------
C     Save primary header for writing at beginning of continuation tape
C     ------------------------------------------------------------------
C
      DO 210 I=1,64
      LPHED(I)=LISTF(I,LP)
  210 CONTINUE
      WRITE(CMSSG,215)(LPHED(I),I=9,23),LPHED(33)
      CALL MESSLOG(LOGUT,LOGUP)
  215 FORMAT(15A4,' - #',I12)
      LPHED(36)=0
      LPHED(37)=0
      LPHED(38)=0
      LPHED(39)=0
C
C     ------------------------------------------------------------------
C     Copy non-data records directly to output - TAPE
C     ------------------------------------------------------------------
C
  300 IF(KIND.EQ.'PAC ') GO TO 320
C
      DO 310 I=1,NOSTR
      JS=I
      CALL MT_WRITE(LUCO(JS),LIST(1,LP),NBYT,NNBYT,IERR)
      IF(IERR.NE.0)CALL IOSERR(IERR)
      CALL IOERR(IERR,STAT)
      IF(STAT.NE.'GOOD')RETURN
  310 CONTINUE
C
      IF(INFOP.EQ.'YES ') GO TO 50
                          GO TO 100
C
  320 IA=1
      NDO=NBYT/1600
      DO 340 N=1,NDO
      CALL MT_WRITE(LUCO(1),LIST(IA,LP),1600,NNBYT,IERR)
      IF(IERR.NE.0)CALL IOSERR(IERR)
      CALL IOERR(IERR,STAT)
      IF(STAT.NE.'GOOD')RETURN
      IA=IA+800
  340 CONTINUE
C
      IF(INFOP.EQ.'YES ') GO TO 50
                          GO TO 100
C
C     ------------------------------------------------------------------
C     Copy non-data records directly to output - LDF
C     ------------------------------------------------------------------
C
  400 MODE='    '
C
      IF(INFOP.EQ.'YES ') THEN
      MODE=KIND
      GO TO 410
      ENDIF
C
      IF(NBYT.EQ.256)    MODE='HEAD'
      IF(NBYT.EQ.128)    MODE='DEAD'
      IF(NBYT.EQ.1600)   MODE='PAC '
      IF(NBYT.EQ.32000)  MODE='SCAL'
C
  410 IF(MODE.EQ.'    ')  THEN
      IF(INFOP.EQ.'YES ') GO TO 50
                          GO TO 100
                          ENDIF
C
      CALL LDFWRIT(LUOUF,MODE,NBYT,LIST(1,LP),IERR)
C
      IF(NBYT.EQ.256) THEN
      WRITE(CMSSG,215)(LISTF(I,LP),I=9,23),LISTF(33,LP)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(INFOP.EQ.'YES ') GO TO 50
                          GO TO 100
C
C     ==================================================================
C     Tape Error return - return simplified tape status in IERR
C     ==================================================================
C
  500 CALL IOERR(IERR,STAT)
      RETURN
      END
