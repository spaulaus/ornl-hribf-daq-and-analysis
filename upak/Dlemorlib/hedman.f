C$PROG HEDMAN    - Generates and writes headers on outout tape
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE HEDMAN(IERR)
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
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM13/ NUMINT,NUMOUT(3),NOUT(3),MTIN(6),MTOUT(6,3),NOSTR
      INTEGER*4    NUMINT,NUMOUT,   NOUT,   MTIN,   MTOUT,     NOSTR
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      INTEGER*4    IERR,ISTAT,IT,I,N
C
      INTEGER*4    HEDF(64),TITL(20),HEDN
C
      INTEGER*2    HEDH(128)
C
      CHARACTER*4  KMD,STAT
C
      EQUIVALENCE (HEDH,HEDF),(KMD,IWD(1))
C
      character*4  ctitl(20)
      equivalence  (ctitl, titl)
      DATA        cTITL,HEDN/20*'    ',1/
C
      character*4 chedf(64)
      equivalence (chedf,hedf)
      DATA        (cHEDF(I),I=1,12)/'HHIR','F   ','L002','    ',
     &                             'LIST',' DAT','A   ','    ',
     &                             '    ','    ','    ','    '/
C
      DATA        (cHEDF(I),I=33,64)/32*'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'HTIT') GO TO 100
      IF(KMD.EQ.'HNUM') GO TO 200
      IF(KMD.EQ.'HOUT') GO TO 300
      GO TO 500
C
  100 DO 110 I=1,19
      TITL(I)=IWDRAW(I+1)
  110 CONTINUE
      RETURN
C
  200 CALL IVALU(LWD(1,2),IT,IERR)
      IF(IERR.NE.0) GO TO 510
      HEDN=IT
      RETURN
C
  300 DO 310 I=1,20
      HEDF(I+12)=TITL(I)
  310 CONTINUE
      HEDF(33)=HEDN
      HEDF(41)=8192
C
      WRITE(CMSSG,315)(TITL(I),I=1,15),HEDN
  315 FORMAT(15A4,' - ',I12)
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(ISWAH.EQ.'YES ') CALL SWAPHED(HEDF,256)
C
      IF(OUTYP.EQ.'TAPE') THEN
      CALL WRITUM(LUCO(1),HEDF,256,STAT)
      IF(STAT.EQ.'GOOD') HEDN=HEDN+1
      RETURN
      ENDIF
C
      IF(OUTYP.EQ.'LDF ') THEN
      CALL LDFWRIT(LUOUF,'HEAD',256,HEDF,ISTAT)
      IF(ISTAT.EQ.0) HEDN=HEDN+1
      RETURN
      ENDIF
C
      GO TO 520
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Illegal command in call to routine HEDMAN')
      GO TO 600
C
  510 WRITE(CMSSG,515)
  515 FORMAT('Syntax error in header number specification')
      GO TO 600
C
  520 WRITE(CMSSG,525)
  525 FORMAT('Output tape or file not open - cmd ignored')
      GO TO 600
C
  600 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
