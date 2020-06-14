C$PROG LDFCHEKA  - Checks LDF-file for proper termination (for DATA ACQ)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/15/2003
C     ******************************************************************
C
      SUBROUTINE LDFCHEKA(MODE,IERR)
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
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      CHARACTER*4  MODE
C
      INTEGER*4    IERR,FSTAT,STATF,FILSIZ,DIRHRR,ACTHRR,STATARA(13)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(MODE.EQ.'OUT ') GO TO 100
C
      RETURN
C
C     ------------------------------------------------------------------
C     Do it for the output file
C     ------------------------------------------------------------------
C
  100 STATF=FSTAT(LUOUF,STATARA)
      IF(STATF.EQ.0) THEN
      FILSIZ=STATARA(8)
      ELSE
      FILSIZ=(-1)*STATF
      ENDIF
C
      DIRHRR=OUDIR(2)
      ACTHRR=FILSIZ/32776
C
      IF(DIRHRR.EQ.ACTHRR) RETURN
C
      WRITE(CMSSG,110)
  110 FORMAT(9('--------'))
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,205)
  205 FORMAT('Output LDF file is not properly terminated')
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,210)DIRHRR
  210 FORMAT('LDF directory says highest-record-written is - ',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,220)ACTHRR
  220 FORMAT('Whereas the ACTUAL highest-record-written is - ',I6)
      CALL MESSLOG(LOGUT,LOGUP)
C
      IERR=1
C
      RETURN
C
      END
