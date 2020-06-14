C$PROG LDFCHEK   - Checks LDF-file for proper termination (for SCANOR)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/12/2003
C     ******************************************************************
C
      SUBROUTINE LDFCHEK(IERR)
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
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
C
      INTEGER*4  IERR,FSTAT,STATF,FILSIZ,DIRHRR,ACTHRR,STATARA(13)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      STATF=FSTAT(LUINF,STATARA)
      IF(STATF.EQ.0) THEN
      FILSIZ=STATARA(8)
      ELSE
      FILSIZ=(-1)*STATF
      ENDIF
C
      DIRHRR=INDIR(2)
      ACTHRR=FILSIZ/32776
C
      IF(DIRHRR.EQ.ACTHRR) RETURN
C
      IERR=1
C
      INDIR(2)=ACTHRR
C
      CALL DINGER(3)
C
      WRITE(CMSSG,50)
   50 FORMAT(9('--------'))
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,100)
  100 FORMAT('Input LDF file is not properly terminated')
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,110)DIRHRR
  110 FORMAT('LDF directory says highest-record-written is - ',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,120)ACTHRR
  120 FORMAT('Whereas the ACTUAL highest-record-written is - ',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,130)
  130 FORMAT('To fix it: Use LEMOR and open unterminated file for ',
     &       'OUTPUT')
      CALL MESSLOG(LOGUT,LOGUP)
      CALL DINGER(3)
      WRITE(CMSSG,135)
  135 FORMAT('That is OUTPUT - with the command: OUF filename - OK?')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,140)
  140 FORMAT('Then follow directions')
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
      CALL DINGER(3)
C
      RETURN
C
      END

