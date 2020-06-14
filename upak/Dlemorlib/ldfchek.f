C$PROG LDFCHEK   - Checks LDF-file for proper termination (for LEMOR)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/12/2003
C     ******************************************************************
C
      SUBROUTINE LDFCHEK(MODE,IERR)
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
      CHARACTER*4  MODE,RESPONSE
C
      INTEGER*4    IERR,FSTAT,STATF,FILSIZ,DIRHRR,ACTHRR,STATARA(13)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(MODE.EQ.'INP ') GO TO 100
      IF(MODE.EQ.'OUT ') GO TO 200
C
      RETURN
C
C     ------------------------------------------------------------------
C     Do it for the input file
C     ------------------------------------------------------------------
C
  100 STATF=FSTAT(LUINF,STATARA)
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
      CALL DINGER(3)
C
      WRITE(CMSSG,110)
  110 FORMAT(9('--------'))
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,120)
  120 FORMAT('Input LDF file is not properly terminated')
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,130)DIRHRR
  130 FORMAT('LDF directory says highest-record-written is - ',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,140)ACTHRR
  140 FORMAT('Whereas the ACTUAL highest-record-written is - ',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,150)
  150 FORMAT('To fix it: Use LEMOR and open unterminated file for ',
     &       'output')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,160)
  160 FORMAT('Then follow directions')
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,110)
      CALL MESSLOG(LOGUT,LOGUP)
C
      CALL DINGER(3)
C
      INDIR(2)=ACTHRR
C
      IERR=1

      RETURN
C
C     ------------------------------------------------------------------
C     Do it for the output file
C     ------------------------------------------------------------------
C
  200 STATF=FSTAT(LUOUF,STATARA)
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
      CALL DINGER(3)
C
      WRITE(CMSSG,110)
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
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,230)
  230 FORMAT('If you DO     want me to fix it, type: yes')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,240)
  240 FORMAT('If you DO NOT wamt me to fix it, type: anything else')
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(6,250)
  250 FORMAT(1H ,'So what do you say?->',$)
      READ(5,260)RESPONSE
  260 FORMAT(A)
C
      IF(RESPONSE.EQ.'YES '.OR.RESPONSE.EQ.'yes ') THEN
      IF(ACTHRR.GT.65518) ACTHRR=65518
      OUDIR(2)=ACTHRR
      OURECI  =ACTHRR
      CALL LDFILMAR(2,1,IERR)
      WRITE(CMSSG,270)
  270 FORMAT('Fix request executed')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,110)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL DINGER(3)
      RETURN
      ENDIF
C
      WRITE(CMSSG,280)
  280 FORMAT('File not fixed - output not allowed')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,110)
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
C
      RETURN
C
      END
