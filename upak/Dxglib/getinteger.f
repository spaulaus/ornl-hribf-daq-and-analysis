C$PROG GETINTEGER- Prompts for & gets an integer in "cursor mode"
C
C     ************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ************************************************************
C
      SUBROUTINE GETINTEGER(ACF,KEY,IDONE,IBN)
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
C
      CHARACTER*4 ACF,KEY,IDONE
C
      INTEGER*4   IBN,DUM,IERR
C
      SAVE
C
C     ************************************************************
C     PROMPTS FOR, SETS UP TO, AND "GETS" AN INTEGER WHILE IN THE
C     "HANDLE EVENT" MODE
C     ************************************************************
C
      IDONE='    '
C
      IF(KEY.EQ.'G   ') THEN
                        ACF='ON  '
                        WRITE(LOGUT,30)
                        CALL ZZ_FLUSH(LOGUT)
                        CALL NUMAC('INIT',KEY,DUM,IERR)
                        RETURN
                        ENDIF
C
      IF(ACF.EQ.'ON  '.AND.KEY.EQ.'CR  ') THEN
                        ACF='OFF '
                        CALL NUMAC('DONE',KEY,IBN,IERR)
                        IDONE='YES '
                        WRITE(LOGUT,60)
                        RETURN
                        ENDIF
C
      IF(ACF.EQ.'ON  ') THEN
                        WRITE(LOGUT,50)KEY
                        CALL ZZ_FLUSH(LOGUT)
                        CALL NUMAC('ACQ ',KEY,DUM,IERR)
                        IF(IERR.NE.0) ACF='OFF '
                        RETURN
                        ENDIF
C
   30 FORMAT(1H ,'ENTER ID >',$)
   50 FORMAT(A1,$)
   60 FORMAT(1H )
C
      RETURN
      END
