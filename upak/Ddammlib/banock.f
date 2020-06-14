C$PROG BANOCK    - Checks banana OPEN, CLOSE, CHEK, for status
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BANOCK(MODE,IDW,ISTAT)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      CHARACTER*4  MODE,ISTAT
C
      DATA IDOPEN/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      ISTAT='GOOD'
C
      IF(MODE.EQ.'CLOS') THEN
                         IDOPEN=0
                         RETURN
                         ENDIF
C
      IF(MODE.EQ.'OPEN') THEN
                         IF(IDOPEN.NE.0) GO TO 100
                         IDOPEN=IDW
                         RETURN
                         ENDIF
C
      IF(MODE.EQ.'TEST') THEN
                         IF(IDW.NE.IDOPEN) ISTAT='BAD '
                         RETURN
                         ENDIF
C
      IF(MODE.EQ.'CHEK') THEN
                         IF(IDW.EQ.IDOPEN) RETURN
                         IF(IDOPEN.NE.0)   GO TO 100
                         IDOPEN=IDW
                         RETURN
                         ENDIF
C
  100 WRITE(CMSSG,105)IDOPEN
  105 FORMAT('BANANA ALREADY OPEN IN WINDOW-',I2,' - COMMAND IGNORED')
      CALL MESSLOG(LOGUT,0)
      ISTAT='BAD '
      RETURN
      END
