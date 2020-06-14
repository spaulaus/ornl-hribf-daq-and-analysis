C$PROG YESNO     - Prompts for a YES/NO response from user
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 03/07/2003
C     ******************************************************************
C
      SUBROUTINE YESNO(ANS)
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
      CHARACTER*4 TANS,ANS
C
      INTEGER*4   NTRY,BELL
C
      DATA                BELL/Z'07070707'/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      ANS='NO  '
      NTRY=0
C
  100 WRITE(6,105)BELL
  105 FORMAT(1H ,A4,'do you agree? - Y/N ->',$)
      READ(5,110)TANS
  110 FORMAT(A4)
      CALL CASEUP1(TANS)
C
      IF(TANS.EQ.'YES ') ANS='YES '
      IF(TANS.EQ.'NO  ') ANS='NO  '
      IF(TANS.EQ.'Y   ') ANS='YES '
      IF(TANS.EQ.'N   ') ANS='NO  '
C
      IF(ANS.EQ.'YES ') RETURN
C
      IF(ANS.EQ.'NO  ') RETURN
C
      NTRY=NTRY+1
      IF(NTRY.GE.3) GO TO 150
C
      WRITE(LOGUT,120)BELL
  120 FORMAT(1H ,A4,'Please answer: Y or N')
      GO TO 100
C
  150 WRITE(LOGUT,155)BELL
  155 FORMAT(1H ,A4,'I give up - I will take that as a N!')
      ANS='NO  '
      RETURN
      END
