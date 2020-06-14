C$PROG HWPCVAL   - Returns # half-wds/channel for specifed record#
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 08/20/2005
C     ******************************************************************
C
      INTEGER*4 FUNCTION HWPCVAL(RECN)
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
      COMMON/HIS3/ RECLIS(100),HPCLIS(100),NLIS
      INTEGER*4    RECLIS,     HPCLIS,     NLIS
C     ------------------------------------------------------------------
C
      INTEGER*4    RECN,HWPC,N
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      HWPC=0
C
      DO 100 N=1,NLIS-1
C
      IF(RECN.GE.RECLIS(N).AND.RECN.LT.RECLIS(N+1)) THEN
      HWPC=HPCLIS(N)
      GO TO 110
      ENDIF
  100 CONTINUE
C
  110 HWPCVAL=HWPC
C
      RETURN
      END
