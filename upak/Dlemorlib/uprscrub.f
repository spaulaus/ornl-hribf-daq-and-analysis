C$PROG UPRSCRUB  - Requests MOC resync - scrub request for user process
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE UPRSCRUB
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM17/ NPARX,RESYN
      INTEGER*4    NPARX
      CHARACTER*4        RESYN
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      RESYN='YES '                         !REQUEST RESYNC
      RETURN
      END
