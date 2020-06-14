C$PROG UPVSETUP  - Initializes MOC offset pointers for user processing
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE UPVSETUP(JPAR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    DUM
      PARAMETER   (DUM=4)
C     ------------------------------------------------------------------
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2                                OUBUF(0:DUM)
C     ------------------------------------------------------------------
      COMMON/LM17/ NPARX,RESYN
      INTEGER*4    NPARX
      CHARACTER*4        RESYN
      DATA         RESYN/'YES '/
C     ------------------------------------------------------------------
      INTEGER*4    JPAR
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      NPARX=JPAR                           !# PARAMETERS
      OOFSET(1)=-1                         !SET NO OUBUFS
      OOFSET(2)=-1                         !SET NO OUBUFS
      OOFSET(3)=-1                         !SET NO OUBUFS
      RESYN='YES '                         !REQUEST RESYNC
      RETURN
      END
