C$PROG PVSETUP   - Setup routine for CHILUM (CHILUM2 & CHILUM3)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE PVSETUP(JPAR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF(16384,2,3)
C
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2    OUBUF
C     ------------------------------------------------------------------
      COMMON/LM10/ FWLEN,NPARM,TOFSAVE
      INTEGER*4    FWLEN,NPARM,TOFSAVE
C     ------------------------------------------------------------------
      COMMON/LM28/ UBUFF(2048),TOFFSET
      INTEGER*4    UBUFF,      TOFFSET
C     ------------------------------------------------------------------
      INTEGER*4    TBUFF(0:1999)
C
      EQUIVALENCE (TBUFF(0),UBUFF(25)) 
C
      INTEGER*4    JPAR,I
C
      SAVE
C
C     ------------------------------------------------------------------
C
      FWLEN=JPAR-1                                !LAST TBUF WORD TO ZOT
C
      NPARM=JPAR                                  !SAVE IT
      OOFSET(1)=-1
      OOFSET(2)=-1
      OOFSET(3)=-1                                !SET NO OUTBUFS
      TOFFSET=-1                                  !WHEN STARTING
      DO 10 I=FWLEN,0,-1
      TBUFF(I)=-1                                 !"ZOT"
   10 CONTINUE
      TOFSAVE=TOFFSET                             !SAVE THE POINTER
      RETURN
C
C     ==================================================================
C     SCRUB entry
C     ==================================================================
C
C$ENTR PRSCRUB   - Scrub routine for CHULUM2 & CHILUM3
C
      ENTRY PRSCRUB
C
      TOFSAVE=-2                           !FORCE RESYNCH
      DO 20 I=FWLEN,0,-1
      TBUFF(I)=-1                          !"ZOT"
   20 CONTINUE
      RETURN
      END

