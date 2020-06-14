C$PROG PVSETUP   - Setup routine for CHILUM (CHILUM2 & CHILUM3)
C
C     ******************************************************************
C     BY W.T. Milner at HRIBF  - LAST MODIFIED by WTM 08/28/99
C     ******************************************************************
C
      SUBROUTINE PVSETUP(JPAR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      INTEGER*4    DUM
C
      PARAMETER   (DUM=4)                    !DUMMY DIMENSION
C
      COMMON/SC10/ OPTR(3),OOFSET(3),OSIZE(3),OUTBUF
C
      INTEGER*4    OPTR,   OOFSET,   OSIZE
C
      INTEGER*2    OUTBUF(0:DUM)
C     ------------------------------------------------------------------
      COMMON/SC11/ FWLEN,NPAR,TOFSAVE
      INTEGER*4    FWLEN,NPAR,TOFSAVE
C     ------------------------------------------------------------------
      COMMON/SC21/ UBUFF(2048),TOFFSET
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
      NPAR=JPAR                                   !SAVE IT
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
