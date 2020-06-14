C$PROG SWAPPER   - Byte-swaps buffers according to type 
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/13/99
C     ******************************************************************
C
      SUBROUTINE SWAPPER(IBUF,NBYT)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      INTEGER*4    IBUF(*)
C
      INTEGER*4    NBYT,NHW
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(ISWAB.NE.'YES ') RETURN
C
      NHW=NBYT/2
C
      IF(LFORM.EQ.'L001') THEN
                          CALL SWAPB(IBUF,1,NHW)
                          RETURN
                          ENDIF
C
      IF(NBYT.EQ.1600)    RETURN
C
      IF(NBYT.EQ. 256)    THEN
                          CALL SWAPF(IBUF,33,64)
                          RETURN
                          ENDIF
C
      IF(NBYT.EQ.6780)    THEN
                          CALL SWAPF(IBUF,  8,  19)
                          CALL SWAPF(IBUF,740,1459)
                          RETURN
                          ENDIF
C
      CALL SWAPB(IBUF,1,NHW)
C
      RETURN
      END
