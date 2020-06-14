C$PROG CHILUM    - CHIL manager routine for L001, L002 & L003 processing
C   
C     ******************************************************************
C     By WT Milner at HRIBF - last modified 08/27/99
C     ******************************************************************
C
      SUBROUTINE CHILUM(INBUF,NWDS)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB
      CHARACTER*4                                   LFORM
C     ------------------------------------------------------------------
      INTEGER*2    INBUF(*)
C
      INTEGER*4    NWDS
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(LFORM.EQ.'L003') THEN
      CALL CHILUM3(INBUF,NWDS)
      RETURN
      ENDIF
C
      IF(LFORM.EQ.'L002') THEN
      CALL CHILUM2(INBUF,NWDS)
      RETURN
      ENDIF
C
      IF(LFORM.EQ.'L001') THEN
      CALL CHILUM3(INBUF,NWDS)
      RETURN
      ENDIF
C
      WRITE(CMSSG,10)LFORM
   10 FORMAT('Illegal data format identifier for CHIL processing = ',A4)
      CALL MESSLOG(LOGUT,LOGUP)
      STOP
C
      END
