C$PROG INPUTIPC  - Input routine reads ORPHAS IPC shared memory
C
      SUBROUTINE INPUTIPC(IERR)
C   
C     ******************************************************************
C     By R.L.Varner - last modified by W.T. Milner 07/16/99
C     ******************************************************************
C     For new shared memory buffer scheme.  MCSQ  11/26/93
C     ******************************************************************
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
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM                      
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC06/ LIST(16384,2)
      INTEGER*2    LIST
C     ------------------------------------------------------------------
      COMMON/ORPHAS/ STRBUFEVT,NUMBUFEVTS,BUF_NUM,LASTEVT,SUMEVTS,
     &               BEGINEVT
C
      REAL*8         STRBUFEVT,NUMBUFEVTS,BUF_NUM,LASTEVT,SUMEVTS,
     &               BEGINEVT
C     ------------------------------------------------------------------
      INTEGER*4      IERR,NBYT,I
C
      INTEGER*4      LISTF(16384)
C
      EQUIVALENCE   (LISTF,LIST)
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0                                    !RESET ERR FLG
C
C     ------------------------------------------------------------------
C     Read a buffer from SHM stream
C     ------------------------------------------------------------------
C
   20 CALL READIPC(LIST(1,1),32768,NBYT,IERR,MSGF)
C
      IF(NBYT.EQ.0.OR.IERR.NE.0) THEN
      IERR=999
      RETURN
      ENDIF
C   
      IF(NBYT.EQ.LNBY) GO TO 30                 !TST FOR DATA
      IF(NBYT.EQ.256)  GO TO 40                 !TST FOR HEAD
      IF(NBYT.EQ.128)  GO TO 60                 !TST FOR DEADTIME INFO
                       GO TO 20                 !ELSE, SKIP IT
C
   30 IF(IERR.EQ.0) THEN
      LASTEVT=STRBUFEVT+NUMBUFEVTS
      SUMEVTS=SUMEVTS+  NUMBUFEVTS
      ENDIF
      RETURN
C
   40 WRITE(CMSSG,45)(LISTF(I),I=9,23),LISTF(33)
   45 FORMAT(15A4,I12)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 20
C
   60 DO 70 I=1,28
      MSSG(I)=LISTF(I)
   70 CONTINUE
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 20
C
      END
