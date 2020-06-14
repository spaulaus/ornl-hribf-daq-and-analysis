C$PROG USERMOC   - Demo user modify-copy routine (USERMOC)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE USERMOC(IBUF,NHW)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*2    IBUF(*)
C
      INTEGER*4    IDLST(2000),DALST(2000),EXPEV(2000)
C
      INTEGER*4    NHW,MXID,NPAR,IERR
C
      INTEGER*4    NWDS,IX,IY
C
      LOGICAL      BANTESTI
C   
      DATA         MXID/2000/
C
      CHARACTER*4  DONE
C
      SAVE
C
C     ------------------------------------------------------------------
C
C
  100 CALL UNPACKL(
     &           IBUF,   !I*2 - raw data buffer
     &           NHW,    !I*4 - # of I*2 words in IBUF
     &           MXID,   !I*4 - max-ID (dimension IDLST, DALST, EXPEV)
     &           IDLST,  !I*4 - ID-list   for returned event
     &           DALST,  !I*4 - Data-list for returned event
     &           EXPEV,  !I*4 - expanded-event array
     &           NPAR,   !I*4 - # of parameters in this event
     &           DONE,   !C*4 - YES/NO - requests new IBUF
     &           IERR)   !I*4 - 0 means OK, nonzero means error
C
C
      IF(DONE.EQ.'YES ') GO TO 200
C
      IF(IERR.NE.0) GO TO 100
C
      CALL EVLISOUT(IDLST,DALST,NPAR)
C
      GO TO 100
C
  200 RETURN
C
      END
