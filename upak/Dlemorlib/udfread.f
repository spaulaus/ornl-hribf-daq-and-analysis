C$PROG UDFREAD   - Reads UDF-file and loads IBUF - for LEMOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 11/22/2004
C     ******************************************************************
C
      SUBROUTINE UDFREAD(IBUF,KIND,NBYT,IERR)
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
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      COMMON/LM33/ UDFNAM(20),UDFRECL,UDFNPAR,UDFRECI
      INTEGER*4    UDFNAM,    UDFRECL,UDFNPAR,UDFRECI
C     ------------------------------------------------------------------
      COMMON/LM34/ NCEOF,LAUTO
      INTEGER*4    NCEOF
      CHARACTER*4        LAUTO
C     ------------------------------------------------------------------
      INTEGER*2    IBUF(*)
C
      INTEGER*4    PID(8192),DAT(8192),NPAR
C
      INTEGER*4    NREC,NBYT,IERR,NERR,NB,I
C
      EQUIVALENCE (NPAR,UDFNPAR)
C
      INTEGER*4    MAX,MXID
      DATA         MAX,MXID/16384,8192/
C
      DATA         NERR/0/
C
      CHARACTER*4  KIND
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C     DEFINITIONS:
C
C     NREC   ;Not used for sequential access but included in case it's
C            ;needed for some "direct access" applications
C
C     IBUF   ;INTEGER*2 data buffer to be loaded. Max length is 16384
C
C     KIND   ;CHARACTER*4 record type - should be set to 'DATA' as shown
C            ;in this example
C
C     NBYT   ;Number of bytes stored in IBUF. Should be a multiole of 2
C            ;and be less than or equal to 32768.
C
C     IERR   ;Error flag. 0 says good, 999 says EOF, 5 for all other.
C     ------------------------------------------------------------------
C     NOTE:
C
C     IBUF must be loaded with INTEGER*2 data whose range is 0 to 32767
C          for data and 8000hex plus parameter ID number for parm-IDs.
C
C     IBUF structure is L003.
C     ------------------------------------------------------------------
C
      IERR=0
C
      NB=0
C
      IF(NPAR.EQ.0) GO TO 100
C
      DO 50 I=1,NPAR
      NB=NB+1
      IBUF(NB)=PID(I)+32768
      NB=NB+1
      IBUF(NB)=DAT(I)
   50 CONTINUE
      NB=NB+1
      IBUF(NB)=-1
      NB=NB+1
      IBUF(NB)=-1
C
  100 CALL UDFEVENT(MXID,PID,DAT,NPAR,IERR)
C
      IF(IERR.NE.0) GO TO 120
C
      IF(NB+2*NPAR+2.GT.MAX) GO TO 120
C
      DO 110 I=1,NPAR
      NB=NB+1
      IBUF(NB)=PID(I)+32768
      NB=NB+1
      IBUF(NB)=DAT(I)
  110 CONTINUE
      NB=NB+1
      IBUF(NB)=-1
      NB=NB+1
      IBUF(NB)=-1
C
CX    IF(IERR.NE.0) GO TO 120
C
      IF(NB.EQ.MAX) THEN
      NPAR=0
      KIND='DATA'
      NBYT=32768
      RETURN
      ENDIF
C
      IF(NB.LT.MAX) GO TO 100
C
  120 DO 130 I=NB+1,MAX
      IBUF(I)=-1
  130 CONTINUE
      KIND='DATA'
      NBYT=32768
      IF(IERR.NE.0) GO TO 200
      RETURN
C
C     ------------------------------------------------------------------
C     Error RETURNs
C     ------------------------------------------------------------------
C
  200 NERR=NERR+1
      NPAR=0
      IF(NERR.GT.1) THEN
      IERR=5
      NBYT=0
      NERR=0
      RETURN
      ENDIF
C
      IERR=0
      NPAR=0

      RETURN
      END
