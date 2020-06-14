C$PROG SECVLU    - Returns elapsed time (from first call) in seconds
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/23/2002
C
C     Using fseconds.c function by R.L. VARNER
C     ******************************************************************
C
      REAL*4 FUNCTION SECVLU(ARG)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      EXTERNAL   FSECONDS
C
      REAL*8     FSECONDS,TNIT,TNOW
C
      REAL*4     ARG
C
      INTEGER*4  NCALL
C
      DATA       NCALL/0/
C
      DATA       TNIT/0.0/
C
      SAVE
C
C     ------------------------------------------------------------------
C     SECVLU works like SECNDS but has millisecond resolusion rather
C     than 1 second resolution on LINUX machines.
C
C     Also SECVLU "wraps around" every 24 hours (from first call) rather
C     at midnight as SECNDS does.
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 100
C
      TNIT=FSECONDS()
C
      NCALL=1
C
  100 TNOW=FSECONDS()
C
      IF(TNOW-TNIT.GT.86400.0) THEN
      TNIT=TNIT+86400.0
      ENDIF
C
      SECVLU=TNOW-TNIT-ARG
C
      RETURN
C
      END
