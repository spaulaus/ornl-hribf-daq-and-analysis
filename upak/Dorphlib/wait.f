C$PROG WAIT      - Generates a system wait in (milisec, sec, etc)
C
C     ******************************************************************
C     BY J.R. Beene AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
C
      SUBROUTINE WAIT( IMAG, IUNITS, IDS )
C
      INTEGER IMAG, IUNITS
C
      INTEGER*4 LMAG, LUNITS
C
      INTEGER*4 IDS
C
      SAVE
C
C     ------------------------------------------------------------------
C     Approximate delta time value of 1 millisecond
C     (10 thousand * 100 nanoseconds)
C     ------------------------------------------------------------------
C
C
      LMAG = IMAG
      LUNITS = IUNITS
C
C     ------------------------------------------------------------------
C     CALCULATE DELTA TIME FOR HIBERNATION
C     ------------------------------------------------------------------
C
  100 IF (LUNITS .EQ. 1) THEN				! Milliseconds
      IF (LMAG .LT. 1) GOTO 777
      CALL SET_RTIMER(LMAG)
      CALL CHECK_RTIMER
C
      ELSE IF (LUNITS .EQ. 2) THEN			! Seconds
      IF (LMAG .LT. 1) GOTO 777
      CALL SET_RTIMER_S(LMAG)
      CALL CHECK_RTIMER
C
      ELSE IF (LUNITS .EQ. 3) THEN			! Minutes
      IF (LMAG .LT. 1 .OR. LMAG .GT. 1440) GOTO 777
      CALL SET_RTIMER_M(LMAG)
      CALL CHECK_RTIMER
C
      ELSE IF (LUNITS .EQ. 4) THEN			! Hours
      IF (LMAG .LT. 1 .OR. LMAG .GT. 24) GOTO 777
      CALL SET_RTIMER_H(LMAG)
      CALL CHECK_RTIMER
C
      ELSE
      GOTO 777
      ENDIF
C
C     ------------------------------------------------------------------
C     SCHEDULE WAKEUP, THEN HIBERNATE
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
C
  777 CONTINUE
      RETURN
      END
