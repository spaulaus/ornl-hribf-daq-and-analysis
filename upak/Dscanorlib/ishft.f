C$PROG ISHFT     - Shift function accomodates RIGHT & LEFT shifts
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 11/10/99
C     ******************************************************************
C
      INTEGER*4 FUNCTION ISHFT(IX,NBITS)
C
      INTEGER*4 IX,NBITS,MBITS
C
      MBITS=NBITS
C
      IF(NBITS.EQ.0) THEN
      ISHFT=IX
      RETURN
      ENDIF
C
      IF(NBITS.GT.0) THEN
      ISHFT=LSHIFT(IX,NBITS)
      RETURN
      ENDIF
C
      MBITS=IABS(NBITS)
      ISHFT=RSHIFT(IX,MBITS)
      RETURN
C
      END
