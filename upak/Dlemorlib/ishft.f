C$PROG ISHFT     - Shift function accomodates RIGHT & LEFT shifts
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      INTEGER*4 FUNCTION ISHFT(IX,NBITS)
C
      INTEGER*4 IX,NBITS,MBITS
C
      SAVE
C
C
      MBITS=NBITS
C
      IF(NBITS.EQ.0) THEN
      ISHFT=IX
      RETURN
      ENDIF
C
      IF(NBITS.GT.0) THEN
      ISHFT=ISHIFT(IX,NBITS)
      RETURN
      ENDIF
C
*     MBITS=IABS(NBITS)
      ISHFT=ISHIFT(IX,NBITS)
      RETURN
C
      END
