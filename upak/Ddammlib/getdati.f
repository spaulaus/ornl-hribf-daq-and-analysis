C$PROG GETDATI   - Returns ASCII date & time
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE GETDATI(MDYHMS)
C
      IMPLICIT NONE
C
      CHARACTER*20 MDYHMS
C
      INTEGER*4    JDATE(3),JTIME(2)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL MILDATE2(JDATE)
C
      CALL MILTIME(JTIME)
C
      WRITE(MDYHMS,10)JDATE,JTIME
C
   10 FORMAT(2A4,A1,' ',2A4)
      RETURN
      END
