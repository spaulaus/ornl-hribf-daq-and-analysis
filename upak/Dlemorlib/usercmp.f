C$PROG USERCMP   - Dummy user command processor (USERCMP)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE USERCMP(IWD)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4 IWD(20)
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      WRITE(6,10)
   10 FORMAT(1H ,'LEMOR AIN,T GOT NO USER COMMAND PROCESSOR')
      RETURN
      END
