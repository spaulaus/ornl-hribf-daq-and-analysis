C$PROG USERCMP   - Dummy user command processor
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/29/98
C     ******************************************************************
C
      SUBROUTINE USERCMP(IWD)
C
      IMPLICIT NONE
C
      INTEGER*4 IWD(20)
C
      WRITE(6,10)
   10 FORMAT(1H ,'I AIN,T GOT NO USER COMMAND PROCESSOR')
      RETURN
      END
