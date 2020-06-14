C$PROG HIS_CLOSE - Closes his-file
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE HIS_CLOSE(LUH)
C
      SAVE
C
      CALL SYS_CLOSE(LUH)
C
      RETURN
      END
