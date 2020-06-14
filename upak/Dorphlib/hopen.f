C$PROG HOPEN     - Calls HISMAN to open his-files
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE HOPEN(NAMF,LUH,LUD,IERR)
C
      INTEGER*4 NAMF(20)
C
      SAVE
C
      CALL HISMAN('OPEN',NAMF,LUD,LUH,'RO  ',IERR)
C
      RETURN
      END
