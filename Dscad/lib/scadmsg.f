C$PROG SCADMSG   - Displays message & provides for abort
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/31/2004
C     ******************************************************************
C
      SUBROUTINE SCADMSG
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
      CHARACTER*4   ITST
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL DINGER(4)
C
      WRITE(6,10)
      WRITE(6,20)
      WRITE(6,30)
      WRITE(6,40)
      WRITE(6,50)
      WRITE(6,60)
      WRITE(6,10)
C
   10 FORMAT(
     &'===============================================================')
C
   20 FORMAT('This is a new version of scad'/)
   30 FORMAT('It supports features of earlier scad, scadd & scudd'/)
   40 FORMAT('These are available via the names',
     &' oldscad, oldscadd & oldscudd'/)
   50 FORMAT('Type: [RETURN] to continue with this one or'/)
   60 FORMAT('Type: anything else to abort')
C
      READ(5,70)ITST
   70 FORMAT(A)
C
      IF(ITST.NE.' ') STOP
C
      RETURN
      END
