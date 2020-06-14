C$PROG SCANORMSG  - Displays message that you are using a new scanor
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 02/12/2005
C     ******************************************************************
C
      SUBROUTINE SCANORMSG
C
      IMPLICIT NONE
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
      WRITE(6,70)
      WRITE(6,10)
C
   10 FORMAT(
     &'===============================================================',
     &'========')
C
   20 FORMAT('This is a new version of scanor with added features'/)
   30 FORMAT('See scanor doc - pages 6 & 28 for the new features')
   40 FORMAT('Also type: h vmet & h in & h new for new features'/)
   50 FORMAT('Report problems to: milner@mail.phy.orml.gov or ',
     &       'wtmilner@bellsouth.net'/)
   60 FORMAT('The old version of scanor is still available as:')
   70 FORMAT('/usr/hhirf/oldscanor.o & /usr/hhirf/oldscanorlib.a')
C
      RETURN
      END
