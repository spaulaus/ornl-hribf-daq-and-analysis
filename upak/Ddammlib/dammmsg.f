C$PROG DAMMMSG   - Displays message that you are using a new damm
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 09/16/2004
C     ******************************************************************
C
      SUBROUTINE DAMMMSG
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
c     CALL DINGER(4)
C
c     WRITE(6,10)
c     WRITE(6,20)
c     WRITE(6,30)
c     WRITE(6,40)
c     WRITE(6,50)
c     WRITE(6,10)
C
c  10 FORMAT(
c    &'===============================================================',
c    &'========')
C
c  20 FORMAT('This is a new version of damm with added features'/)
c  30 FORMAT('See damm doc - page 22 or type: h hisx to see new stuff'/)
c  40 FORMAT('Report problems to: milner@mail.phy.orml.gov or ',
c    &       'wtmilner@bellsouth.net'/)
c  50 FORMAT('The old version of damm is still available as olddamm')
C
      RETURN
      END
