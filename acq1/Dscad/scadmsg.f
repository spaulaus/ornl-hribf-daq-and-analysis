C$PROG SCADMSG   - Displays message that you are using a new SCAD
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 02/09/2006
C     ******************************************************************
C
      SUBROUTINE SCADMSG
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      RETURN         ! RLV 8-Sept-2009
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
   20 FORMAT('This is a new version of scad with added features'/)
   30 FORMAT('See scad.doc - pages 5 & 6 for the new features')
   40 FORMAT('Also type: h new for new features'/)
   50 FORMAT('Report problems to: milner@mail.phy.orml.gov or ',
     &       'wtmilner@bellsouth.net'/)
   60 FORMAT('The old version of scad is still available on:')
   70 FORMAT('/usr/hhirf/ as oldscad')
C
      RETURN
      END
