C$PROG KOUTN     - Extracts output stream# from command line
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      INTEGER*4 FUNCTION KOUTN(KMD,NC,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    KMD,NC,IERR,KL
C
      INTEGER*4    X20,X30,X31,X33
      DATA         X20,X30,X31,X33/Z'20',Z'30',Z'31',Z'33'/
C
      SAVE
C
C
C     ------------------------------------------------------------------
C     FUNCTION TO PICK UP VALUE OF KMD CHARACTER# NC (OUT STREAM#)
C     ------------------------------------------------------------------
C
      IERR=0
C
      CALL ILBYTE(KL,KMD,NC-1)
      IF(KL.EQ.X20) KL=1
      IF(KL.GE.X31.AND.KL.LE.X33) KL=KL-X30
      IF(KL.LT.1.OR.KL.GT.3) GO TO 20
      KOUTN=KL
      RETURN
C
   20 KOUTN=0
      IERR=1
      END
