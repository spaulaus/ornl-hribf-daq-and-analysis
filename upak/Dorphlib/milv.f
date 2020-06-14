C$PROG MILV      - ASCII to numeric decoder for 8 character fields
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/27/2002
C     ******************************************************************
C
      SUBROUTINE MILV(JWD,IV,XV,KIND,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      INTEGER*4     IWD(2),JWD(2)
C
      CHARACTER*8   CIWD
      EQUIVALENCE  (CIWD,IWD)
C
      INTEGER*4     X2E,X44,X45,X65
      DATA          X2E,X44,X45,X65/Z'2E',Z'44',Z'45',Z'65'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO TEST FOR AND RETURN LEGAL FIXED AND FLOATING VALUES
C     FIRST CHARACTER MUST BE +, -, ., OR DECIMAL DIGIT
C     DOES NOT SET IV OR XV IF ERROR OCCURRS
C     ------------------------------------------------------------------
C
      IERR=0
      KIND=0
      NDEC=0
C
      DO 10 I=1,2
      IWD(I)=JWD(I)
   10 CONTINUE
C
      DO 20 I=1,8
      CALL ILBYTE(IT,IWD,I-1)
      IF(IT.EQ.X2E) GO TO 200  !Tst for .
      IF(IT.EQ.X44) GO TO 200  !Tst for D
      IF(IT.EQ.X45) GO TO 200  !Tst for E
      IF(IT.EQ.X65) GO TO 200  !Tst for e
   20 CONTINUE
C
  100 READ(CIWD,110,ERR=500)IV
  110 FORMAT(I8)
      XV=DFLOAT(IV)
      KIND=1
      RETURN
C
  200 READ(CIWD,210,ERR=500)XV
  210 FORMAT(E8.0)
      IV=XV
      KIND=2
      RETURN
C
  500 WRITE(CMSSG,505)CIWD
  505 FORMAT('Error decoding string: ',A,' - 0-value returned')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
