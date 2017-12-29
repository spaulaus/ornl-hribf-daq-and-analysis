C$PROG DELAYX
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/02/2001
C                               for CAEN support
C     ******************************************************************
C
      SUBROUTINE DELAYX
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/PACP/ DELAZ(30),NDELAZ
      INTEGER*4    DELAZ,    NDELAZ
      DATA         DELAZ/30*0/
      DATA         NDELAZ/30/
C     ------------------------------------------------------------------
      REAL*4       XV
C
      INTEGER*4    DLAVAL,KIND,IERR,IV,I,NDX
C
      INTEGER*4    DLANDX(7)
C
      CHARACTER*8  DLATYP,DLALIS(7)
C
      EQUIVALENCE (DLATYP,LWD(1,2))
C
      DATA         DLANDX/7,13,14,17,18,19,26/
C
      DATA         DLALIS/'LATCH   ',
     &                    'UNCONDIT',
     &                    'CONDIT  ',
     &                    'CAMAC   ',
     &                    'FASTBUS ',
     &                    'FERA    ',
     &                    'VME     '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 20 I=1,7
      NDX=I
      IF(DLATYP.EQ.DLALIS(I)) GO TO 50
   20 CONTINUE
      GO TO 500
C
   50 CALL MILV(LWD(1,3),IV,XV,KIND,IERR)
      IF(IERR.NE.0)            GO TO 510
      IF(IV.LT.0.OR.IV.GT.255) GO TO 520
      NDX=DLANDX(NDX)
      DELAZ(NDX)=IV
      RETURN
C
C     ------------------------------------------------------------------
C     Error returns
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)DLATYP
  505 FORMAT('Illegal delay-type specifier = ',A)
      GO TO 1000
C
  510 WRITE(CMSSG,515)
  515 FORMAT('Syntax error in delay specification')
      GO TO 1000
C
  520 WRITE(CMSSG,525)IV
  525 FORMAT(I8,' is an illegal delay value - 0-255 is legal range')
      GO TO 1000
C
 1000 CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END

