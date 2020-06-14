C$PROG STATMAN   - Displays/logs certain status information
C
C     ******************************************************************
C     BY W.T. Milner at HRIBF - last modified 04/25/2002
C     ******************************************************************
C
      SUBROUTINE STATMAN
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/STX02/   IM,IP,IE
      INTEGER*4       IM,IP,IE
C     ------------------------------------------------------------------
      COMMON/ABSS/    AZ(5,10),AA(5,10),INUM(5,10),IKEY(5,10),
     &                FRACT(5,10),NCOM(10),PRES(10),THCK(10),IONZ(10)
C
      REAL*4          AZ,AA,FRACT,PRES,THCK,IONZ
C
      INTEGER*4       INUM,IKEY,NCOM
C     ------------------------------------------------------------------
      COMMON/STX06/   THCKCM(10)
      REAL*4          THCKCM
C     ------------------------------------------------------------------
      INTEGER*4       J
C     ------------------------------------------------------------------
C
      WRITE(LOGUT,340)
      WRITE(LOGUP,340)
C
      WRITE(LOGUT,350)
      WRITE(LOGUP,350)
C
  340 FORMAT(/,' EQUIVALENT ABSORBER CHARGE AND MASS.')
C
  350 FORMAT(/,' ABSORBER # ','      Z*       A*    IAVE(EV)     ',
     &         'P(TORR) T(MG/CM**2)       T(CM)')
C
      DO 360 J=1,IM
C
      WRITE(LOGUT,355) J,AZ(1,J),AA(1,J),IONZ(J),PRES(J),THCK(J),
     &                 THCKCM(J)
C
      WRITE(LOGUP,355) J,AZ(1,J),AA(1,J),IONZ(J),PRES(J),THCK(J),
     &                 THCKCM(J)
C
  355 FORMAT(' ',4X,I2,4X,2F9.3,1P4E12.4)
C
  360 CONTINUE
C
      RETURN
      END
