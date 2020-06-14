C$PROG FINDX     - Sets up find-parameters (bias, fwhm, etc)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE FINDX(IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SM24/ BIAS,IFWHM,IFINDF
      CHARACTER*4             IFINDF
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD
C   
      EQUIVALENCE (KMD,LWD(1,1))
C   
      DATA         BIAS,IFWHM,IFINDF/5.0,5,'OFF '/
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
      IF(KMD.EQ.'NOFI') GO TO 10
                        GO TO 20
C   
   10 IFINDF='OFF '
      RETURN
C   
   20 IFINDF='ON  '
      BIAS=5.0
      IFWHM=5
      IF(NF.LT.2) RETURN
      CALL MILV(LWD(1,2),IT,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      BIAS=XV
      IF(NF.LT.3) RETURN
      CALL MILV(LWD(1,3),IT,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 100
      IFWHM=IT
      RETURN
C   
  100 IERR=1
      WRITE(CMSSG,105)
  105 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - CMD IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
