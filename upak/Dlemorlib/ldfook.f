C$PROG LDFOOK    - Checks output ldf-pointer for write-ready
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE LDFOOK(IERR)
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
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      INTEGER*4    IERR,NREC
C
      CHARACTER*4  TYP1,TYP2
C
      EQUIVALENCE (NREC,OUDIR(2))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(OUFOP.NE.'YES ')  GO TO 510   !Tst for file open
C
      IF(OURECI.EQ.NREC)   RETURN      !Tst for positioned for write
C
      READ(LUOUF,REC=NREC  )TYP1
      READ(LUOUF,REC=NREC-1)TYP2
C
      IF(TYP1.EQ.'EOF '.AND.
     &   TYP2.EQ.'EOF ')  GO TO 100
C
      GO TO 500
C
  100 IF(OURECI.EQ.NREC-1) RETURN      !Tst for positioned for write
      IF(OURECI.EQ.NREC-2) RETURN      !Tst for positioned for write
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Output ldf-file pointer not positioned for write')
      GO TO 600
C
  510 WRITE(CMSSG,515)
  515 FORMAT('Output ldf-file not open')
      GO TO 600
C
  600 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
