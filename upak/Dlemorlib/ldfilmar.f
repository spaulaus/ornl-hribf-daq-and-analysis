C$PROG LDFILMAR  - Writes file-mark records to LDF-files
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE LDFILMAR(NEOF,NBAK,IERR)
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
      INTEGER*4    NREC
C
      EQUIVALENCE (NREC,  OUDIR(2))           !Total number of records
C     ------------------------------------------------------------------
      INTEGER*4    NEOF,NBAK,IERR,I
C
      INTEGER*4    IBUF(8192),NUMBY
C
      DATA         NUMBY/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(NEOF.LE.0) GO TO 20
C
      DO 10 I=1,NEOF
      CALL LDFWRIT(LUOUF,'EOF ',NUMBY,IBUF,IERR)
      IF(IERR.NE.0) RETURN
   10 CONTINUE
C
   20 IF(NBAK.LE.0) RETURN
C
      OURECI=NREC
C
      DO 30 I=1,NBAK
      OURECI=OURECI-1
      IF(OURECI.LT.1) OURECI=1
   30 CONTINUE
      RETURN
      END
