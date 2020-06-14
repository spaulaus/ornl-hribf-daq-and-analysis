C$PROG RESETX    - Resetx window dimensions to 24 x 80
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE RESETX(LU,ISET)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SD04/ SNITFIL(20)
      INTEGER*4    SNITFIL
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
C
      CHARACTER*4  ISET
C
      SAVE
C
C     ------------------------------------------------------------------
C
      ISET='NO  '
C
      MAXR=24
      MAXC=80
C
      CALL LOGOPEN(LU,SNITFIL,IERR)
C
      CALL SETUP(ISET)
C
      RETURN
C
      END
