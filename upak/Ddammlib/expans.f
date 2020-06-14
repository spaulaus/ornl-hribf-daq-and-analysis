C$PROG EXPANS    - Expands fit display
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE EXPANS(IDW)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/SM08/ MARFS(4,20),JLOX,JHIX,JLOF,JHIF,SLOX,SHIX,SLOF,SHIF
C
      CHARACTER*4  MARFS
      INTEGER*4                JLOX,JHIX,JLOF,JHIF
      REAL*4                                       SLOX,SHIX,SLOF,SHIF
C     ------------------------------------------------------------------
      CHARACTER*4  KDSP
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(JLOX.LT.0)    GO TO 200
      IF(JHIX.LE.JLOX) GO TO 200
C
      ILO=JLOX+1
      IHI=JHIX+1
      NCH=IHI-ILO+1
      KDSP=WINFLC(4,IDW)
C
      CALL PLOTUMS(IDW,ILO,NCH,KDSP)
C
      RETURN
C   
C     ------------------------------------------------------------------
C     RETURN ERROR MESSAGES
C     ------------------------------------------------------------------
C   
  200 WRITE(CMSSG,205)
  205 FORMAT('ILLEGAL OR UNSPECIFIED EXPAND REGION')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
