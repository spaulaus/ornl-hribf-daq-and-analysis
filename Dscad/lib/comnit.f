C$PROG COMNIT    - Defines/sets common
C
C     ******************************************************************
C     BY W.T. MILNER AT ORPH - LAST MODIFIED 01/25/2000
C     ******************************************************************
C
      SUBROUTINE COMNIT
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
      COMMON/DD03/ NCALLR
      INTEGER*4    NCALLR
      DATA         NCALLR/0/
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C
      DATA         RATFLG  /'RON '/
      DATA         RATAVG  /9*5/
      DATA         RATPAR  /1,2,3,4,5,6,7,8,9/
      DATA         RATTYP  /9*'SCAL'/
      DATA         RATDSP  /'LOG '/
CX    DATA         NRATE   /1/
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN,ISOPEN
      character*4  CISOPEN
      equivalence  (CISOPEN,ISOPEN)
C
      DATA         CISOPEN/'NO  '/
      DATA         NUMWIN/0/
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
      character*4  CMODE_PL
      equivalence  (CMODE_PL, MODE_PL)
C
      DATA         CMODE_PL/'LIN '/
C     ------------------------------------------------------------------
      COMMON/SCD2/ MSPRED,DISPSEC
      INTEGER*4    MSPRED,DISPSEC
C
      DATA         MSPRED /100/
      DATA         DISPSEC/10/
C     ------------------------------------------------------------------
      COMMON/SCD3/ DISPTYP
      CHARACTER*4  DISPTYP
C
      DATA         DISPTYP/'GLIN'/
C     ------------------------------------------------------------------
C
      INTEGER*4    MODE,NN
C
      SAVE
C
C     ==================================================================
C
C
      RETURN
      END
