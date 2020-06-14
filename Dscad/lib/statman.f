C$PROG STATMAN   - Status display routine for SCAD
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 11/10/99
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
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD1/ GNITNAM
      CHARACTER*80 GNITNAM
C     ------------------------------------------------------------------
      COMMON/SCD2/ MSPRED,DISPSEC
      INTEGER*4    MSPRED,DISPSEC
C     ------------------------------------------------------------------
      COMMON/SD04/ SNITFIL(20)
      INTEGER*4    SNITFIL
C
      CHARACTER*80 SNITNAM
      EQUIVALENCE  (SNITNAM,SNITFIL)
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
      INTEGER*4    I,J,N
C
      CHARACTER*4  STATE
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     Display tabular related status
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 FORMAT('===================================================',
     &       '=============')
C
  102 FORMAT('---------------------------------------------------',
     &       '-------------')
C
      WRITE(CMSSG,105)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
  105 FORMAT('TABULAR DISPLAY INFORMATION')
C
      WRITE(CMSSG,115)SNITNAM
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,120)SEC
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,125)LSEC
      CALL MESSLOG(LOGUT,LOGUP)
C
  115 FORMAT('Snit-file name   = ',A)
  120 FORMAT('Display interval = ',I5,' seconds')
  125 FORMAT('Log     interval = ',I5,' seconds')
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,130)
  130 FORMAT('NAME            C   N   A   F')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 150 N=1,NR
      WRITE(CMSSG,140)(LA(I,N),I=1,3),CN(N),SN(N),A(N),F(N)
  140 FORMAT(3A4,1X,4I4)
      CALL MESSLOG(LOGUT,LOGUP)
  150 CONTINUE
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Display tabular-limit related status
C     ------------------------------------------------------------------
C
      IF(NLIM.LE.0) GO TO 200
C
      WRITE(CMSSG,160)
  160 FORMAT('Tabular Limit Specifications    LOW        HIGH')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 170 J=1,NLIM
      N=SCNDX(J)
      WRITE(CMSSG,165)(LA(I,N),I=1,3),LIMLO(J),LIMHI(J)
  165 FORMAT(3A4,11X,2F12.1)
      CALL MESSLOG(LOGUT,LOGUP)
  170 CONTINUE
C
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Display graphic related status
C     ------------------------------------------------------------------
C
  200 IF(NSCA.LE.0) RETURN
C
      WRITE(CMSSG,210)
  210 FORMAT('GRAPHIC DISPLAY INFORMATION')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,220)GNITNAM
  220 FORMAT('Gnit-file name = ',A)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,230)DISPSEC
  230 FORMAT('Displays/sec   = ',I2)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,240)
  240 FORMAT('NAME            C   N   A   F  NAGV  STATE')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 300 N=1,NSCA
C
      STATE=' OFF'
C
      IF(RATTYP(N).EQ.'SCAL') STATE='  ON'
C
      WRITE(CMSSG,250)(LAG(I,N),I=1,3),CC(N),NN(N),AA(N),FF(N),
     &                RATAVG(N),STATE
  250 FORMAT(3A4,1X,4I4,I6,3X,A)
      CALL MESSLOG(LOGUT,LOGUP)
C
  300 CONTINUE
C
      WRITE(CMSSG,102)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Display Meter limit related status
C     ------------------------------------------------------------------
C
CX    IF(NLIMG.LE.0) RETURN
C
      WRITE(CMSSG,310)
  310 FORMAT('Meter Limit Specifications      LOW        HIGH')
C
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 320 J=1,NSCA
      WRITE(CMSSG,315)(LAG(I,J),I=1,3),GLIMLO(J),GLIMHI(J),GLIMON(J)
  315 FORMAT(3A4,11X,2F12.1,2X,A4)
      CALL MESSLOG(LOGUT,LOGUP)
  320 CONTINUE
C
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
C
      RETURN
C
      END
