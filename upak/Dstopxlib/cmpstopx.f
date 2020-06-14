C$PROG CMPSTOPX  - Command processor for program STOPXX
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     PROGRAM STOPX
C     WRITTEN BY T.C.AWES ORNL
C     JULY 1983
C     CALCULATES ENERGY LOSSES FOR ALL Z'S IN ANY ABSORBER
C     OR SERIES OF ABSORBERS.
C     ------------------------------------------------------------------
C
      SUBROUTINE CMPSTOPX(IDONE,IERR)
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/HEP/  IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
      COMMON/PROJ/    PZ(100),PA(100),EP(500)
      REAL*4          PZ,     PA,     EP
C     ------------------------------------------------------------------
      COMMON/STX02/   IM,IP,IE
      INTEGER*4       IM,IP,IE
C     ------------------------------------------------------------------
      COMMON/STX04/   IFILE, DEP
      LOGICAL         IFILE
      REAL*4                 DEP
C     ------------------------------------------------------------------
      COMMON/STX05/   IDIV
      LOGICAL         IDIV
C     ------------------------------------------------------------------
      INTEGER*4    NAMCMD(20),IHELP(20,400)
C
      INTEGER*4    IERR,LHEP,ITPF,I
C
      DATA         LHEP/13/
C
      CHARACTER*4  IDONE,KMD
C
      REAL*4       AP
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      DATA         ITPF/'0C202020'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
C
      IF(IP.NE.1) GO TO 30
      IF(IE.EQ.0) GO TO 30
      IF(IDIV)    GO TO 30
C
      AP=PA(1)
      DEP=DEP/AP
C
      DO 10 I=1,IE
      EP(I)=EP(I)/AP
   10 CONTINUE
      IDIV=.TRUE.
C
   30 CONTINUE
C
      IF(KMD.EQ.'H   ') GO TO 100
      IF(KMD.EQ.'CMD ') GO TO 110
      IF(KMD.EQ.'TPF ') GO TO 120
      IF(KMD.EQ.'STAT') GO TO 130
C
      IF(KMD.EQ.'ABSB') GO TO 150
      IF(KMD.EQ.'DEDX') GO TO 200
      IF(KMD.EQ.'RNGE') GO TO 250
      IF(KMD.EQ.'ELOS') GO TO 300
      IF(KMD.EQ.'PROJ') GO TO 350
      IF(KMD.EQ.'EA  ') GO TO 400
C
      IF(KMD.EQ.'FILE') GO TO 550
C
      IF(KMD.EQ.'END ') STOP
C
      GO TO 900
C
  100 CALL HELPMANU(IWD,LHEP,IHELP,400,20,IHEPF)
      GO TO 2500
C
  110 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)  !OPEN COMMAND FILE
      GO TO 2500
C
  120 WRITE(LOGUP,125)ITPF
  125 FORMAT(A4)
      GO TO 2500
C
  130 CALL STATMAN
      GO TO 2500
C
  150 CALL GETABSORB     !Process Absorber definitions
      GO TO 2500
C
C
  200 CALL DODEDX        !Output Stopping Powers
      GO TO 2500
C
C
  250 CALL DORANGE       !Output Ranges
      GO TO 2500
C
C
  300 CALL DOELOSS       !Output Energy Losses
      GO TO 2500
C
C
  350 CALL GETPROJ       !Input Projectile definitions
      GO TO 2500
C
C
  400 CALL GETENERGY     !Input incident energy per nucleon
      GO TO 2500
C
C
  550 CALL GETFILE       !Assign file to receive energy losses
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Error Messages
C     ------------------------------------------------------------------
C
  900 WRITE(LOGUT,1000)
      WRITE(LOGUP,1000)
C
 1000 FORMAT(' Illegal command or syntax error - Ignored')
      GO TO 2500
C
C     ------------------------------------------------------------------
C     RETURN
C     ------------------------------------------------------------------
C
 2500 IDONE='YES '
C
      RETURN
      END
