C$PROG GETENERGY - Processes Ion Energy specifications
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE GETENERGY
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
      COMMON/PROJ/    PZ(100),PA(100),EP(500)
      REAL*4          PZ,     PA,     EP
C     ------------------------------------------------------------------
      COMMON/PARTICLE/IZT,IZP,ZT,ZP,AT,AP,SPWRH
      INTEGER*4       IZT,IZP
      REAL*4                  ZT,ZP,AT,AP,SPWRH
C     ------------------------------------------------------------------
      COMMON/STX02/   IM,IP,IE
      INTEGER*4       IM,IP,IE
C     ------------------------------------------------------------------
      COMMON/STX03/   IFLG,IGO,ILP,IHP,NK,IDP,IPC
      INTEGER*4       IFLG,IGO,ILP,IHP,NK,IDP,IPC
C     ------------------------------------------------------------------
      COMMON/STX04/   IFILE, DEP
      LOGICAL         IFILE
      REAL*4                 DEP
C     ------------------------------------------------------------------
      COMMON/STX05/   IDIV
      LOGICAL         IDIV
C     ------------------------------------------------------------------
      COMMON/CONSTANT/TMC2, PI, E4, CON(92), DENSITY(92)
      REAL*4          TMC2, PI, E4, CON,     DENSITY
C     ------------------------------------------------------------------
      INTEGER*4       ICH(80)
C
      INTEGER*4       II,I,J,K,L
C
      INTEGER*4       IPL,IPH,IZ
C
      REAL*4          EL,EH,EA
C
      EXTERNAL        UNPACK
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL UNPACK(IWD,ICH,80)
      IE=1
      II=3
  405 CALL BLANK(ICH,II,IFLG)
      GO TO (410,440) IFLG
  410 CALL NUMBER(ICH,II,IGO,EA)
      GO TO (430,420) IGO
  420 IF (EA.LE.0.) GO TO 410
      EP(IE)=EA*1000.
      IE=IE+1
      GO TO 410
  430 CALL CHARA(ICH,II,5,IGO)
      GO TO (440,405) IGO
  440 IE=IE-1
      IF (IE.EQ.0) GO TO 910
      DEP=EP(1)
      EL=DEP
      IF (IE.EQ.3) GO TO 444
      IF (IE.LT.3) GO TO 448
      DO 441 I=2,IE
  441 IF (EP(I).LE.EP(I-1)) GO TO 910
      GO TO 448
  444 DEP=EP(3)
      IF (EP(2).LT.DEP) GO TO 448
      EH=EP(2)
      IF ((EH-EL).LT.DEP) GO TO 448
      IE=(EH-EL)/DEP+1.5
      EA=EL
      DO 446 I=1,IE
      EP(I)=EA
  446 EA=EA+DEP
  448 CONTINUE
      IDIV=.FALSE.
      GO TO 500
C
  910 WRITE(LOGUT,9100)
      WRITE(LOGUP,9100)
c
 9100 FORMAT(' ARGUMENT LIST NOT UNDERSTOOD.')
      GO TO 500
C
C
  500 RETURN
      END
