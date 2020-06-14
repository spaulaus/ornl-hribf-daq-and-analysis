C$PROG GETPROJ   - Processes Projectile definitions
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE GETPROJ
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
      COMMON/CONSTANT/TMC2, PI, E4, CON(92), DENSITY(92)
      REAL*4          TMC2, PI, E4, CON,     DENSITY
C     ------------------------------------------------------------------
      INTEGER*4       ICH(80)
C
      INTEGER*4       II,I,J,K,L
C
      INTEGER*4       IPL,IPH,IZ
C
      EXTERNAL UNPACK
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL UNPACK(IWD,ICH,80)
      IP=1
      II=5
  355 CALL BLANK(ICH,II,IFLG)
      GO TO (360,390) IFLG
  360 CALL ELEMENTX(ICH,II,IP,IGO,1)
      GO TO (365,380) IGO
  365 CALL NUMBER(ICH,II,IGO,ZP)
      GO TO (370,375) IGO
  370 CALL CHARA(ICH,II,5,IGO)
      GO TO (390,355) IGO
  375 PZ(IP)=ZP
      PA(IP)=0.
  380 IP=IP+1
      GO TO 355
  390 IP=IP-1
      IF (IP.EQ.0) GO TO 910
      IDP=1
      IF (IP.NE.3) GO TO 394
      IF (PZ(2).LE.PZ(3)) GO TO 394
      IDP=PZ(3)
      IPL=PZ(1)
      IPH=PZ(2)
      IP=(IPH-IPL)/IDP+1
      IZ=IPL
      DO 393 I=1,IP
      PZ(I)=IZ
      IZ=IZ+IDP
  393 PA(I)=0.
  394 CONTINUE
      DO 395 I=1,IP
  395 IF (PA(I).EQ.0.) PA(I)=CON(IFIX(PZ(I)+.5))*.6023
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
