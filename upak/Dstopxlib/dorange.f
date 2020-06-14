C$PROG DORANGE   - Outputs Range table
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE DORANGE
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
      COMMON/ABSS/    AZ(5,10),AA(5,10),INUM(5,10),IKEY(5,10),
     &                FRACT(5,10),NCOM(10),PRES(10),THCK(10),IONZ(10)
C
      REAL*4          AZ,AA,FRACT,PRES,THCK,IONZ
C
      INTEGER*4       INUM,IKEY,NCOM
C     ------------------------------------------------------------------
      COMMON/PROJ/    PZ(100),PA(100),EP(500)
      REAL*4          PZ,     PA,     EP
C     ------------------------------------------------------------------
      COMMON/PARTICLE/IZT,IZP,ZT,ZP,AT,AP,SPWRH
      INTEGER*4       IZT,IZP
      REAL*4                  ZT,ZP,AT,AP,SPWRH
C     ------------------------------------------------------------------
      COMMON/RUNGE/   DER,EMIN,E0,DUMY,R0
      REAL*4          DER,EMIN,E0,DUMY
      REAL*8                           R0
C     ------------------------------------------------------------------
      COMMON/STX01/   SPWR(10)
      REAL*4          SPWR
C     ------------------------------------------------------------------
      COMMON/STX02/   IM,IP,IE
      INTEGER*4       IM,IP,IE
C     ------------------------------------------------------------------
      COMMON/STX03/   IFLG,IGO,ILP,IHP,NK,IDP,IPC
      INTEGER*4       IFLG,IGO,ILP,IHP,NK,IDP,IPC
C     ------------------------------------------------------------------
      COMMON/STOPPING/IAVE,CFACT,C2,A0,A1,A2,A3,A4
      REAL*4          IAVE,CFACT,C2,A0,A1,A2,A3,A4
C     ------------------------------------------------------------------
      REAL*4          ERES(500,11)
C
      INTEGER*4       ICH(80)
C
      INTEGER*4       II,I,J,K,L,ITPF,NLN
C
      REAL*4          EA,EMINR,C,EL,R,DE
C
      REAL*4          RANGI
C
      DATA            ITPF/'0C202020'X/
C
      CHARACTER*8     PRONAM(8)
C
      EXTERNAL        UNPACK
C
      SAVE
C
C     ------------------------------------------------------------------
C
   50 FORMAT(1H /)
   55 FORMAT(A4)
C
      CALL UNPACK(IWD,ICH,80)
      II=5
      CALL BLANK(ICH,II,IFLG)
C
      GO TO(100,120) IFLG
C
  100 CALL NUMBER(ICH,II,IGO,EMINR)
C
      GO TO(120,110) IGO
C
  110 CALL NUMBER(ICH,II,IGO,DER)
C
C     ------------------------------------------------------------------
C     ERROR MESSAGES.
C     ------------------------------------------------------------------
C
  120 IFLG=0
      IF(IM.NE.0) GO TO 130
      IFLG=1
C
      WRITE(LOGUT,125)
      WRITE(LOGUT,125)
C
  125 FORMAT(' MUST DEFINE ABSORBER MATERIAL.')
C
  130 IF(IP.NE.0) GO TO 150
      IFLG=1
C
      WRITE(LOGUT,140)
      WRITE(LOGUP,140)
C
  140 FORMAT(' MUST DEFINE STOPPING PROJECTILES')
C
  150 IF (IE.NE.0) GO TO 170
      IFLG=1
C
      WRITE(LOGUT,160)
      WRITE(LOGUP,160)
C
  160 FORMAT(' MUST ENTER INCIDENT ENERGIES.')
C
C     ------------------------------------------------------------------
C     This is where we start doing it
C     ------------------------------------------------------------------
C
  170 IF(IFLG.NE.0) GO TO 500
C
      DO 300 J=1,IM
C
      ZT=AZ(1,J)
      IZT=ZT+.5
      AT=AA(1,J)
      IAVE=IONZ(J)*1.E-06
      IHP=MIN0(IP,8)
C
      EL=EP(1)/100.
      IF(EMINR.GT.0) EL=EMINR
C
      WRITE(LOGUT,50)
      WRITE(LOGUP,55)ITPF
C
      WRITE(LOGUT,180)J,EL
      WRITE(LOGUP,180)J,EL
C
      WRITE(LOGUT,185)
      WRITE(LOGUP,185)
C
  180 FORMAT(/,' Range (mg/cm**2) in absorber#',I2,'   Emin=',F7.2,
     &         ' keV/Amu')
C
  185 FORMAT(1H ,8('----------')/)
C
      DO 220 I=1,IHP
      ZP=PZ(I)
      IZP=ZP+.5
      AP=PA(I)
      R0=0.D0
      C=AP/1000.
      EL=EP(1)/100.
      IF(EMINR.GT.0) EL=EMINR
C
      R=RANGI(EL,EL)
C
      DO 200 L=1,IE
      EA=EP(L)
      DE=(EA-EL)*IE/100.
      IF (DER.GT.0.) DE=DER
      ERES(L,I)=RANGI(EA,DE)*C
      EL=EA
  200 CONTINUE
C
      CALL GETNUCN(PA(I),PZ(I),PRONAM(I))
C
  220 CONTINUE
C
      WRITE(LOGUT,310)(PRONAM(I),I=1,IHP)
      WRITE(LOGUP,310)(PRONAM(I),I=1,IHP)
C
      NLN=0
      DO 240 L=1,IE
C
      WRITE(LOGUT,320) EP(L)/1000.0,(ERES(L,I),I=1,IHP)
      WRITE(LOGUP,320) EP(L)/1000.0,(ERES(L,I),I=1,IHP)
C
      NLN=NLN+1
      IF(NLN.GE.5) THEN
      WRITE(LOGUT,230)
      WRITE(LOGUP,230)
  230 FORMAT(1H )
      NLN=0
      ENDIF
C
  240 CONTINUE
C
  300 CONTINUE
C
  310 FORMAT(1H ,' MeV/Amu',8A)
  320 FORMAT(1H ,F8.3,8(F8.3))
C
  500 RETURN
      END
C
