C$PROG DODEDX    - Outputs DE/DX table
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE DODEDX
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
      INTEGER*4       ICH(80)
C
      INTEGER*4       II,I,J,K,L,ITPF,NLN,N,NN
C
      REAL*4          EA,EMINR
C
      REAL*4          STOPP
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
  170 IF (IFLG.NE.0) GO TO 500
C
C     ------------------------------------------------------------------
C     Here is where we start doing it
C     ------------------------------------------------------------------
C
      DO 270 J=1,IM
C
      WRITE(LOGUT,50)
      WRITE(LOGUP,55)ITPF
C
      WRITE(LOGUT,200)J
      WRITE(LOGUP,200)J
C
      WRITE(LOGUT,205)
      WRITE(LOGUP,205)
C
  200 FORMAT(1H ,'Stopping power (MeV/(mg/cm**2)) in absorber#',I2)
C
  205 FORMAT(1H ,8('----------'))
C
      ZT=AZ(1,J)
      IZT=ZT+.5
      AT=AA(1,J)
      IAVE=IONZ(J)*1.E-06
      IPC=IP
      ILP=1
C
  210 IDP=MIN0(IPC,7)-1
      IHP=ILP+IDP
C
      NN=0
      DO 215 N=ILP,IHP
      NN=NN+1
      CALL GETNUCN(PA(N),PZ(N),PRONAM(NN))
  215 CONTINUE
C
      WRITE(LOGUT,220) (PRONAM(I),I=1,NN)
      WRITE(LOGUP,220) (PRONAM(I),I=1,NN)
C
  220 FORMAT(/,'  MeV/Amu',8A)
C
      NLN=0
      DO 250 L=1,IE
      EA=EP(L)
      NK=0
C
      DO 240 I=ILP,IHP
      ZP=PZ(I)
      IZP=ZP+.5
      AP=PA(I)
      NK=NK+1
      SPWR(NK)=STOPP(EA)
  240 CONTINUE
C
      WRITE(LOGUT,260)EA/1000.0,(SPWR(K),K=1,NK)
      WRITE(LOGUP,260)EA/1000.0,(SPWR(K),K=1,NK)
C
      NLN=NLN+1
      IF(NLN.GE.5) THEN
      WRITE(LOGUT,245)
      WRITE(LOGUP,245)
  245 FORMAT(1H )
      NLN=0
      ENDIF
C
  250 CONTINUE
C
  260 FORMAT(1H ,F8.3,8(F8.3))
C
      ILP=IHP+1
      IF(ILP.GT.IP) GO TO 270
      IPC=IPC-IDP-1
      GO TO 210
C
  270 CONTINUE
C
C
  500 RETURN
      END
C
