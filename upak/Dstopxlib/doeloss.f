C$PROG DOELOSS   - Outputs Eloss table
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE DOELOSS
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
      COMMON/STX04/   IFILE, DEP
      LOGICAL         IFILE
      REAL*4                 DEP
C     ------------------------------------------------------------------
      COMMON/STOPPING/IAVE,CFACT,C2,A0,A1,A2,A3,A4
      REAL*4          IAVE,CFACT,C2,A0,A1,A2,A3,A4
C     ------------------------------------------------------------------
      REAL*4          ERES(500,11),RNGE(10000)
C
      INTEGER*4       ICH(80),IBUF(505)
C
      INTEGER*4       II,I,J,K,L,ITPF,NLN,LOUT
C
      REAL*4          EA,EMINR,C,EL,R,DE
C
      REAL*4          RANGI,STOPP
C
      REAL*4          ELOS,EH,DIF,RNG,RNGL,RNG0,DIFR,DKK,THK
C
      INTEGER*4       IZMX,NE,J1,KK,KKL,IPNT,IFIL
C
      DATA            ITPF/'0C202020'X/
C
      DATA            LOUT/11/
C
      CHARACTER*8     ABSID(10),PROJNAM,EEXIT
C
      DATA            ABSID/'  ABS#01','  ABS#02','  ABS#03','  ABS#04',
     &                      '  ABS#05','  ABS#06','  ABS#07','  ABS#08',
     &                      '  ABS#09','  ABS#10'/
C
      EXTERNAL        UNPACK
C
      SAVE
C
C     ------------------------------------------------------------------
C
   10 FORMAT(1H /)
   20 FORMAT(A4)
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
C
C     ------------------------------------------------------------------
C     OUTPUT ENERGY LOSSES.
C     ------------------------------------------------------------------
C
c     WRITE(LOGUT,10)
c     WRITE(LOGUP,20)ITPF
C
      DO 180 K=1,IE
      ERES(K,1)=EP(K)
  180 CONTINUE
      EH=EP(IE)
C
C     ------------------------------------------------------------------
C     THE TRICK HERE IS TO TRY TO MAKE SOME INTELLIGENT CHOICE
C     FOR THE INTEGRATION STEP SIZE, DE, AND THE STARTING POINT,
C     EMIN.  FOR PARTICLES JUST PUNCHING THRU THIS CAN BE CRUCIAL!!
C
C     MAKE AN ESTIMATE OF WHETHER CLOSE TO STOPPING OR NOT FOR
C     LARGEST Z.
C     ------------------------------------------------------------------
C
      IZMX=1
      DO 190 I=2,IP
      IF(PZ(I).GT.PZ(IZMX)) IZMX=I
  190 CONTINUE
C
      AP=PA(IZMX)
      ZP=PZ(IZMX)
      IZP=ZP+.5
      ELOS=0.
C
      DO 200 J=1,IM
      AT=AA(1,J)
      ZT=AZ(1,J)
      IZT=ZT+.5
      IAVE=IONZ(J)*1.E-06
      EA=EP(1)-ELOS
      IF(EA.LT.0.) GO TO 210
      ELOS=ELOS+STOPP(EA)*THCK(J)*1000./AP
  200 CONTINUE
C
C     ------------------------------------------------------------------
C     CAREFUL-- CLOSE TO STOPPING
C     ------------------------------------------------------------------
C
  210 IF((EA-ELOS).LT.0.5*EA) THEN
C
      DE=AMIN1(EP(1)/100.,DEP/10.)
      EL=DE
      ELSE
C
      DE=AMIN1(EP(1)/10.,DEP/10.)
      EL=DE/10.
C
      ENDIF
C
      IF(DER.GT.0.) DE=DER
      NE=EH/DE
      NE=MIN0(NE,10000)
      DE=EH/NE
      IF(EMINR.GT.0) EL=EMINR
C
      DO 400 I=1,IP
C
      WRITE(LOGUT,10)
      WRITE(LOGUP,20)ITPF
C
      AP=PA(I)
      C=AP/1000.
      ZP=PZ(I)
      IZP=ZP+.5
C
      DO 240 J=1,IM
      J1=J+1
      AT=AA(1,J)
      ZT=AZ(1,J)
      IZT=ZT+.5
      IAVE=IONZ(J)*1.E-06
      THK=THCK(J)
      R0=0.D0
      EA=EL
C
      DO 220 K=1,NE+1
      RNGE(K)=RANGI(EA,DE)*C
      EA=EA+DE
  220 CONTINUE
C
      DO 235 K=1,IE
      EA=ERES(K,J)
      DKK=(EA-EL)/DE+1.
      KK=DKK
C
      IF(KK.GT.0) THEN
      RNG=RNGE(KK)
      ELSE
C
      KK=1
      RNG=0.0
      ENDIF
C
      DIF=DKK-KK
      RNG=(RNGE(KK+1)-RNG)*DIF+RNG
      RNGL=RNG-THK
C
      IF(RNGL.GT.0.) THEN
      KKL=KK
C
  230 IF(RNGL.GT.RNGE(KKL)) THEN
      RNG0=RNGE(KKL)
      ELSE
C
      KKL=KKL-1
      IF(KKL.GT.1) GO TO 230
      RNG0=0.
      ENDIF
C
      DIFR=RNGE(KKL+1)-RNG0
      ERES(K,J1)=(KKL-1.+(RNGL-RNG0)/DIFR)*DE+EL
      ELSE
C
      ERES(K,J1)=0.
      ENDIF
C
  235 CONTINUE
C
  240 CONTINUE
C
      WRITE(LOGUT,250)
      WRITE(LOGUP,250)
C
  250 FORMAT(1H ,/)
C
      CALL GETNUCN(AP,ZP,PROJNAM)
C
      WRITE(LOGUT,260)PROJNAM
      WRITE(LOGUP,260)PROJNAM
C
      WRITE(LOGUT,265)EL,DE
      WRITE(LOGUP,265)EL,DE
C
      WRITE(LOGUT,270)
      WRITE(LOGUP,270)
C
  260 FORMAT(1H ,'Energy loss for ',A/)
C
  265 FORMAT(1H ,'Emin=',F7.2,' keV/Amu   E-step=',F7.2,' keV/Amu')
C
  270 FORMAT(1H ,8('----------')/)
C
      EEXIT=' ExitMeV'
C
      WRITE(LOGUT,290)(ABSID(J),J=1,IM),EEXIT
      WRITE(LOGUP,290)(ABSID(J),J=1,IM),EEXIT
C
  290 FORMAT(1H ,' E0(MeV)',11A)
C
      NLN=0
      DO 310 K=1,IE
C
      WRITE(LOGUT,300) ERES(K,1)*C,((ERES(K,J)-ERES(K,J+1))*C,J=1,IM)
     &                ,ERES(K,IM+1)*C
C
      WRITE(LOGUP,300) ERES(K,1)*C,((ERES(K,J)-ERES(K,J+1))*C,J=1,IM)
     &                ,ERES(K,IM+1)*C
C
  300 FORMAT(' ',F8.3,11(F8.3))
C
      NLN=NLN+1
      IF(NLN.GE.5) THEN
      WRITE(LOGUT,305)
      WRITE(LOGUP,305)
  305 FORMAT(1H )
      NLN=0
      ENDIF
C
  310 CONTINUE
C
C     ------------------------------------------------------------------
C     OUTPUT ENERGY LOSSES TO FILE.
C     ------------------------------------------------------------------
C
      IF(.NOT.IFILE) GO TO 400
C
      IBUF(1)=IZP
      IBUF(2)=EP(1)
      IBUF(3)=IE
      IBUF(4)=DEP
C
      DO 340 K=1,IE
      IBUF(K+4)=ERES(K,IM+1)+.5
  340 CONTINUE
C
      WRITE(LOUT,350) (IBUF(K),K=1,IE+4)
C
  350 FORMAT(10I8)
C
      DO 360 K=1,IE+4
      IBUF(K)=0
  360 CONTINUE
C
  400 CONTINUE
C
      GO TO 500
C
C
  500 RETURN
      END
C
