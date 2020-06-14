C$PROG GETABSORB - Processes Absorber definitions
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE GETABSORB
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
      COMMON/III/  LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
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
      COMMON/STX05/   IDIV
      LOGICAL         IDIV
C     ------------------------------------------------------------------
      COMMON/ABSS/    AZ(5,10),AA(5,10),INUM(5,10),IKEY(5,10),
     &                FRACT(5,10),NCOM(10),PRES(10),THCK(10),IONZ(10)
C
      REAL*4          AZ,AA,FRACT,PRES,THCK,IONZ
C
      INTEGER*4       INUM,IKEY,NCOM
C     ------------------------------------------------------------------
      COMMON/ARRAYS/  ION(92), IONGS(92), C2S(92),
     &                A0S(10), A1S(10),   A2S(10), A3S(10), A4S(10)
C
      REAL*4          ION,     IONGS,     C2S,
     &                A0S,     A1S,       A2S,     A3S,     A4S
C     ------------------------------------------------------------------
      COMMON/CONSTANT/TMC2, PI, E4, CON(92), DENSITY(92)
      REAL*4          TMC2, PI, E4, CON,     DENSITY
C     ------------------------------------------------------------------
      COMMON/STOPPING/IAVE,CFACT,C2,A0,A1,A2,A3,A4
      REAL*4          IAVE,CFACT,C2,A0,A1,A2,A3,A4
C     ------------------------------------------------------------------
      COMMON/STX06/   THCKCM(10)
      REAL*4          THCKCM
C     ------------------------------------------------------------------
      INTEGER*4       I,J,K
C
      INTEGER*4       IZ,NC,JN,INDX,KEY,ICNT,INSUM
C
      REAL*4          FSUM,FRCT,ZSUM,ASUM,AVEI
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 60 J=1,10
      DO 50 I=1,5
      AZ(I,J)=0
      AA(I,J)=0
      INUM(I,J)=0
      IKEY(I,J)=0
      FRACT(I,J)=0.
      NCOM(J)=0
      PRES(J)=0.
      THCK(J)=0.
   50 CONTINUE
   60 CONTINUE
C
      IM=1
      WRITE(LOGUT,110)
      WRITE(LOGUP,110)
C
  100 WRITE(LOGUT,115)IM
      WRITE(LOGUP,115)IM
C
  110 FORMAT(' ','ABSORBER DEFINITIONS:')
  115 FORMAT(' ABSORBER #',I2,':')
C
      READ(LIN,120,END=400,ERR=400)IWD
C
  120 FORMAT(20A4)
C
      CALL CASEUP(IWD)
C
      IF(LIN.NE.5) WRITE(LOGUT,125)IWD
C
  125 FORMAT(1H ,19A4,A2)
C
      WRITE(LOGUP,130)(IWD(I),I=1,20)
C
  130 FORMAT(1H ,20A4)
C
      CALL ABSORBER(IWD,80,IM,IFLG)
C
      IF(PRES(IM).GT.0..OR.THCK(IM).GE.0.)                GO TO 150
C
      IF(NCOM(IM).EQ.1.AND.DENSITY(IFIX(AZ(1,IM))).NE.1.) GO TO 150
C
C     ------------------------------------------------------------------
C     DO NOT HAVE DENSITY OF SOLID TO CONVERT FROM CM TO MG/CM2
C     ------------------------------------------------------------------
C
      WRITE(LOGUT,140)
C
  140 FORMAT(' SORRY - DO NOT HAVE DENSITY FOR THIS SOLID - ',
     & /,' MUST SPECIFY MG/CM**2')
C
      GO TO 100
C
  150 CONTINUE
C
      GO TO (170,180,100),IFLG
C
  170 IM=IM+1
      IF(IM.GT.10) GO TO 180
      GO TO 100
C
  180 IM=IM-1
C
      DO 330 J=1,IM
      NC=NCOM(J)
C
      DO 190 I=1,NC
      JN=INUM(I,J)
      IZT=AZ(I,J)
      IF(AA(I,J).EQ.0.) AA(I,J)=CON(IZT)*0.6023
      AA(I,J)=AA(I,J)*JN
      AZ(I,J)=AZ(I,J)*JN
  190 CONTINUE
C
      IF(PRES(J).EQ.0.) GO TO 230
C
C     ------------------------------------------------------------------
C     CALCULATE EFFECTIVE Z OF GAS MOLECULE.
C     ------------------------------------------------------------------
C
      INDX=1
      FSUM=INUM(1,J)*FRACT(1,J)
      IZ=AZ(1,J)/INUM(1,J)
      IAVE=IONGS(IZ)*FSUM
      IF (IAVE.EQ.0.) IAVE=ION(IZ)*FSUM
      KEY=IKEY(1,J)
      INUM(1,J)=1
C
      DO 220 I=2,NC
      IZ=AZ(I,J)/INUM(I,J)
      AVEI=IONGS(IZ)
      IF (AVEI.EQ.0.) AVEI=ION(IZ)
      IF (IKEY(I,J).EQ.KEY) GO TO 200
      INDX=INDX+1
      AZ(INDX,J)=AZ(I,J)
      AA(INDX,J)=AA(I,J)
      INUM(INDX,J)=1
      FRACT(INDX,J)=FRACT(I,J)
      KEY=IKEY(I,J)
      GO TO 210
C
  200 AZ(INDX,J)=AZ(INDX,J)+AZ(I,J)
      AA(INDX,J)=AA(INDX,J)+AA(I,J)
C
  210 FRCT=INUM(I,J)*FRACT(INDX,J)
      FSUM=FSUM+FRCT
      IAVE=IAVE+AVEI*FRCT
C
  220 CONTINUE
C
      NC=INDX
      IONZ(J)=IAVE/FSUM
      GO TO 300
C
C     ------------------------------------------------------------------
C     CALCULATE RELATIVE AMOUNTS IN SOLID COMPOUNDS.
C     ------------------------------------------------------------------
C
  230 ICNT=0
      FRCT=FRACT(1,J)
      INSUM=INUM(1,J)
      KEY=IKEY(1,J)
C
      DO 280 I=2,NC+1
C
      IF (I.GT.NC.AND.ICNT.NE.0) GO TO 240
      IF (I.GT.NC)               GO TO 280
      IF (IKEY(I,J).EQ.KEY)      GO TO 270
      IF (ICNT.EQ.0)             GO TO 260
C
  240 FRCT=FRCT/INSUM
      DO 250 K=I-ICNT-1,I-1
      FRACT(K,J)=FRCT
  250 CONTINUE
      ICNT=0
C
  260 FRCT=FRACT(I,J)
      INSUM=INUM(I,J)
      KEY=IKEY(I,J)
      GO TO 280
C
  270 INSUM=INSUM+INUM(I,J)
      ICNT=ICNT+1
C
  280 CONTINUE
C
C     ------------------------------------------------------------------
C     CALCULATE AVERAGE IONIZATION ENERGY OF SOLID
C     ------------------------------------------------------------------
C
      IAVE=0.
      DO 290 I=1,NC
      IZ=IFIX(AZ(I,J))/INUM(I,J)
      IAVE=IAVE+FRACT(I,J)*INUM(I,J)*ION(IZ)
  290 CONTINUE
C
C     ------------------------------------------------------------------
C     CALCULATE EFFECTIVE CHARGE AND MASS OF MIXTURE.
C     ------------------------------------------------------------------
C
  300 ZSUM=0.
      ASUM=0.
      FSUM=0.
C
      DO 310 I=1,NC
      FRCT=FRACT(I,J)
      FSUM=FSUM+FRCT*INUM(I,J)
      ZSUM=ZSUM+FRCT*AZ(I,J)
      ASUM=ASUM+FRCT*AA(I,J)
  310 CONTINUE
C
      AZ(1,J)=ZSUM/FSUM
      AA(1,J)=ASUM/FSUM
      IF (PRES(J).EQ.0.) GO TO 320
C
C     ------------------------------------------------------------------
C     CONVERT THICKNESS FROM CM TO MILIGRAMS/CM**2 FOR GAS.
C     ------------------------------------------------------------------
C
      IF(THCK(J).LT.0.) THCK(J)=-AA(1,J)*THCK(J)*PRES(J)/(22.4*760.)
      THCKCM(J)=THCK(J)*(22.4*760.)/(PRES(J)*AA(1,J))
      GO TO 330
C
  320 CONTINUE
C
C     ------------------------------------------------------------------
C     FOR SOLID  -  FOR SOLID  -  FOR SOLID  -  FOR SOLID
C     ------------------------------------------------------------------
C
      IONZ(J)=IAVE/FSUM
      IF (THCK(J).LT.0.) THCK(J)=-THCK(J)*DENSITY(IZ)*1000.
      THCKCM(J)=THCK(J)/(DENSITY(IZ)*1000.)
C
  330 CONTINUE
C
C
      WRITE(LOGUT,340)
      WRITE(LOGUP,340)
C
      WRITE(LOGUT,350)
      WRITE(LOGUP,350)
C
  340 FORMAT(/,' EQUIVALENT ABSORBER CHARGE AND MASS.')
C
  350 FORMAT(/,' ABSORBER # ','      Z*       A*    IAVE(EV)     ',
     &         'P(TORR) T(MG/CM**2)       T(CM)')
C
      DO 360 J=1,IM
C
      WRITE(LOGUT,355) J,AZ(1,J),AA(1,J),IONZ(J),PRES(J),THCK(J),
     &                 THCKCM(J)
C
      WRITE(LOGUP,355) J,AZ(1,J),AA(1,J),IONZ(J),PRES(J),THCK(J),
     &                 THCKCM(J)
C
  355 FORMAT(' ',4X,I2,4X,2F9.3,1P4E12.4)
C
  360 CONTINUE
C
      GO TO 500
C
C     ------------------------------------------------------------------
C     RETURN
C     ------------------------------------------------------------------
C
  400 CONTINUE
C
  500 RETURN
      END
