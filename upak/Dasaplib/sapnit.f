C$PROG SAPNIT
      SUBROUTINE SAPNIT
C
C     **************************************************************
C     INITIALIZES STUFF FOR PROGRAM ASAP
C     **************************************************************
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
      COMMON/HEPF/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/III/ LIN,LCM,LCI
C     ------------------------------------------------------------------
      COMMON/CCC/ BGD(256),WT(256),YCAL(256),GAU(200),NG(16)
      COMMON/DDD/ IHOL(16),XP(16),AREA(16),BETA(18),LOCA(250),BIAS
      COMMON/EEE/ ID,ILO,IHI,KWD,FWHMA,FWHMB,EOOO,GAINN,WDNC,DELCH,BSTD
      COMMON/FFF/ HWID(16),QFN,QFLO,IFBGD,NTRY,MSN,NCH,KPK,I1,I2
      COMMON/GGG/ MXNPK,MXRGL,MXPIS,NBNF
      COMMON/HHH/ IGAUS,KNBAK,KNVAR,KVW
      CHARACTER*4                   KVW
      COMMON/SSS/ LUS,LUH,LUD,LUT,KFIL
      CHARACTER*4                 KFIL
      COMMON/TTT/ ITITL(19)
C
      INTEGER*4 NAMHEP(6)
      CHARACTER*24 CNAMHEP
      EQUIVALENCE (CNAMHEP,NAMHEP)
      DATA CNAMHEP/'asap.hep                '/
C
      character*4 cITITL(19)
      equivalence (cITITL, ITITL)
      DATA cITITL/19*'    '/
C
      character*4 cNAMPROG(2)
      equivalence (cNAMPROG, NAMPROG)
      DATA cNAMPROG/'ASAP','    '/
C
      SAVE
C
C     **************************************************************
C     LUS = 10 = LOGICAL UNIT FOR SPK-FILE
C     LUH      = CHANNEL #    FOR HIS-FILE
C     LUD = 11 = LOGICAL UNIT FOR DRR-FILE
C     LCI =  5 = LOGICAL UNIT FOR CMD INPUT - WHEN FROM VDT
C     LIN =  5 = LOGICAL UNIT FOR CMD INPUT - ACTIVE NOW
C     LCM =  3 = LOGICAL UNIT FOR CMD INPUT - WHEN FROM FILE
C            6 = LOGICAL UNIT FOR TERMINAL  OUTPUT
C            7 = LOGICAL UNIT FOR LIST-FILE OUTPUT
C     **************************************************************
C
      CMSSG=' '
      LOGUT=6
      LOGUP=7
      LISFLG='LOF '
      MSGF='    '
C
C
      CALL HELPOPEN(1,NAMHEP,IHEPF)             !OPEN HELP-FILE
C
c     OPEN(UNIT      = LOGUP,                       !OPEN/CREATE LOG-FILE
c    &     FILE      = 'asap.log',
c    &     STATUS    = 'UNKNOWN',
c    &     IOSTAT    = IOS)
C
c     CLOSE(UNIT=LOGUP,DISP='DELETE')
C
      OPEN(UNIT      = LOGUP,                       !OPEN/CREATE LOG-FILE
     &     FILE      = 'asap.log',
     &     STATUS    = 'REPLACE',
     &     IOSTAT    = IOS)
C
      CALL CTCNIT
C
      LCI=5
      LIN=5
      LCM=3      
      LUS=10
      LUH=10
      LUD=11
      KFIL='    '
      MXNPK=249
      MXRGL=256
      MXPIS=8
      NTRY=10
      BIAS=3.0
      BSTD=2.0
      KWD=8
      KNBAK=1
      IGAUS=1
      DELCH=0.0
      WDNC=0.05
      EOOO=0.0
      GAINN=1.0
      FWHMA=4.0
      FWHMB=0.0
      KVW='LOCK'
C
C
C     **************************************************************
C     CALCULATE SOME EXPODENTIALS
C     **************************************************************
C
      DO 10 I=1,200
      X=I-1
      X=X/25.0
      GAU(I)=EXP(-X)
   10 CONTINUE
C
      RETURN
      END
