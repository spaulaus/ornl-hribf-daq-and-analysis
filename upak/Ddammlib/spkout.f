C$PROG SPKOUT    - Outputs data to spk-files
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SPKOUT(JB,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
C     ------------------------------------------------------------------
      DIMENSION ADAT(16384),BDAT(16384)
      INTEGER*4 IDATF(16384),JDATF(16384),IHEDF(128),JHEDF(128)
      INTEGER*4 DATIME(6),NDX(4)
      INTEGER*2 IDATH(16384),JDATH(16384),IHEDH(256),JHEDH(256)
C   
      EQUIVALENCE (IHEDF(1),XBUF(1)),(IHEDH(1),XBUF(1))
      EQUIVALENCE (IDATF(1),XBUF(129)),(ADAT(1),XBUF(129))
      EQUIVALENCE (IDATH(1),XBUF(8321))
      EQUIVALENCE (JHEDF(1),XBUF(16513)),(JHEDH(1),XBUF(16513))
      EQUIVALENCE (JDATF(1),XBUF(16641)),(BDAT(1),XBUF(16641))
      EQUIVALENCE (JDATH(1),XBUF(24833))
      EQUIVALENCE (IDI,IHEDF(1)),(IDJ,JHEDF(1))
      EQUIVALENCE (NCHI,IHEDF(12)),(NCHJ,JHEDF(12))
C   
      DATA LOU/10/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO OUTPUT SPECTRA TO SPK-FILES
C     ------------------------------------------------------------------
C   
      IF(KFIL(10).EQ.'SPK ') GO TO 5
C   
      WRITE(CMSSG,2)
    2 FORMAT('OUTPUT SPK-FILE NOT ASSIGNED')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C   
    5 CALL PUSPUL(IPUS,JB)
C
      CALL MILYMDHMS(DATIME)
C
      GO TO (10,50),JB
   10 IHEDF(10)=2*NCHI
      DO 12 I=1,6
      IHEDH(I+8)=DATIME(I)
   12 CONTINUE
      IHEDF(8)=4
      IHEDF(9)=64
      IHEDF(11)=1
      IF(IHEDF(14).GT.IHEDF(12)) IHEDF(12)=IHEDF(14)
      NCH=NCHI
      LH=IHEDF(9)/2
      IF(NUID.NE.0) IDI=NUID
      DO 20 I=1,NCH
      ADD=0.5
      IF(ADAT(I).LT.0.0) ADD=-0.5
      IDATF(I)=ADAT(I)+ADD
   20 CONTINUE
      GO TO 80
   50 JHEDF(10)=2*NCHJ
      DO 52 I=1,6
      JHEDH(I+8)=DATIME(I)
   52 CONTINUE
      JHEDF(8)=4
      JHEDF(9)=64
      JHEDF(11)=1
      IF(JHEDF(14).GT.JHEDF(12)) JHEDF(12)=JHEDF(14)
      NCH=NCHJ
      LH=JHEDF(9)/2
      IF(NUID.NE.0) IDJ=NUID
      DO 60 I=1,NCH
      ADD=0.5
      IF(BDAT(I).LT.0.0) ADD=-0.5
      JDATF(I)=BDAT(I)+ADD
   60 CONTINUE
   80 NBYH=4*LH
      NBYD=4*NCH
      NBYHA=NBYH-128
      NDXH=IOFF(1,JB)+1
      NDXD=IOFF(2,JB)+1
      NDXHA=NDXH+32
C   
C     ------------------------------------------------------------------
C     OUTPUT TO THE MIGHTY SPK-FILE
C     ------------------------------------------------------------------
C   
      CALL SPKIO(2,LOU,ID,XBUF(NDXH),256,XBUF(NDXD),NDX,NCH,IERR)
      CALL SPKERR(IERR)
      IF(IERR.NE.0) GO TO 450
C   
C     ------------------------------------------------------------------
C     BUMP THE ID SEQUENCE NUMBER
C     ------------------------------------------------------------------
C   
      IF(NUID.GT.0) NUID=NUID+1
      IDN=IDI
      IF(JB.EQ.2) IDN=IDJ
      WRITE(CMSSG,410)IDN
      CALL MESSLOG(6,7)
  410 FORMAT('ID # STORED =',I8)
      GO TO 500
C
  450 WRITE(CMSSG,455)
  455 FORMAT('TRYING TO OUTPUT SPECTRUM')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
C   
  500 CALL PUSPUL(IPUL,JB)
      RETURN
      END
