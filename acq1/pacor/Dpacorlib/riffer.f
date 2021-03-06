C$PROG RIFFER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE RIFFER(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXG=100)
      PARAMETER (MXL=50)
      PARAMETER (MXR=10)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PACC/ RIFLATI(MXR),RIFMASK(MXR),RIFMOTY(2,MXR),NRIF
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      INTEGER*4 NAMLAT(4,MXL),NDXL(MXL),NAML(4),NAMG(4),NLAT
C
      DATA NCALL,NLAT,NRIF,MXLAT,MXRIF/0,0,0,50,10/
C
      integer*4 SETW
      character*4 cSETW
      equivalence (cSETW, SETW)
      DATA cSETW/'SETW'/
C
      CHARACTER*4  KIMO,USED,FOUND,CLWD(3,40)
C
      EQUIVALENCE (CLWD,LWD)
C
      SAVE
C
      IF(NCALL.GT.0) GO TO 100
C
      DO 50 N=1,NUMT
      IF(KIMO(N).NE.'$LAT') GO TO 50
      NLAT=NLAT+1
      IF(NLAT.GT.MXLAT)     GO TO 510
      NAMLAT(1,NLAT)=NAMO(1,N)
      NAMLAT(2,NLAT)=NAMO(2,N)
      NAMLAT(3,NLAT)=NAMO(3,N)
      NAMLAT(4,NLAT)=NAMO(4,N)
   50 CONTINUE
      NCALL=1
C
  100 CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER)
C
      CALL GREAD(IWD,LWD,ITYP,NF,6,80,NTER)
C
      IF(NTER.NE.0) GO TO 520
C
C     ************************************************************
C     DETERMINE IF READ CONDITION IS ON LATCH-WORD OR GATE
C     LWD(1,1)='TRUE' SAYS GATE-TYPE
C     ************************************************************
C
      IF(CLWD(1,1).EQ.'TRUE'.AND.CLWD(2,1).EQ.'    ') GO TO 200
C
      CALL MILV3(LWD(1,2),LDX,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 530
C
      DO 110 I=1,3
      NAML(I)=LWD(I,1)
  110 CONTINUE
      NAML(4)=LDX
C
      CALL NAMLOC(NAMLAT,NAML,NLAT,JXX)
      IF(JXX.LE.0) GO TO 540
C
      CALL HEXVAL(LWD(2,3),MSK,IERR)
      IF(IERR.NE.0) GO TO 550
C
      CALL MODCOD(LWD(1,4),MCODE,IERR)
      IF(IERR.NE.0) GO TO 1000
C
      FOUND='NO  '
      DO 150 I=1,NUMT
      IF(MCODE.NE.MOTY(1,I))  GO TO 150
      IF(USED(I).EQ.'YES ') GO TO 570
      USED(I)='YES '
      FOUND='YES '
  150 CONTINUE
      IF(FOUND.NE.'YES ') GO TO 580
C
      NRIF=NRIF+1
      IF(NRIF.GT.MXRIF)   GO TO 590
      RIFLATI(NRIF)=LDX
      RIFMASK(NRIF)=MSK
      RIFMOTY(1,NRIF)=MCODE
      GO TO 1000
C
C     ************************************************************
C     PROCESS GATE-TYPE CONDITIONAL MODULE-READ
C     ************************************************************
C
  200 CALL MILV3(LWD(1,3),IDX,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 530
      DO 210 I=1,3
      NAMG(I)=LWD(I,2)
  210 CONTINUE
      NAMG(4)=IDX
C
      CALL NAMLOC(GNAM,NAMG,NGAT,NDX)
      IF(NDX.LE.0) GO TO 560
C
      CALL MODCOD(LWD(1,4),MCODE,IERR)
      IF(IERR.NE.0) GO TO 1000
C
      FOUND='NO  '
      DO 250 I=1,NUMT
      IF(MCODE.NE.MOTY(1,I)) GO TO 250
      IF(USED(I).EQ.'YES ')  GO TO 570
      USED(I)='YES '
      FOUND='YES '
  250 CONTINUE
      IF(FOUND.NE.'YES ') GO TO 580
C
      NRIF=NRIF+1
      IF(NRIF.GT.MXRIF)   GO TO 590
      RIFLATI(NRIF)=PATN(NDX)
      RIFMASK(NRIF)=GMSK(NDX)
      RIFMOTY(1,NRIF)=MCODE
      GO TO 1000
C
C     ************************************************************
C     SEND ERROR MESSAGES
C     ************************************************************
C
  510 WRITE(CMSSG,515)NLAT
  515 FORMAT('LATCH-WORD TABLE OVERFLOW FROM RIFFER - NLAT=',I6)
      GO TO 800
  520 WRITE(CMSSG,525)
  525 FORMAT('TRUNCATION ERROR FROM - GREAD')
      GO TO 800
  530 WRITE(CMSSG,535)
  535 FORMAT('SYNTAX ERROR FROM MILV3')
      GO TO 800
  540 WRITE(CMSSG,545)NAML
  545 FORMAT('REFERRENCED LATCH-WORD & INDEX - ',3A4,I6,' NOT FOUND')
      GO TO 800
  550 WRITE(CMSSG,555)LWD(2,3),LWD(3,3)
  555 FORMAT('ERROR DECODING HEX MASK - ',2A4)
      GO TO 800
  560 WRITE(CMSSG,565)NAMG
  565 FORMAT('REFERRENCED GATENAME & INDEX - ',3A4,I6,' NOT FOUND')
      GO TO 800
  570 WRITE(CMSSG,575)LWD(1,4),LWD(2,4),LWD(3,4)
  575 FORMAT('MULTIPLE CONDITIONAL READOUT OF MODULE - ',3A4)
      GO TO 800
  580 WRITE(CMSSG,585)LWD(1,4),LWD(2,4),LWD(3,4)
  585 FORMAT('REQUESTED MODULE TYPE - ',3A4,' NOT FOUND')
      GO TO 800
  590 WRITE(CMSSG,595)NRIF
  595 FORMAT('RIF TABLE OVERFLOW AT - ',I6)
C
  800 CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
      RETURN
      END
