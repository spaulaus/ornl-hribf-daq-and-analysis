C$PROG CMPPLO    - Command processor for graphic operations
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE CMPPLO(IDONE,IERR)
C   
      IMPLICIT INTEGER*4 (A-Z)
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
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if (PTRSIZE == 64)
      INTEGER*8    DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
#endif
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL
      CHARACTER*4            IBANNER
C     ------------------------------------------------------------------
      COMMON/XLKK/ KURTY,MBKEY(3,5)
C     ------------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO                                       !/DML3
      INTEGER*4    IDL,NCHL,KOLR,NNID                        !/DML3
      CHARACTER*4             KFL,                  KDST     !/DML3
C     ------------------------------------------------------------------
      COMMON/DML7/ NUD(20),NUB
C     ------------------------------------------------------------------
      COMMON/PL01/ ILOX(20),IHIX(20),ILOF(20),IHIF(20),      !/PL01
     &             FLOX(20),FHIX(20),FLOF(20),FHIF(20),      !/PL01
     &             GWID,MOPL,ECAL(3)                         !/PL01
C
      COMMON/PL08/ BIAS,IFWHM,IFINDF,IPLOGF
C
      COMMON/PL09/ XL(64,21),YL(64,21),NXYL(21),IBL(21),
     &             IBWN(21),LNBAN(4,21),MAXXYL
C
      COMMON/PL12/ KZSCAL,NZLEV
C
C     ------------------------------------------------------------------
      COMMON/PL13/ MARF(4,20),MARF2(2,20)
      CHARACTER*4  MARF,      MARF2
C
      DATA         MARF,      MARF2/80*'    ',40*'    '/
C     ------------------------------------------------------------------
C
      COMMON/PL14/ MSDLA1
C
      COMMON/PL15/ KRUD
C     ------------------------------------------------------------------
      REAL*4       FLOX,FHIX,FLOF,FHIF,GWID,ECAL
C
      REAL*4       BIAS
C   
      INTEGER*4    LWD3(3,25)
C
      REAL*4       XV
C
      CHARACTER*4  KMD,KZSCAL,IPLOGF,IFLG,MOPL,IFINDF,IDONE
C   
      EQUIVALENCE (KMD,LWD),(LWD3,LWD)
C   
      character*4 csetw
      equivalence(csetw, setw)
      DATA cSETW/'SETW'/
C
      DATA BIAS,IFWHM,IFINDF/5.0,5,'OFF '/
C   
      DATA KDST/20*'    '/
C   
      DATA ILOX,IHIX,ILOF,IHIF/80*-1/
      DATA FLOX,FHIX,FLOF,FHIF/80*-1.0/
      DATA GWID,ECAL/10.0,0.0,1.0,0.0/

      character*4 cnud(20), cnub, cmopl
      equivalence (cnud, nud), (cnub,nub), (cmopl, mopl)
      DATA cNUD,cNUB,cMOPL/20*'NO  ','NO  ','OV  '/
      DATA IDW/1/

      character*4 cmbkey(15)
      equivalence (cmbkey, mbkey)
      DATA cMBKEY/15*'    '/
      DATA KZSCAL,NZLEV/'ZSON',10/
C
      SAVE
C
C     ==================================================================
C   
      IERR=0
C
      IF(KMD.EQ.'GRAS') GO TO 110
      IF(KMD.EQ.'ZLEV') GO TO 120
      IF(KMD.EQ.'ZSON') GO TO 130
      IF(KMD.EQ.'ZSOF') GO TO 130
      IF(KMD.EQ.'PLON') GO TO 135
      IF(KMD.EQ.'PLOF') GO TO 135   
      IF(KMD.EQ.'AXON') GO TO 140
      IF(KMD.EQ.'AXOF') GO TO 140
      IF(KMD.EQ.'WIN ') GO TO 150
      IF(KMD.EQ.'SSI ') GO TO 160
      IF(KMD.EQ.'BNON') GO TO 170
      IF(KMD.EQ.'BNOF') GO TO 175
C
      IF(KMD.EQ.'CURT') GO TO 180
      IF(KMD.EQ.'BUD ') GO TO 181
      IF(KMD.EQ.'BUDD') GO TO 182
      IF(KMD.EQ.'BUDS') GO TO 183
C
      IF(KMD.EQ.'CAL ') GO TO 210
      IF(KMD.EQ.'KRUD') GO TO 240
      IF(KMD.EQ.'GWID') GO TO 250
C   
      IF(KMD.EQ.'OV  ') GO TO 260
      IF(KMD.EQ.'ST  ') GO TO 260
      IF(KMD.EQ.'NOFI') GO TO 270
      IF(KMD.EQ.'FIND') GO TO 280
C
      IF(KMD.EQ.'DLAD') GO TO 290
C
      IF(KMD.EQ.'SL  ') GO TO 520
      IF(KMD.EQ.'SUML') GO TO 520
      IF(KMD.EQ.'SUM ') GO TO 520
C   
      IF(KMD.EQ.'LIN ') GO TO 580
      IF(KMD.EQ.'LOG ') GO TO 580
      IF(KMD.EQ.'DMM ') GO TO 580
      IF(KMD.EQ.'DNOR') GO TO 580
      IF(KMD.EQ.'DL  ') GO TO 580
      IF(KMD.EQ.'DX  ') GO TO 580
      IF(KMD.EQ.'D   ') GO TO 580
      IF(KMD.EQ.'COL ') GO TO 580
C   
      IF(KMD.EQ.'XC  ') GO TO 600
      IF(KMD.EQ.'YC  ') GO TO 600
      IF(KMD.EQ.'CRNX') GO TO 600
      IF(KMD.EQ.'CRNY') GO TO 600
      IF(KMD.EQ.'ZMM ') GO TO 600
      IF(KMD.EQ.'ZLOG') GO TO 600
      IF(KMD.EQ.'ZLIN') GO TO 600
      IF(KMD.EQ.'ZINT') GO TO 600
      IF(KMD.EQ.'DD  ') GO TO 600
      IF(KMD.EQ.'DDX ') GO TO 600
C   
      IF(KMD.EQ.'ZBL ') GO TO 610
      IF(KMD.EQ.'LBL ') GO TO 620
C   
      IF(KMD.EQ.'DDID') GO TO 730
C   
      RETURN
C
  110 IT=NF-1
      IF(IT.LT.2)  GO TO 1090
      IF(IT.GT.10) GO TO 1090
      NZLEV=IT
      CALL COLRSET(LDUM,IWD,IERR)
      GO TO 2500
C
  120 CALL MILV(LWD(1,2),IT,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1090
      IF(IT.GT.10)  GO TO 1090
      IF(IT.LT.0)   GO TO 1090
      NZLEV=IT
      GO TO 2500
C
  130 KZSCAL=KMD
      GO TO 2500
C
  135 IPLOGF=KMD
      GO TO 2500
C   
  140 DO 145 I=2,NF
      CALL MILV(LWD(1,I),IT,XV,KIND,IERR)
      IF(IT.LT.1)  IT=1
      IF(IT.GT.20) IT=20
      IFLG='ON  '
      IF(KMD.EQ.'AXOF') IFLG='OFF '
      WINFLC(2,IT)=IFLG
  145 CONTINUE
      GO TO 2500
C
  150 CALL MILV(LWD(1,2),IT,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1090
      IF(IT.GT.20)  GO TO 1090
      IDW=IT
      GO TO 2500
C
  160 DO 165 I=1,20
      IF(WINFLG(1,I).EQ.0) GO TO 165
      CALL XX_WINMAN('ERAS',I)
      WINFLG(1,I)=0
  165 CONTINUE
      NCALL=0
      IDW=1
      GO TO 2500
C
  170 IBANNER='ON  '
      GO TO 2500
  175 IBANNER='OFF '
      GO TO 2500
C
  180 KURTY=LWD(1,2)
      GO TO 2500
C
  181 MBKEY(1,1)=LWD(1,2)
      MBKEY(2,1)=LWD(1,3)
      MBKEY(3,1)=LWD(1,4)
      GO TO 2500
  182 MBKEY(1,2)=LWD(1,2)
      MBKEY(2,2)=LWD(1,3)
      MBKEY(3,2)=LWD(1,4)
      GO TO 2500
  183 MBKEY(1,3)=LWD(1,2)
      MBKEY(2,3)=LWD(1,3)
      MBKEY(3,3)=LWD(1,4)
      GO TO 2500
C
  210 IF(NF.EQ.1) THEN        !ENTER CAL CONSTANTS
                  ECAL(1)=0.0
                  ECAL(2)=1.0
                  ECAL(3)=0.0
                  GO TO 2500
                  ENDIF
C
      IF(NF.LT.3) GO TO 1090 
C   
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER)
C   
      CALL GREAD(IWD,LWD3,ITYP,NF,1,80,NTER)
C
      IF(NTER.NE.0) GO TO 1090
C   
      DO 212 I=1,3
      CALL MILV3(LWD3(1,I+1),IT,ECAL(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 214
  212 CONTINUE
  214 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
      IF(IERR.NE.0) GO TO 1090
      GO TO 2500
C
  240 CALL LIMIV(LWD(1,2),0,128,KRUD,IERR)
      IF(IERR.NE.0) GO TO 1090
      GO TO 2500
C   
  250 CALL LIMXV(LWD(1,2),1.0,1000.0,GWID,IERR)
      IF(IERR.NE.0) GO TO 1090
      GO TO 2500
C   
  260 MOPL=KMD
      GO TO 2500
C
  270 IFINDF='OFF '
      GO TO 2500
C
  280 IFINDF='ON  '
      BIAS=5.0
      IFWHM=5
      IF(NF.LT.2) GO TO 2500
      CALL MILV(LWD(1,2),IT,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1090
      BIAS=XV
      IF(NF.LT.3) GO TO 2500
      CALL MILV(LWD(1,3),IT,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1090
      IFWHM=IT
      GO TO 2500
C
  290 CALL MILV(LWD(1,2),IT,XV,KIND,IERR)
      IF(IT.LE.0)     IT=500
      IF(IT.LT.10)    IT=10
      IF(IT.GT.20000) IT=20000
      MSDLA1=IT
      GO TO 2500
C
  520 CALL SUMMER1            !PROCESS SUM REQUESTS
      GO TO 2500
C   
  580 CALL DISPHAN1(IDW)      !PROCESS 1-D DISPLAY & SETUP COMMANDS
C   
      IF(KMD.EQ.'D   ') KDST(IDW)='1D  '
      IF(KMD.EQ.'DX  ') KDST(IDW)='1D  '
      GO TO 2500
C   
  600 CALL DISPHAN2(IDW)      !PROCESS 2-D DISPLAY & SETUP COMMANDS
C   
      IF(KMD.EQ.'DD  ') KDST(IDW)='2D  '
      GO TO 2500
C   
  610 DO 615 I=1,MAXXYL       !ZOT BAN-LIB
      IBL(I)=-1
  615 CONTINUE
      GO TO 2500
C   
  620 DO 630 I=1,MAXXYL       !SHOW BAN-LIB
      IF(IBL(I).LE.0)GOTO 630
      WRITE(LOGUT,625)IBL(I)
  625 FORMAT(1H ,'BID=',I6)
  630 CONTINUE
      GO TO 2500
C   
  730 IF(KDST(IDW).EQ.'2D  ') CALL DID2(IDW)
      GO TO 2500
C
 1090 WRITE(CMSSG,1095)
 1095 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - CMD IGNORED')
C   
 2000 CALL MESSLOG(LOGUT,LOGUP)
C   
 2500 IDONE='YES '
      RETURN
      END
