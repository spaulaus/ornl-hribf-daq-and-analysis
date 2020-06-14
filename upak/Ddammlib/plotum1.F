C$PROG PLOTUM1   - Display routine for 1-D
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE PLOTUM1(IDW,IFI,NCH,KDSP)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     -----------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO                                       !/DML3
      INTEGER*4    IDL,NCHL,KOLR,NNID                        !/DML3
      CHARACTER*4             KFL,                  KDST     !/DML3
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if (PTRSIZE == 64)
      INTEGER*8   DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4   DPY,WDID                                 !Alpha STAR8
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
      character*4  cititl(20)
      equivalence(cititl,ititl)
      CHARACTER*4            IBANNER
C     ------------------------------------------------------------------
      COMMON/PL01/ ILOX(20),IHIX(20),ILOF(20),IHIF(20),      !/PL01
     &             FLOX(20),FHIX(20),FLOF(20),FHIF(20),      !/PL01
     &             GWID,MOPL,ECAL(3)                         !/PL01
C     ------------------------------------------------------------------
      COMMON/PL02/ MDYHMS(5),KINDD,KINFD,MINDY,MAXDY
      INTEGER*4    MDYHMS                MINDY,MAXDY
      CHARACTER*4            KINDD,KINDF
      CHARACTER*4  MINDYC,MAXDYC
      EQUIVALENCE (MINDYC,MINDY),(MAXDYC,MAXDY)
C     ------------------------------------------------------------------
      COMMON/PL14/ MSDLA1
C
      COMMON/PL15/ KRUD
C
      COMMON/PL16/ LENHIS(4,20)
C
      COMMON/PL17/ LENSPK
C     ------------------------------------------------------------------
      CHARACTER*4  MOPL,KDSP
C   
      INTEGER*4    HIST,CVEC,READ
      character*4  cHIST,cCVEC,cREAD
      equivalence  (cHIST,hist),(cCVEC,cvec),(cREAD,read)
C
      REAL*4       YLO(33),YHI(33),FDAT(16384)
C
      REAL*8       SUMI(33)
C
      INTEGER*4    ANUM(3)
C
      DATA cITITL/10*'* * ',10*'    '/
C
      DATA MSDLA1/10/
C
      EQUIVALENCE (FDAT,IDATF(16385))
C   
      DATA cHIST,cCVEC,cREAD/'HIST','CVEC','READ'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     DAT  - CONTAINS DATA, CAL, BGD TO BE DISPLAYED
C     IDN  = ID # FOR DISPLAY LABEL
C     IFI  - GIVES INDEX OF SPECTRUM ELEMENT CONTAINED IN IDAT(1)
C     NCH  = # OF CHANNELS TO DISPLAY
C     KDSP = 'LIN ' SAYS DO LINEAR   DISPLAY
C     KDSP = 'LOG ' SAYS DO SEMI-LOG DISPLAY
C     ------------------------------------------------------------------
C
      WINFLC(3,IDW)='1D  '
      WINFLC(4,IDW)=KDSP
C
      ILO=IFI
      IHI=IFI+NCH-1
C
      NID=NNID(IDW)
C   
C     ------------------------------------------------------------------
C     COMPUTE MIN & MAX VALUES OF X & Y FOR ALL ID'S
C     ------------------------------------------------------------------
C   
      NCHMAX=0
      DO 20 N=1,NID
      YLO(N)=1.0E20
      YHI(N)=1.0
C   
      CALL SPKIN1(READ,KFL(N,IDW),IDL(N,IDW),CNO(N,IDW),
     &            ILO,IHI,NCHL(N,IDW),IERR)
C
      IF(N.EQ.1) CALL TITLO(ITITL,KFL(1,IDW),IDL(1,IDW))
C
      LENHIS(1,IDW)=LENSPK
C
      IF(NCHL(N,IDW).GT.NCHMAX) NCHMAX=NCHL(N,IDW)
      NDO=NCH
      IF(NCHL(N,IDW).LT.IHI) NDO=NCH-(IHI-NCHL(N,IDW))
C   
      DO 10 I=1,NDO
      DAT=IDATF(I)
      IF(DAT.GT.YHI(N)) YHI(N)=DAT
      IF(DAT.LT.YLO(N)) YLO(N)=DAT
   10 CONTINUE
   20 CONTINUE
C   
      DO 30 I=1,NID
      IF(MINDYC.NE.'VAR ') YLO(I)=MINDY
      IF(MAXDYC.NE.'VAR ') YHI(I)=MAXDY
   30 CONTINUE
C   
C     ------------------------------------------------------------------
C     REQUIRE THAT LIMITS BE SOMETHING REASONABLE
C     ------------------------------------------------------------------
C   
      DO 60 I=1,NID
      IF(KDSP.EQ.'LOG ') GO TO 50
      IF(YHI(I).LE.YLO(I)) YHI(I)=YLO(I)+1.0
      GO TO 60
   50 IF(YLO(I).LT.1.0) YLO(I)=0.8
      IF(YHI(I).LE.YLO(I)) YHI(I)=YLO(I)+1.0
   60 CONTINUE
C   
      YMIN=1.0E12
      YMAX=1.0
C   
      DO 70 I=1,NID
      IF(YLO(I).LT.YMIN) YMIN=YLO(I)
      IF(YHI(I).GT.YMAX) YMAX=YHI(I)
   70 CONTINUE
C   
      NCHI=NCH
      IF(NCHMAX.LT.IHI) NCHI=NCH-(IHI-NCHMAX)
C   
C     ------------------------------------------------------------------
C     PLOT DATA
C     ------------------------------------------------------------------
C   
      ILOC=IFI-1
      IHIC=ILOC+NCHI-1
      XLO=FLOAT(IFI-1)
      XHI=XLO+FLOAT(NCHI)-1.0
C   
      IF(MOPL.EQ.'ST  ') GO TO 200
C   
C     ------------------------------------------------------------------
C     DO OVERLAY MODE
C     ------------------------------------------------------------------
C   
      IF(WINFLG(1,IDW).EQ.0) RETURN
C
      CALL WAIT(MSDLA1,1,ISTAT)  !CALLING XX_SYNC(DPY,.TRUE.)
C                                !DOES NOT HELP
C
Crlv      CALL XX_WINMAN('ERAS',IDW)
C
      WINFLG(1,IDW)=0
      WINDAT(5,IDW)=XLO-0.5
      WINDAT(6,IDW)=YMIN
      WINDAT(7,IDW)=XHI+0.5
      WINDAT(8,IDW)=YMAX
C
      CALL XX_WINMAN('WIN ',IDW)
      IF(IERR.NE.0) RETURN
C   
      DO 150 N=1,NID
C   
      CALL SPKIN1(READ,KFL(N,IDW),IDL(N,IDW),CNO(N,IDW),
     &            ILO,IHI,NCHL(N,IDW),IERR)
C   
      SUMI(N)=0.0D0
      DO 120 I=1,NCHI
      SUMI(N)=SUMI(N)+IDATF(I)
      FDAT(I)=IDATF(I)
  120 CONTINUE
C   
      CALL PLOTYY(IDW,HIST,KOLR(N,IDW),KRUD,XLO,1.0,FDAT,NCHI)
C
      CALL PEAKMAN(IDW,IFI,NCH,N)
C   
  150 CONTINUE
      CALL DMAR1(IDW)
C
      CALL GETDATI(MDYHMS)
C
      N=NID+1
      DO 180 K=1,NID
      N=N-1
      CALL LUGET(KFL(N,IDW),IDUM,J,JDUM,IERR)
      J=J-1
C
      CALL DFASCII(SUMI(N),ANUM,10)
C
      IF(IERR.EQ.0) THEN
      WRITE(CMSSG,155)IDL(N,IDW),ILOC,IHIC,CNO(N,IDW),ANUM,
     &                    (NAMFIL(I,J),I=1,5),(MDYHMS(I),I=1,4)
                    ENDIF
C
      IF(IERR.NE.0) THEN
      WRITE(CMSSG,160)IDL(N,IDW),ILOC,IHIC,CNO(N,IDW),ANUM,
     &                    (MDYHMS(I),I=1,4)
                    ENDIF
C
  155 FORMAT(I8,' S(',I5,'-',I5,')*',1PE10.3,'=',1X,2A4,A2,' ',
     &       4A4,A1,3A4,A3)
  160 FORMAT(I8,' S(',I5,'-',I5,')*',1PE10.3,'=',1X,2A4,A2,' ',
     &       17X,   3A4,A3)
      CALL MESSLOG(LOGUT,LOGUP)
  180 CONTINUE
      RETURN
C   
C     ------------------------------------------------------------------
C     DO STACKED MODE
C     ------------------------------------------------------------------
C   
  200 JDW=IDW-1
      NDID=0
      NJDW=0
C   
      DO 250 N=1,NID
      JDW=JDW+1
      IF(JDW.GT.NUMWIN) GO TO 260
      NDID=NDID+1
      NJDW=JDW
      KDST(JDW)='1D  '
      NNID(JDW)=1
      WINFLC(3,JDW)='1D  '
      WINFLC(4,JDW)=KDSP
C
      IF(WINFLG(1,JDW).EQ.0) RETURN
C
      CALL WAIT(MSDLA1,1,ISTAT)  !CALLING XX_SYNC(DPY,.TRUE.)
C                                !DOES NOT HELP
C rlv      CALL XX_WINMAN('ERAS',JDW)
      WINFLG(1,JDW)=0
      WINDAT(5,JDW)=XLO-0.5
      WINDAT(6,JDW)=YLO(N)
      WINDAT(7,JDW)=XHI+0.5
      WINDAT(8,JDW)=YHI(N)
C
      KFL(1,JDW)=KFL(N,IDW)
      IDL(1,JDW)=IDL(N,IDW)
      CNO(1,JDW)=CNO(N,IDW)
      KOLR(1,JDW)=KOLR(1,IDW)
      NCHL(1,JDW)=NCHL(N,IDW)
C
      CALL SPKIN1(READ,KFL(1,JDW),IDL(1,JDW),CNO(1,JDW),
     &            ILO,IHI,NCHL(1,JDW),IERR)
C
      CALL TITLO(ITITL,KFL(1,JDW),IDL(1,JDW))
C
      CALL XX_WINMAN('WIN ',JDW)
      IF(IERR.NE.0) RETURN
C   
      SUMI(N)=0.0D0
      DO 240 I=1,NCHI
      SUMI(N)=SUMI(N)+IDATF(I)
      FDAT(I)=IDATF(I)
  240 CONTINUE
C   
      CALL PLOTYY(JDW,HIST,KOLR(1,JDW),KRUD,XLO,1.0,FDAT,NCHI)
C
      CALL PEAKMAN(JDW,IFI,NCH,1)
C   
      CALL DMAR1(JDW)
C
  250 CONTINUE
C   
  260 CALL GETDATI(MDYHMS)
C
      N=NDID+1
      JDW=NJDW+1
      DO 320 K=1,NDID
      JDW=JDW-1
      N=N-1
      CALL LUGET(KFL(1,JDW),IDUM,J,JDUM,IERR)
      J=J-1
C
      CALL DFASCII(SUMI(N),ANUM,10)
C
      IF(IERR.EQ.0) THEN
      WRITE(CMSSG,155)IDL(1,JDW),ILOC,IHIC,CNO(1,JDW),ANUM,
     &                    (NAMFIL(I,J),I=1,5),(MDYHMS(I),I=1,4)
                    ENDIF
C
      IF(IERR.NE.0) THEN
      WRITE(CMSSG,160)IDL(1,JDW),ILOC,IHIC,CNO(1,JDW),ANUM,
     &                    (MDYHMS(I),I=1,4)
                    ENDIF
C
      CALL MESSLOG(LOGUT,LOGUP)
  320 CONTINUE
C   
      RETURN
      END






