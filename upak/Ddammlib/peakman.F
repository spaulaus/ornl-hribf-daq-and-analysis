C$PROG PEAKMAN   - Executes peak-finds, displays, etc
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE PEAKMAN(IDW,IFI,NCH,IDX)
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
C     ------------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO                                       !/DML3
      INTEGER*4    IDL,NCHL,KOLR,NNID                        !/DML3
      CHARACTER*4             KFL,                  KDST     !/DML3
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
      COMMON/XLBB/ AA(2,20),BB(2,20),PLOTYP(20)
      REAL*4       AA,      BB
      CHARACTER*4                    PLOTYP
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/PL01/ ILOX(20),IHIX(20),ILOF(20),IHIF(20),      !/PL01
     &             FLOX(20),FHIX(20),FLOF(20),FHIF(20),      !/PL01
     &             GWID,MOPL,ECAL(3)                         !/PL01
C     ------------------------------------------------------------------
      COMMON/PL08/ BIAS,IFWHM,IFINDF,IPLOGF
      CHARACTER*4             IFINDF,IPLOGF
C     ------------------------------------------------------------------
      REAL*4       CHANX(200),PSIZE(200)
C
      CHARACTER*4  LISFSAV
C
      INTEGER*4    IENE(2)
C
      CHARACTER*8  CENE
C
      EQUIVALENCE (CENE,IENE)
C
      DATA MAXPK/199/
C
      SAVE
C
C     ==================================================================
C
      IF(IFINDF.NE.'ON  ') RETURN
C
      CALL PFIND(IDATF,CHANX,PSIZE,1,NCH,IFWHM,BIAS,MAXPK,NPK)
C
      IF(NPK.LE.0) RETURN
C
      LISFSAV=LISFLG
      JDX=1
      IF(IPLOGF.EQ.'PLON') THEN
      LISFLG='LON '
      CALL LUGET(KFL(IDX,IDW),IDUM,JDX,JDUM,IERR)
      JDX=JDX-1
                           ENDIF
C
      DO 100 N=1,NPK
C
      X=IFI+CHANX(N)-2.0
C
      IF(IPLOGF.EQ.'PLON') THEN
      WRITE(CMSSG,20)IDL(IDX,IDW),X,PSIZE(N),
     &(NAMFIL(II,JDX),II=1,16)
   20 FORMAT('PEAK',I10,F10.2,F10.0,2X,16A4)
      CALL MESSLOG(0,LOGUP)
                           ENDIF
C
      NDX=CHANX(N)+0.5
      Y=IDATF(NDX)
      IF(Y.LT.1.0) Y=1.0
      KENE=ECAL(1)+ECAL(2)*X+ECAL(3)*X*X
      WRITE(CENE,50)KENE
   50 FORMAT(I8)
      CALL SQUEZL(IENE,1,8)
C
      IX=AA(1,IDW)+BB(1,IDW)*X
C
      IF(PLOTYP(IDW).NE.'LOG ') THEN
      JY=AA(2,IDW)+BB(2,IDW)*Y
                                ENDIF
C
      IF(PLOTYP(IDW).EQ.'LOG ') THEN
      JY=AA(2,IDW)+BB(2,IDW)*ALOG(Y)
                                ENDIF
C
      IF(JY.LT.16) JY=16
C
      CALL MARKITS(IDW,'OGRE',X,JY+10,JY-6)
C
      CALL TEXOUT(IDW,CENE,'OGRE',IX,JY-8)
C
  100 CONTINUE
C
      LISFLG=LISFSAV
      RETURN
      END
