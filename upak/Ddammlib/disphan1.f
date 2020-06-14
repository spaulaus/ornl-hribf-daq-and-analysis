C$PROG DISPHAN1  - Processes display related requests for 1-D display
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE DISPHAN1(IDW)
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
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     ------------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO                                       !/DML3
      INTEGER*4    IDL,NCHL,KOLR,NNID                        !/DML3
      CHARACTER*4             KFL,                  KDST     !/DML3
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
      COMMON/PL02/ MDYHMS(5),KINDD,KINFD,MINDY,MAXDY
      INTEGER*4    MDYHMS                MINDY,MAXDY
      CHARACTER*4            KINDD,KINDF
      CHARACTER*4  MINDYC,MAXDYC
      EQUIVALENCE (MINDYC,MINDY),(MAXDYC,MAXDY)
C     ------------------------------------------------------------------
C   
      CHARACTER*4  KMD,DDONE,KDSP,NORF
C
      INTEGER*4    KSOT(7),IV(33),TEST,READ
      character*4  cksot(7),cread,ctest
      equivalence (cksot, ksot),(cread,read),(ctest,test)
C      
      INTEGER*4    LWDS(2,40),KOLS(33),N33
      CHARACTER*4  cKOLS(33)
      equivalence  (cKOLS,KOLS)
C
      REAL*8       SUMNOR(33),SUMMAX,SUMI
C   
      EQUIVALENCE (KMD,LWD(1,1))
C   
      DATA         KDSP,DDONE,cTEST,cREAD/'LIN ','NO  ','TEST','READ'/
C
      DATA         IFI,ID,NCH,ILOC,IHIC/1,0,0,-1,-1/
C
      DATA cKOLS/'WHIT','RED ','GREE','BLUE','RDGR','RDBL','GRBL',
     &          'WHIT','WHIT','WHIT','WHIT','WHIT','WHIT','WHIT',
     &          'WHIT','WHIT','WHIT','WHIT','WHIT','WHIT','WHIT',
     &          'WHIT','WHIT','WHIT','WHIT','WHIT','WHIT','WHIT',
     &          'WHIT','WHIT','WHIT','WHIT','WHIT'/
C
      DATA MINDYc,MAXDYc/'VAR ','VAR '/
C
      DATA NORF,NORL,NORH/'NO  ',0,0/
C
      DATA cKSOT/'M   ','N   ','O   ','P   ','Q   ','R   ','S   '/
C
      DATA NCALL/0/
C
      DATA N33/33/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CHARACTER*4  KODLIS(33)
C
      INTEGER*4    IDLIS(33),NUMID,KERR
C
      REAL*4       NORLIS(33)
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
C     ROUTINE TO PROCESS DISPLAY REQUESTS
C     ------------------------------------------------------------------
C   
      IF(NCALL.NE.0) GO TO 50
C
      DO 20 J=1,20
      DO 10 I=1,33
      KOLR(I,J)=KOLS(I)
   10 CONTINUE
   20 CONTINUE
      NCALL=1
C
   50 IF(KMD.EQ.'D   ') GO TO 200
      IF(KMD.EQ.'DX  ') GO TO 200
C   
      NV=NF-1
      IF(NV.GT.N33) NV=N33
      DO 60 I=1,N33
      IV(I)=0
   60 CONTINUE
C   
      IF(KMD.EQ.'DMM ') GO TO 120
C   
      DO 70 I=1,NV
      CALL MILV(LWD(1,I+1),IV(I),XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 510
   70 CONTINUE
C   
      IF(KMD.EQ.'LIN ') GO TO 100
      IF(KMD.EQ.'LOG ') GO TO 100
      IF(KMD.EQ.'DL  ') GO TO 110
      IF(KMD.EQ.'COL ') GO TO 130
      IF(KMD.EQ.'DNOR') GO TO 170
C   
      GO TO 510
C   
  100 KDSP=KMD
      RETURN
C   
  110 ILOC=-1
      IHIC=-1
      IF(NV.NE.2)        RETURN
      IF(IV(1).LT.0)     GO TO 510
      IF(IV(2).LE.IV(1)) GO TO 510
      ILOC=IV(1)
      IHIC=IV(2)
      RETURN
C   
  120 MINDYC='VAR '
      MAXDYC='VAR '
      IF(NV.EQ.0)      RETURN
      IF(NV.NE.2)      GO TO 510
      IF(ITYP(2).EQ.1) GO TO 122
      CALL MILV(LWD(1,2),MINDY,XV,KIND,IERR)
      IF(IERR.NE.0)    GO TO 510
  122 IF(ITYP(3).EQ.1) RETURN
      CALL MILV(LWD(1,3),MAXDY,XV,KIND,IERR)
      IF(IERR.NE.0)    GO TO 510
      RETURN
C   
  130 IF(NV.LE.0) GO TO 140
      DO 135 I=1,NV
      IT=IV(I)
      IF(IT.LT.1)   GO TO 520
      IF(IT.GT.N33) GO TO 520
      KOLR(I,IDW)=KOLS(IT)
  135 CONTINUE
      RETURN
  140 DO 145 I=1,N33
      KOLR(I,IDW)=KOLS(I)
  145 CONTINUE
      RETURN
C   
  170 IF(NV.NE.0) GO TO 180
C   
      NORF='NO  '
      DO 175 I=1,N33
      CNO(I,IDW)=1.0
  175 CONTINUE
      RETURN
C   
  180 IF(NV.NE.2)        GO TO 510
      IF(IV(1).LT.0)     GO TO 510
      IF(IV(2).GE.16384) GO TO 510
      IF(IV(1).GT.IV(2)) GO TO 510
      NORF='YES '
      NORL=IV(1)+1
      NORH=IV(2)+1
      RETURN
C   
  200 IF(WINFLG(1,IDW).EQ.0) GO TO 570
C
      IF(NF.EQ.1) GO TO 205
C
      CALL GETDLIS(IDLIS,KODLIS,NORLIS,NUMID,N33,KERR)
C
      IF(KERR.NE.0) RETURN
C
  205 NID=0
C
      DO 210 I=1,NUMID
      NID=NID+1
      KFL(NID,IDW)=KODLIS(I)
      IDL(NID,IDW)=IDLIS(I)
      CNO(NID,IDW)=NORLIS(I)
  210 CONTINUE
C
  220 IF(NID.LE.0) GO TO 550
C
      NNID(IDW)=NID
C   
  330 MAXCH=0
      NORC=NORH-NORL+1
C   
      DO 350 N=1,NID
      CALL SPKIN1(TEST,KFL(N,IDW),IDL(N,IDW),1.0,
     &            NORL,NORH,NCHL(N,IDW),IERR)
      IF(IERR.NE.0) RETURN
C
      IF(NORH.GT.NCHL(N,IDW)) GO TO 540
C
      IF(NCHL(N,IDW).GT.MAXCH) MAXCH=NCHL(N,IDW)
      IF(NORF.NE.'YES ') GO TO 350
C   
      CALL SPKIN1(READ,KFL(N,IDW),IDL(N,IDW),1.0,
     &            NORL,NORH,NCHL(N,IDW),IERR)
      SUMI=0.0D0
      DO 340 I=1,NORC
      SUMI=SUMI+IDATF(I)
  340 CONTINUE
      SUMNOR(N)=SUMI
  350 CONTINUE
C   
      IF(NORF.NE.'YES ') GO TO 380
C
      SUMMAX=0.0D0   
      MDX=1
      DO 360 I=1,NID
      IF(SUMNOR(I).LE.0)      GO TO 380
      IF(SUMNOR(I).LE.SUMMAX) GO TO 360
      SUMMAX=SUMNOR(I)
      MDX=I
  360 CONTINUE
C   
      DO 370 I=1,NID
      CNO(I,IDW)=1.0
      IF(I.EQ.MDX) GO TO 370
      CNO(I,IDW)=SUMNOR(MDX)/SUMNOR(I)
  370 CONTINUE
C   
  380 IF(KMD.EQ.'D   ')          GO TO 390  !TST FOR NORMAL D-CMD
C   
      IF(ILOX(IDW).LT.0)         GO TO 530  !TST FOR LEGAL EXP-REG
      IF(IHIX(IDW).LE.0)         GO TO 530  !TST FOR LEGAL EXP-REG
      IF(ILOX(IDW).GE.IHIX(IDW)) GO TO 530  !TST FOR LEGAL EXP-REG
C   
      ILO=ILOX(IDW)+1
      IHI=IHIX(IDW)+1
      GO TO 400
C   
  390 ILO=1
      IHI=ILO+MAXCH-1
      IF(ILOC.GE.0) THEN
                    ILO=ILOC+1
                    IHI=IHIC+1
                    ENDIF
C   
  400 IF(IHI.GT.MAXCH)  IHI=MAXCH
      IF(IHI.GT.16384)  IHI=16384
      IF(ILO.GE.IHI)    GO TO 560
      NCH=IHI-ILO+1
      IFI=ILO
C   
C     ------------------------------------------------------------------
C     DO THE MIGHTY PLOT
C     ------------------------------------------------------------------
C   
      CALL PLOTUM1(IDW,IFI,NCH,KDSP)
      DDONE='YES '
      RETURN
C   
C     ------------------------------------------------------------------
C     RETURN ERROR MESSAGES
C     ------------------------------------------------------------------
C   
  510 WRITE(CMSSG,515)
  515 FORMAT('SYNTAX ERROR OR ILLEGAL COMMAND - IGNORED')
      GO TO 600
C   
  520 WRITE(CMSSG,525)
  525 FORMAT('ILLEGAL COLOR DEFINITION - RANGE = 1 TO 7')
      GO TO 600
C   
  530 WRITE(CMSSG,535)
  535 FORMAT('ILLEGAL OR UNSPECIFIED EXPAND REGION')
      GO TO 600
C
  540 WRITE(CMSSG,545)KFL(N,IDW),IDL(N,IDW)
  545 FORMAT('DNOR range exceeds data for ',A1,'-file,  ID =',I8)
      GO TO 600
C   
  550 WRITE(CMSSG,555)
  555 FORMAT('Display request is empty - command ignored')
      GO TO 600
C   
  560 WRITE(CMSSG,565)
  565 FORMAT('DISPLAY RANGE SPECIFIED BY "DL" DOESN,T MATCH DATA')
      GO TO 600
C
  570 WRITE(CMSSG,575)IDW
  575 FORMAT('WINDOW-',I2,'  UNDEFINED - COMMAND IGNORED')
C
  600 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
