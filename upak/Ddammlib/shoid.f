C$PROG SHOID     - Displays/lists IDs from spk- or his-file
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SHOID
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
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
C     ------------------------------------------------------------------
      PARAMETER (MXNH=6144)
C   
      INTEGER*4 IBUF(16384),IDLST(MXNH),IHED(32),MSER(10),NDX(4)
C
      INTEGER*4 BID(880),MSG(7)
C
      CHARACTER*4  KMD,LISSAV,KINF,KFI
C   
      EQUIVALENCE (IBUF(1), IDATF(1)),
     &            (NID,     IDATF(1)),
     &            (IDLST(1),IDATF(2)),
     &            (IHED(1), IDATF(2050))
      EQUIVALENCE (BID(1),  IDATF(1))
      EQUIVALENCE (KMD,LWD(1,1)),(KFI,LWD(1,2))
C   
      integer*4 init, mdir
      character*4 cinit, cmdir
      equivalence (cinit,init), (cmdir,mdir)
      DATA LUD,LUH,LUS,cINIT,cMDIR/0,0,0,'INIT','DIR '/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO DISPLAY & LIST ID'S FROM SPK- OR HIS-FILE
C     ------------------------------------------------------------------
C   
      LP=0
      IF(KMD.EQ.'LDIR') LP=LOGUP
      LISSAV=LISFLG
      LISFLG='LON '
C   
      IF(KFI.EQ.'BAN ') GO TO 350
C   
      CALL LUGET(KFI,LUH,LUD,KINF,IERR)
      IF(IERR.NE.0) GO TO 500
      LUS=LUH
C   
      IF(KINF.EQ.'SPK ') GO TO 200
      IF(KINF.EQ.'HIS ') GO TO 210
                         GO TO 520
C   
  200 CALL SPKIO(6,LUS,0,IHED,64,IBUF,NDX,256,IERR)
      CALL SPKERR(IERR)
      IF(IERR.NE.0) GO TO 400
      GO TO 300
C   
  210 CALL HISIO(INIT,LUD,LUH,0,NDX,2048,IHED,IBUF,IERR,MSER)
      CALL HISERR(IERR)
      IF(IERR.NE.0) GO TO 400
C   
      CALL HISIO(MDIR,LUD,LUH,0,NDX,2048,IHED,IBUF,IERR,MSER)
      CALL HISERR(IERR)
      IF(IERR.NE.0) GO TO 400
C   
  300 IHI=0
  310 ILO=IHI+1
      IF(ILO.GT.NID) GO TO 400
      IHI=ILO+7
      IF(IHI.GT.NID) IHI=NID
      WRITE(CMSSG,320)(IDLST(I),I=ILO,IHI)
  320 FORMAT(8I9)
      CALL MESSLOG(LOGUT,LP)
      GO TO 310
C   
  350 LUB=20
      IF(KFIL(LUB).NE.'BAN ') GO TO 520
C
      CALL BANIO(5,LUB,JP,JP,JP,JP,JP,BID,JP,JP,NBB,JP,MSG,IERR)
      IF(IERR.NE.0) GO TO 540
C
      WRITE(6,365)(BID(I),I=1,NBB)
  365 FORMAT(1H ,10I7)
C   
  400 LISFLG=LISSAV
      RETURN
C   
  500 WRITE(CMSSG,510)
  510 FORMAT('ILLEGAL DIRECTORY REQUEST')
      GO TO 550
C
  520 WRITE(CMSSG,530)KFI
  530 FORMAT(A3,'- FILE NOT OPEN')
      GO TO 550
C
  540 WRITE(CMSSG,545)MSG
  545 FORMAT('BAN-FILE ',7A4)
C
  550 CALL MESSLOG(LOGUT,0)
      LISFLG=LISSAV
      RETURN
      END
