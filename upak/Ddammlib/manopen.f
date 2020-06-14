C$PROG MANOPEN   - Manages opening of all file/device types
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE MANOPEN(IDONE,IERR)
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO
      INTEGER*4    IDL,NCHL,KOLR,NNID
      CHARACTER*4             KFL,                  KDST     !/DML3
C     ------------------------------------------------------------------
      COMMON/DML7/ NUD(20),NUB
      CHARACTER*4  NUD,    NUB
C     ------------------------------------------------------------------
C
      CHARACTER*4  IDONE,KMD,KODF
C
      INTEGER*4 NAMCMD(20),NAM(20),IDEV(2)
      character*4 cidev(2)
      equivalence (cidev, idev)
C   
      CHARACTER*80 FNAME
      EQUIVALENCE (FNAME,NAM(1)),(KMD,LWD(1,1))
C   
      DATA cIDEV,KODF/'    ','    ','N   '/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO MANAGE OPENING OF ALL FILE/DEV TYPES
C     ------------------------------------------------------------------
C     LU  - USED FOR .....................
C   
C      1    HELP FILE
C      2    MAG TAPE OUTPUT
C   
C      3    SCRATCH FILE FOR GRAPHICS "INITIA"
C      4    CMD FILE
C      5    VDT INPUT
C      6    VDT OUTPUT
C      7    LOG-FILE OR PRINTER
C   
C      8    INPUT  HIS-FILE OR SPK-FILE
C      9    INPUT  DIR-FILE IF REQUIRED
C   
C     10    OUTPUT HIS-FILE OR SPK-FILE
C     11    OUTPUT DIR-FILE IF REQUIRED
C   
C     12    Q-FILE HIS-FILE OR SPK-FILE
C     13    Q-FILE DIR-FILE IF REQUIRED
C   
C     14    R-FILE HIS-FILE OR SPK-FILE
C     15    R-FILE DIR-FILE IF REQUIRED
C   
C     16    S-FILE HIS-FILE OR SPK-FILE
C     17    S-FILE DIR-FILE IF REQUIRED
C   
C     18    PROJ-FILE
C     19    DON'T TRY TO USE
C     20    BAN- FILE
C     ------------------------------------------------------------------
C   
      IERR=0                                 !RESET ERROR FLAG
      IDONE='    '                           !RESET DONE  FLAG
C   
      IF(KMD.EQ.'DISK') GO TO 50
      IF(KMD.EQ.'CMD ') GO TO 100
      IF(KMD.EQ.'CMDF') GO TO 100
      IF(KMD.EQ.'IN  ') GO TO 110
      IF(KMD.EQ.'OU  ') GO TO 120
      IF(KMD.EQ.'HOU ') GO TO 120
      IF(KMD.EQ.'QF  ') GO TO 130
      IF(KMD.EQ.'RF  ') GO TO 140
      IF(KMD.EQ.'SF  ') GO TO 150
      IF(KMD.EQ.'BAN ') GO TO 160
      IF(KMD.EQ.'BANF') GO TO 160
C   
      RETURN
C   
   50 IDEV(1)=LWD(1,2)
      IDEV(2)=LWD(2,2)
      GO TO 300
C   
  100 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)
      GO TO 300
C   
  110 LU=8
      KODF='N   '
      GO TO 200
  120 LU=10
      KODF='O   '
      GO TO 200
  130 LU=12
      KODF='Q   '
      GO TO 200
  140 LU=14
      KODF='R   '
      GO TO 200
  150 LU=16
      KODF='S   '
      GO TO 200
  160 LU=20
      CALL BOPEN(LU,IERR)
      NUB='YES '
      GO TO 300
C   
  200 CALL FILOPEN(IDEV,LU,IERR)
C   
      DO 220 K=1,20
      NDO=NNID(K)
      DO 210 I=1,NDO
      IF(KODF.EQ.KFL(I,K)) KDST(K)='    '
  210 CONTINUE
  220 CONTINUE
C   
  300 IDONE='YES '
      RETURN
      END
