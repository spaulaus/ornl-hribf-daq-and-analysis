C$PROG BOPEN     - Creates & opens BAN-files
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BOPEN(LUB,IERR)
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
      COMMON/DML5/ TITI(20),FILI(6),KPARI(9),IHI,DGI,        !/DML5
     &             TITO(20),FILO(6),KPARO(9),IHO,DGO         !/DML5
      INTEGER*4    TITI,    FILI,   KPARI,   IHI,DGI
      INTEGER*4    TITO,    FILO,   KPARO,   IHO,DGO
C     ------------------------------------------------------------------
      INTEGER*4    NAM(20),MSG(7),IDEV(2),EXT
      character*4  cidev(2),cext
      equivalence  (cidev,idev),(cext,ext)
C
      INTEGER*4    RECLVALU
C
      CHARACTER*4  NU
C   
      CHARACTER*80 FNAME
      EQUIVALENCE (FNAME,NAM(1))
C   
      DATA cEXT,cIDEV/'.ban',2*'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO CREATE & OPEN BAN-FILES
C     ------------------------------------------------------------------
C   
      IERR=0                                !ERROR FLAG
C   
      CALL BILNAM(IWDRAW,IDEV,EXT,NAM,IX,LD,NU,IERR) !GET FNAME, EXT
C
      IF(IERR.NE.0)    GO TO 200            !TST FOR ERROR
C   
      DO 5 I=1,20
      NAMFIL(I,LUB)=NAM(I)
    5 CONTINUE
C   
      CLOSE(UNIT=LUB)                       !CLOSE BAN-FILE
      KFIL(LUB)='    '                      !SET BAN-FILE CLOSED
C   
      IF(NU.EQ.'NEW ') GO TO 50             !TST FOR CREATE REQ
      IF(NU.EQ.'new ') GO TO 50
                       GO TO 100
C   
   50 OPEN(UNIT       = LUB,                !CREATE NEW BAN-FILE
     &     FILE       = FNAME,
     &     STATUS     = 'NEW',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(80),
     &     IOSTAT     = IOS)
C   
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 200
C   
      CALL BANIO(0,LUB,FILI,TITI,IHI,IBN,DGI,IX,JY,NP,NID,KPARI,
     &             MSG,IERR)
C   
      IF(IERR.EQ.0) GO TO 110
C   
      WRITE(CMSSG,60)MSG
   60 FORMAT(7A4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C   
  100 OPEN(UNIT       = LUB,                !OPEN EXISTING BAN-FILE
     &     FILE       = FNAME,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(80),
     &     IOSTAT     = IOS)
C   
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 200
C   
  110 KFIL(LUB)='BAN '                      !SET BAN-FILE OPEN
      RETURN
C   
  200 IERR=1
      RETURN
      END
