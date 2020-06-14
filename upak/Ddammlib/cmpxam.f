C$PROG CMPXAM    - Command processor for "exam"  operations
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE CMPXAM(IDONE,IERR)
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
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
C   
      COMMON/DML6/ ISIGNF
C   
      COMMON/XAM1/ NAMFI(20)
C
      CHARACTER*4  KMD,IDONE,KINF
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'DDIR') GO TO 10
      IF(KMD.EQ.'DIRF') GO TO 10
      IF(KMD.EQ.'DSUM') GO TO 10
      RETURN
C
   10 KSOR=LWD(1,2)
      CALL LUGET(KSOR,LUH,LUD,KINF,IERR)
      IF(IERR.NE.0) GO TO 500
C
      LUS=LUH
C
      JFI=LUD-1
      DO 20 I=1,20
      NAMFI(I)=NAMFIL(I,JFI)
   20 CONTINUE
C
      IF(KMD.EQ.'DDIR') GO TO 100
      IF(KMD.EQ.'DIRF') GO TO 110
      IF(KMD.EQ.'DSUM') GO TO 120
      GO TO 500
C
  100 IF(KINF.EQ.'SPK ') CALL SPKDIR(LUS)
      IF(KINF.EQ.'HIS ') CALL HISDIR(LUD)
      GO TO 200
C
  110 IF(KINF.NE.'HIS ') GO TO 500
      CALL LDIR(LUD)
      GO TO 200
C
  120 IF(KINF.EQ.'SPK ') CALL SPKSUM(LUS)
      IF(KINF.EQ.'HIS ') CALL HISSUM(LUH,LUD,ISIGNF)
C
  200 IDONE='YES '
      RETURN
C
  500 WRITE(CMSSG,505)
  505 FORMAT('ILLEGAL COMMAND OR SYNTAX ERROR - IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      IDONE='YES '
      IERR=1
      RETURN
      END
