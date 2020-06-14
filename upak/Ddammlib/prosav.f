C$PROG PROSAV    - Saves projections on file damq8q.spk
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PROSAV(KIND,NC,IERR)
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
      CHARACTER*4  CNAMFIL(20,20)
      EQUIVALENCE (CNAMFIL,NANFIL)
C     ------------------------------------------------------------------
      COMMON/PL03/ IDAT(4096),MINCN(2,20),MAXCN(2,20),MINZ,MAXZ
C     ------------------------------------------------------------------
      INTEGER*4    IHEDF(32),ITMP(6)
C   
      INTEGER*2    IHEDH(64)
C
      INTEGER*4    RECLVALU
C   
      EQUIVALENCE (IHEDH,IHEDF)
C   
      CHARACTER*10 PROJF
      DATA  PROJF/'damq8q.spk'/
C   
      DATA NUID,IDM,IHEDF/34*0/
C
      DATA NCALL,LUP/0,18/
C
      SAVE
C
C     ==================================================================
C   
      IF(NC.LE.0) RETURN
C   
      IF(NCALL.GT.0) GO TO 50
C   
      NCALL=1
      OPEN(UNIT       = LUP,                 !OPEN NEW SPK-FILE
     &     FILE       = PROJF,               !FOR PROJECTION OUTPUT
     &     STATUS     = 'UNKNOWN',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(2048),
     &     IOSTAT     = IOS)
C   
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   RETURN
                   ENDIF
C   
      CALL SPKIO(0,LUP,0,0,0,0,0,0,IERR)
      CALL SPKERR(IERR)
      IF(IERR.NE.0) RETURN
C   
      CNAMFIL(1,LUP)='damq'
      CNAMFIL(2,LUP)='8q.s'
      CNAMFIL(3,LUP)='pk  '
      KFIL(LUP)='SPK '
C   
   50 NUID=NUID+1
      IHEDF(1)=NUID
      IHEDF(8)=4
      IHEDF(9)=64
      IHEDF(10)=2*NC
      IHEDF(11)=1
      IHEDF(12)=NC
C   
      CALL MILYMDHMS(ITMP)
C
      DO 60 I=1,6
      IHEDH(I+8) =ITMP(I)
   60 CONTINUE
C   
      CALL SPKIO(2,LUP,IDM,IHEDF,IDM,IDAT,IDM,IDM,IERR)
      CALL SPKERR(IERR)
      IF(IERR.NE.0) RETURN
C   
      WRITE(CMSSG,70)KIND,NUID
   70 FORMAT(A2,'-ID=',I6)
      CALL MESSLOG(LOGUT,LOGUP)
C   
      RETURN
      END
