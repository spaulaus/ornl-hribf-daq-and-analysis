C$PROG GATEMAN   - Sets up simple 1-D gates for LEMOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE GATEMAN
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/LM30/ GATNAM(20),GATLO(1000),GATHI(1000),GATMX,GATOP,LUGAT
      INTEGER*4    GATNAM,    GATLO,      GATHI,      GATMX,      LUGAT
      CHARACTER*4                                           GATOP
C     ------------------------------------------------------------------
      REAL*4       XV
C
      INTEGER*4    NXNB,LSNB
C
      INTEGER*4    I,IA,IB,NB,ID,LO,HI,KIND,IERR,STAT,NLN
C
      INTEGER*4    IWX(20),LWX(2,40),ITYX(40),NFX,NTERX
C
      CHARACTER*4  KMD,KMX
C
      CHARACTER*80 CGATNAM
C
      EQUIVALENCE (KMD,LWD(1,1)),
     &            (KMX,LWX(1,1)),
     &            (CGATNAM,GATNAM)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'GATZ') GO TO 100
C
      IF(KMD.EQ.'GATE') GO TO 200
C
      IF(KMD.EQ.'GAT ') GO TO 300
C
                        GO TO 1000
C
C     ------------------------------------------------------------------
C     Reset all gates to "impossible"
C     ------------------------------------------------------------------
C
  100 DO 110 I=1,GATMX
      GATLO(I)=1
      GATHI(I)=0
  110 CONTINUE
C
      GATOP='NO  '
C
      RETURN
C
C     ------------------------------------------------------------------
C     Process GATE ID LO HI
C     ------------------------------------------------------------------
C
  200 CALL MILV(LWD(1,2),ID,XV,KIND,IERR)
C
      IF(IERR.NE.0)   GO TO 1000
C
      IF(ID.LT.1)     GO TO 1010
      IF(ID.GT.GATMX) GO TO 1010
C
      CALL MILV(LWD(1,3),LO,XV,KIND,IERR)
      IF(IERR.NE.0)   GO TO 1000
C
      CALL MILV(LWD(1,4),HI,XV,KIND,IERR)
      IF(IERR.NE.0)   GO TO 1000
C
      GATLO(ID)=LO
      GATHI(ID)=HI
C
      RETURN
C
C     ------------------------------------------------------------------
C     Process GATF filename - open and read gate file
C     ------------------------------------------------------------------
C
  300 IA=NXNB(IWDRAW,4,80)
      IF(IA.LE.0) GO TO 1020
      IB=LSNB(IWDRAW,IA,80)
      IF(IB.LE.0) GO TO 1020
      CALL LODUP(IWDRAW,IA,IB,GATNAM,1)
      NB=IB-IA+1
      CALL ISBYTE(0,GATNAM,NB)
C
      OPEN(UNIT    = LUGAT,
     &     FILE    = CGATNAM,
     &     STATUS  = 'OLD',
     &     IOSTAT  = STAT)
C
      IF(STAT.NE.0) THEN
      CALL IOFERR(STAT)
      GO TO 1030
      ENDIF
C
      NLN=0
C
  310 READ(LUGAT,320,END=500)IWX
  320 FORMAT(20A4)
C
      NLN=NLN+1
C
      CALL CASEUP(IWX)
C
      CALL GREAD(IWX,LWX,ITYX,NFX,1,80,NTERX)
C
      IF(KMX.NE.'GATE') GO TO 310
C
      CALL MILV(LWX(1,2),ID,XV,KIND,IERR)
C
      IF(IERR.NE.0) THEN
      WRITE(CMSSG,330)NLN
  330 FORMAT('Syntax error on line# ',I4,' line skipped')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 310
      ENDIF
C
      IF(ID.LT.1.OR.ID.GT.GATMX) THEN
      WRITE(CMSSG,340)ID,GATMX,NLN
  340 FORMAT('Gate-ID =',I6,' outside allowed range of 1 to ',I5,
     &       ' - line#',I4,'  skipped')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 310
      ENDIF
C
      CALL MILV(LWX(1,3),LO,XV,KIND,IERR)
C
      IF(IERR.NE.0) THEN
      WRITE(CMSSG,330)NLN
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 310
      ENDIF

      CALL MILV(LWX(1,4),HI,XV,KIND,IERR)
C
      IF(IERR.NE.0) THEN
      WRITE(CMSSG,330)NLN
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 310
      ENDIF
C
      GATLO(ID)=LO
      GATHI(ID)=HI
C
      WRITE(CMSSG,350)ID,LO,HI
  350 FORMAT('Gate-ID, LO, HI =',3I6)
      CALL MESSLOG(LOGUT,LOGUP)
C
      GO TO 310

C
  500 CLOSE(UNIT=LUGAT)
C
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Syntax error in gate specification - cmd ignored')
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)ID,GATMX
 1015 FORMAT('Gate-ID =',I6,' outside allowed range of 1 to ',I5,
     &       ' - cmd ignored')
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Syntax error in gate-file specification - cmd ignored')
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('Unable to open gate-file - cmd ignored')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
