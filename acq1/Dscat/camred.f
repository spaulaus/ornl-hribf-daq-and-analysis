C$PROG CAMRED
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE CAMRED
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SCATB/ READERR,ZEROERR,LOGERR,NDUMPS
      CHARACTER*4                   LOGERR
      INTEGER*4     READERR,ZEROERR,       NDUMPS
C     ------------------------------------------------------------------
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
      CHARACTER*4   TY,KI,LA
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SCAT3/ CC(NSC),NN(NSC),AA(NSC),FF(NSC),VBUF(NSC),NLIST
C     ------------------------------------------------------------------
      INTEGER*4 STAT(NSC)
C
      DATA MODE/1/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 I=1,NR
      STAT(I)=0
   10 CONTINUE
C
      CALL CAMLIST(CC,NN,AA,FF,MODE,VBUF,NLIST,STAT)
C
      DO 50 I=1,NR
      IF(STAT(I).EQ.0) GO TO 50
      READERR=READERR+1
      IF(LOGERR.EQ.'OFF ') GO TO 50
      WRITE(CMSSG,40)I,STAT(I)
      CALL MESSLOG(LOGUT,LOGUP)
   40 FORMAT('BAD SCAT STAT - N,ZSTAT =',I6,Z8)
   50 CONTINUE
      NDUMPS=NDUMPS+1
      RETURN
      END
