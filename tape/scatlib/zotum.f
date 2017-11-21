C$PROG ZOTUM
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE ZOTUM
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
      CHARACTER*4   TY,KI,LA
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
C     ------------------------------------------------------------------
      common/scat2a/ modty(NSC),vmemod(20),vmesn(20),vmeidx(20),nvme
      character*8   modty,     vmemod
      integer*4                           vmesn,    vmeidx,    nvme
C     ------------------------------------------------------------------
      COMMON/SCATB/ READERR,ZEROERR,LOGERR,NDUMPS
      CHARACTER*4                   LOGERR
      INTEGER*4     READERR,ZEROERR,       NDUMPS
C     ------------------------------------------------------------------
      COMMON/SCATC/ CNZ(100),SNZ(100),AZ(100),NZOT
      INTEGER*4     CNZ,     SNZ,     AZ,     NZOT
C     ------------------------------------------------------------------
C
      CHARACTER*4   KXQ
C
      SAVE
C
C     ******************************************************************
C     ZERO THE SCALERS
C     ******************************************************************
C
      DO 10 I=1,NT
      VN(I)=0
      VO(I)=0
      VD(I)=0
   10 CONTINUE
C
      DO 50 I=1,NZOT
      if ((cn(i).eq. 0) .and. 
     &    (sn(i).eq. 0) .and. 
     &    (a(i).eq.0)) then
          goto 50
      endif
      CALL CAMACIO(1,CNZ(I),SNZ(I),0,9,IDUM,0,STAT)
      IF(STAT.EQ.0.OR.STAT.EQ.1) GO TO 50
      ZEROERR=ZEROERR+1
      IF(LOGERR.EQ.'OFF ') GO TO 50
      KXQ='????'
      IF(STAT.EQ.2) KXQ='X   '
      IF(STAT.EQ.3) KXQ='X&Q '
      WRITE(CMSSG,20) KXQ,CNZ(I),SNZ(I),AZ(I)
   20 FORMAT('BAD-',A4,'ZEROING SCALER - C,N,A =',3I4)
      CALL MESSLOG(LOGUT,LOGUP)
   50 CONTINUE
C
      IF ((NEC.LE.0) .and. (nvme .le. 0)) RETURN
C
      DO 100 I=1,NEC
      CALL CAMACIO(1,ECN(I),ESN(I),0,16,'40'X,1,STAT)
      IF(STAT.EQ.0.OR.STAT.EQ.1) GO TO 100
      ZEROERR=ZEROERR+1
      IF(LOGERR.EQ.'OFF ') GO TO 100
      KXQ='????'
      IF(STAT.EQ.2) KXQ='X   '
      IF(STAT.EQ.3) KXQ='X&Q '
      WRITE(CMSSG,60) KXQ,ECN(I),ESN(I)
   60 FORMAT('BAD-',A4,'ZEROING ECL SCALER - C,N =',2I4)
      CALL MESSLOG(LOGUT,LOGUP)
  100 CONTINUE
      call vmeclr()
      RETURN
      END
