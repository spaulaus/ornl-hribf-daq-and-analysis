C$PROG ZOTUM     - Zeros all scalers for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE ZOTUM
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 TY
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      common/sd02a/ modty(512),vmemod(20),vmesn(20),vmeidx(20),nvme
      character*8   modty,     vmemod
      integer*4                           vmesn,    vmeidx,    nvme
C     ------------------------------------------------------------------
C
      CHARACTER*4  KXQ
C
      SAVE
C
C     ------------------------------------------------------------------
C     ZERO THE SCALERS
C     ------------------------------------------------------------------
C
      DO 10 I=1,NT
      VN(I)=0
      VO(I)=0
      VD(I)=0
   10 CONTINUE
C
      DO 50 I=1,NR
      if ((cn(i).eq. 0) .and. 
     &    (sn(i).eq. 0) .and. 
     &    (a(i).eq.0)) then
          goto 50
      endif
      IF(TY(I).EQ.'ECL ') GO TO 50
      if (ty(i) .eq. 'VME ') go to 50
      CALL BHIO(1,LCA,CN(I),SN(I),A(I),9,IDUM,0,STAT)
      IF(STAT.EQ.0.OR.STAT.EQ.1) GO TO 50
      KXQ='????'
      IF(STAT.EQ.2) KXQ='X   '
      IF(STAT.EQ.3) KXQ='X&Q '
      WRITE(CMSSG,20)KXQ,CN(I),SN(I),A(I)
   20 FORMAT('BAD-',A,' ZEROING SCALER - C,N,A =',3I4)
      CALL MESSLOG(LOGUT,LOGUP)
   50 CONTINUE
C
      IF(NEC.LE.0 .and. nvme .le. 0) RETURN
C
      DO 100 I=1,NEC
      CALL BHIO(1,LCA,ECN(I),ESN(I),0,16,'40'X,1,STAT)
      IF(STAT.EQ.0.OR.STAT.EQ.1) GO TO 100
      KXQ='????'
      IF(STAT.EQ.2) KXQ='X   '
      IF(STAT.EQ.3) KXQ='X&Q '
      WRITE(CMSSG,60)KXQ,ECN(I),ESN(I)
   60 FORMAT('BAD-',A,' ZEROING ECL SCALER - C,N =',2I4)
      CALL MESSLOG(LOGUT,LOGUP)
  100 CONTINUE
      call vmeclr()
      RETURN
      END
