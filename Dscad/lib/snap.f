C$PROG SNAP      - Logs scalers & rates to file scad.snap
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 05/22/2005
C     ******************************************************************
C
      SUBROUTINE SNAP
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
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD16/ PRNAM
      CHARACTER*76 PRNAM
C     ------------------------------------------------------------------
      COMMON/SDXX/ ISKIP(512),SKIPLAB(8,512),NSKIP
      INTEGER*4    ISKIP,     SKIPLAB,       NSKIP
C
      CHARACTER*32 SKIPLABC(512)
      EQUIVALENCE (SKIPLABC,SKIPLAB)
C     ------------------------------------------------------------------
      CHARACTER*4  KIN(512)
      EQUIVALENCE (KIN,KI)
C
      INTEGER*4    JV(3),KV(3),JV2(2),KV2(2)
      EQUIVALENCE (JV2,JV),(KV2,KV)
C
      REAL*4       VNF(512),VDF(512)
      EQUIVALENCE (VNF,VN),(VDF,VD)
C
      INTEGER*4    NAMF(20),DATIM(6),DATIME(5),JTIME(2),YR,MO,DA,LU,N
C
      INTEGER*4    STRAPPNS,STRAPPEND,JSTAT
C
      CHARACTER*80 CNAMF,LPRCMD
C
      EQUIVALENCE (CNAMF,NAMF)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      LU=37
C
      CALL MILYMDHMS(DATIM)
C
      CALL MILTIME(JTIME)
C
      YR=DATIM(1)
      MO=DATIM(2)
      DA=DATIM(3)
C
      CNAMF=' '
C
      WRITE(CNAMF,10)YR,MO,DA,JTIME
   10 FORMAT('scad-',I4.4,'-',I2.2,'-',I2.2,'_',2A4,'.snap')
C
C
      OPEN(UNIT       = LU,
     &     FILE       = CNAMF,
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = IOS)
C
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   WRITE(LOGUT,20)
                   RETURN
                   ENDIF
C
   20 FORMAT('OUTPUT TO SNAP-FILE NOT DONE!!')
C
C     ------------------------------------------------------------------
C
      CALL MILDATIM(DATIME)
C
      WRITE(LU,30)
   30 FORMAT('CURRENT SCALER VALUES ARE LISTED BELOW ')
      WRITE(LU,40)DATIME
   40 FORMAT('================= ',5A4)
C
      M=0
C
      DO 100 N=1,NT
C
      M=M+1
C
      IF(ISKIP(M).NE.0) THEN
      WRITE(LU,45)SKIPLABC(M)
   45 FORMAT(A)
      M=M+1
      ENDIF
C
      IF(KIN(N).NE.'FLOT') GO TO 50
C
      CALL FLO8(VNF(N),JV)
      CALL FLO8(VDF(N),KV)
      GO TO 60
C
   50 CALL FLI8(VN(N), JV)
      CALL FLO8(VDF(N),KV)
C
   60 WRITE(LU,70)(LA(J,N),J=1,3),JV2,KV2
   70 FORMAT(2A4,A3,1X,2A4,2X,2A4)
C
  100 CONTINUE
C
      WRITE(LU,105)
  105 FORMAT('END SCALER LIST')
C
      WRITE(LU,40)DATIME
C
      CLOSE(UNIT=LU)
C
      LPRCMD='enscript -fCourier-Bold8'
C
      IF(PRNAM.NE.' ') THEN
      ISTAT=STRAPPNS(LPRCMD,'-P')
      ISTAT=STRAPPEND(LPRCMD,PRNAM)
      ENDIF
C
      ISTAT=STRAPPNS(LPRCMD,CNAMF)
C
      CALL SYSTEM(LPRCMD,ISTAT)
C
      IF(ISTAT.NE.0) THEN
      CALL SENDBUF(6,DATLOC,8)
      WRITE(6,110)ISTAT
  110 FORMAT(/,'SNAP ERROR - LPR STAT =',Z8)
      CALL WAIT(2,2,JSTAT)
      CALL SENDBUF(6,DATLOC,8)
      WRITE(6,115)
  115 FORMAT(/,'                                   ')
      RETURN
      ENDIF
C
      CALL SENDBUF(6,DATLOC,8)
C
      WRITE(6,120)
  120 FORMAT(/,'SNAP DONE***********')
      CALL WAIT(1,2,JSTAT)
C
      RETURN
      END
