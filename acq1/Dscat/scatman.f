C$PROG SCATMAN
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE SCATMAN(MODE)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      COMMON/SCATA/ MAXT,MAXS,SEC,ISET,NERR,MODEGO
      CHARACTER*4                 ISET,     MODEGO
      INTEGER*4     MAXT,MAXS,SEC,     NERR
C     ------------------------------------------------------------------
      COMMON/SCAT0/ SCATBUF(8000,2),NSCAT,SCATDMP,SCATCLR,SCATERR
      CHARACTER*4                         SCATDMP,SCATCLR
      INTEGER*4     SCATBUF,        NSCAT,                SCATERR
C     ------------------------------------------------------------------
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
      CHARACTER*4   TY,KI,LA
C     ------------------------------------------------------------------
C
      CHARACTER*4   MODE
C
      CHARACTER*80  CSCAT
      EQUIVALENCE  (CSCAT,SCATBUF)
C
      INTEGER*4     IBUF(10,800)
      EQUIVALENCE  (IBUF,SCATBUF)
C
      INTEGER*4     JBUF(10)
      CHARACTER*40  CBUF
      EQUIVALENCE  (CBUF,JBUF)
C
      REAL*4        VNF(NSC),VDF(NSC)
      EQUIVALENCE  (VNF,VN),(VDF,VD)
C
      INTEGER*4 JDATE(3),JTIME(2)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      NSCAT=0
      IF(ISET   .NE.'YES ') RETURN
      IF(SCATDMP.NE.'YES ') RETURN
C
      IF(MODE.EQ.'CHEK') GO TO 100
      IF(MODE.EQ.'FLUS') GO TO 105
      IF(MODE.EQ.'INIT') GO TO 200
      RETURN
C
  100 IF(SEC.EQ.0) RETURN
      CALL TIMEKEEP('GET ',ISEC)
      IDIF=ISEC-LASTIME
      IF(IDIF.LT.SEC) RETURN
      LASTIME=ISEC
C
  105 CALL SCATREAD
C
      CALL MILDATE2(JDATE)
      CALL MILTIME(JTIME)
C
      WRITE(CSCAT,110,ERR=160)JDATE,JTIME,SEC,SCATCLR
  110 FORMAT('SCALER DUMP  ',2A4,A1,3X,2A4,'  Tsec=',I4,'  Clear= ',A4)
C
      DO 150 N=1,NT
C
      IF(KI(N).EQ.'FLOT') GO TO 120
C
      WRITE(CBUF,115,ERR=160)VN(N),VDF(N)
  115 FORMAT(I12,1PE12.3)
      GO TO 130
C
  120 WRITE(CBUF,125,ERR=160)VNF(N),VDF(N)
  125 FORMAT(1P2E12.3)
C
  130 DO 140 I=1,6
      IBUF(I+3,N+2)=JBUF(I)
  140 CONTINUE
C
  150 CONTINUE
C
  160 IF(SCATCLR.EQ.'YES ') CALL ZOTUM
      NSCAT=1
      RETURN
C
  200 CALL TIMEKEEP('GET ',LASTIME)
      RETURN
      END
