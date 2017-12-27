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
      real*8 vn, vo, vd
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
*     INTEGER*4     IBUF(10,800)
      INTEGER*4     IBUF(20,400)
      EQUIVALENCE  (IBUF,SCATBUF)
C
*     INTEGER*4     JBUF(10)
      INTEGER*4     JBUF(20)
      CHARACTER*80  CBUF
      EQUIVALENCE  (CBUF,JBUF)
C
      REAL*8        VNF(NSC),VDF(NSC)
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
C        Change the output format to accommodate the much longer words
            WRITE(CBUF,115,ERR=160)VN(N),VDF(N)
  115       FORMAT(1P,D21.14,4x,1P,D21.14)
            GO TO 130
C
  120    continue

C        Handle formatting errors with a message
         WRITE(CBUF,125,ERR=160)VNF(N),VDF(N)
  125    FORMAT(1P,D21.14,4x,D21.14)
C
  130    DO 140 I=1,17
            IBUF(I+3,N+1)=JBUF(I)
  140    CONTINUE
C
  150 CONTINUE
C
  160 CONTINUE
      IF(SCATCLR.EQ.'YES ') CALL ZOTUM
      NSCAT=1
      RETURN
C
  200 CALL TIMEKEEP('GET ',LASTIME)
      RETURN
      END
