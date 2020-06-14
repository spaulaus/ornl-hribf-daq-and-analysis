C$PROG PROJE     - Does projections for PROJALL
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PROJE(ID,MID,DEGA,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DIR/  KLOC(6),JHSP(4),LENG(4),ND,NHW,           !/DIR
     &             LENH,LENT,IOF,LDF,NHIS,LEND(4),           !/DIR
     &             LENS(4),MINC(4),MAXC(4),CONS(4),ITEX(20), !/DIR
     &             ITIT(10),LABX(3),LABY(3),MSER(10),KFILT   !/DIR 
C     ------------------------------------------------------------------
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
C
      COMMON/MD01/ KODFIL
C
C     ==================================================================
      DIMENSION ADAT(16384)
      INTEGER*4 IDATF(16384),IHEDF(128)
      INTEGER*2 IHEDH(256)
C   
      EQUIVALENCE (IHEDF(1),XBUF(1)),(IHEDH(1),XBUF(1))
      EQUIVALENCE (IDATF(1),XBUF(129)),(ADAT(1),XBUF(129))
      EQUIVALENCE (IDI,IHEDF(1))
      EQUIVALENCE (NCHI,IHEDF(12))
C   
      INTEGER*4 TITI(20),KPARI(9),FILI(6),MSG(7)
      INTEGER*4 KX(64),KY(64),DGI,HIDI
C   
      EQUIVALENCE (LXG,KPARI(4)),(LYG,KPARI(6))
C
      CHARACTER*4  KINF
C
      SAVE
C
C     ==================================================================
C   
      IERR=0
C   
      KODE=KODFIL
      CALL LUGET(KODE,LUH,LUD,KINF,IERR)
C   
      IF(IERR.EQ.0.AND.KINF.EQ.'HIS ') GO TO 20
C   
      WRITE(CMSSG,10)
   10 FORMAT('REQUIRED INPUT HIS-FILE NOT OPEN')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C   
C     ------------------------------------------------------------------
C     READ IN THE "MIGHTY" CHICKEN GATE
C     ------------------------------------------------------------------
C   
   20 CALL BANIO(1,20,FILI,TITI,HIDI,MID,DGI,KX,KY,NP,NIDI,
     &KPARI,MSG,IERR)
C   
      IF(IERR.EQ.0) GO TO 50
C   
      WRITE(CMSSG,30)MSG
      CALL MESSLOG(LOGUT,LOGUP)
   30 FORMAT(7A4)
      IERR=1
      RETURN
C   
C     ------------------------------------------------------------------
C     DO THE PROJECTION AND SET UP THE HEADER
C     ------------------------------------------------------------------
C   
   50 CALL PROJ(LUD,LUH,ID,IDATF,ADAT,KX,KY,NP,LXG,LYG,DEGA,NC,IERR)
C   
      IF(IERR.NE.0) RETURN
C   
      CALL HEDZOT(IHEDF)
      DO 60 I=1,3
      IHEDF(I+1)=LABX(I)
      XBUF(I+17)=CONS(I)
   60 CONTINUE
      XBUF(21)=CONS(4)
      DO 70 I=1,6
      IHEDH(I+8)=KLOC(I)
   70 CONTINUE
      DO 80 I=1,10
      IHEDF(I+22)=ITIT(I)
   80 CONTINUE
C   
      IDI=ID
      IHEDF(8)=4
      IHEDF(9)=64
      IHEDF(10)=2*NC
      IHEDF(11)=1
      NCHI=NC
      IHEDF(13)=LEND(1)
      IDEGA=DEGA+0.5
      IF(IDEGA.EQ.90) IHEDF(13)=LEND(2)
      IHEDF(14)=0
      IHEDF(15)=0
C   
      IF(IDEGA.EQ.0)  IHEDF(14)=LENS(1)
      IF(IDEGA.EQ.90) IHEDF(14)=LENS(2)
C   
C     ------------------------------------------------------------------
C     CRUNCH SPECTRUM IF REQUESTED
C     ------------------------------------------------------------------
C   
      IF(KRUN.GT.1) CALL CRUNCH(1,KRUN)
      RETURN
      END
