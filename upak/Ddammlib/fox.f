C$PROG FOX       - Sets parameters & computes QFN for routine SMIN
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      FUNCTION FOX(X)
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
C
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C
      COMMON/SM03/ XP(4,44),XPSAV(4,44),IPO(176),JPO(176)
C
      COMMON/SM04/ KXF(4,44),KFUN(44),BETA(50),IVF(4)
C
      COMMON/SM06/ NUMITS,ICON
C
      DIMENSION X(*)
C
      SAVE
C
C     ------------------------------------------------------------------
C     THIS FUNCTION DOES THE ACTUAL PARAMETER CHANGES FOR ROUTINE MIN
C     AND RETURNS THE RESULTING QFN
C     ------------------------------------------------------------------
C   
      IF(MSGF.NE.'    ') GO TO 210
      IF(NVLUI.EQ.0) GO TO 100
      DO 50 I=1,NVLUI
      IP=IPO(I)
      JC=JPO(I)
      GO TO (10,20,20,20),IP
   10 XP(IP,JC)=XPSAV(IP,JC)+DXFAC*X(I)
      GO TO 50
   20 XP(IP,JC)=XPSAV(IP,JC)*X(I)
   50 CONTINUE
  100 IF(NVLU.EQ.NVLUI) GO TO 200
      NDX=NVLUI
      DO 180 I=1,4
      IF(IVF(I).EQ.1) GO TO 180
      NDX=NDX+1
      DO 150 J=1,NPK
      IF(KXF(I,J).NE.0) GO TO 150
      IF(I.GT.1) GO TO 120
      XP(I,J)=XPSAV(I,J)+DXFAC*X(NDX)
      GO TO 150
  120 XP(I,J)=XPSAV(I,J)*X(NDX)
  150 CONTINUE
  180 CONTINUE
  200 CALL PKFIT
  210 FOX=QFN
      NUMITS=NUMITS+1
      IF(10*(NUMITS/10).NE.NUMITS) RETURN
C   
      WRITE(CMSSG,230)(IWD(I),I=1,7),QFN
  230 FORMAT(1H>,7A4,'FITTING - QFN = ',F8.2,'       *')
      CALL MESSLOG1(LOGUT,LOGUP)
C   
      RETURN
      END
