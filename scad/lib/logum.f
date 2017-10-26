C$PROG LOGUM     - Logs scalers (read by calling prog) for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LOGUM(LT,LP)
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
     &                       KI,     VN,     VO,             PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
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
      INTEGER*4    LT,LP,N
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     LOG SCALERS
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,10)
   10 FORMAT('CURRENT SCALER VALUES ARE LISTED BELOW')
      CALL MESSLOG(LT,LP)
      WRITE(CMSSG,20)
   20 FORMAT('======================================')
      CALL MESSLOG(LT,LP)
C
      DO 100 N=1,NT
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
   60 IF(LT.NE.0) THEN
      WRITE(LT,70)(LA(J,N),J=1,3),JV2,KV2
      ENDIF
C
      WRITE(LP,70)(LA(J,N),J=1,3),JV2,KV2
C
   70 FORMAT(2A4,A3,1X,2A4,2X,2A4)
C
  100 CONTINUE
C
      WRITE(CMSSG,110)
  110 FORMAT('END SCALER LIST')
      CALL MESSLOG(LT,LP)
      WRITE(CMSSG,20)
      CALL MESSLOG(LT,LP)
C
      RETURN
      END
