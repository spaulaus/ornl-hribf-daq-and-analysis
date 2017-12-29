C$PROG COMPUM
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE COMPUM
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      real*8 vn, vo, vd
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
      CHARACTER*4   TY,KI,LA
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
C     ------------------------------------------------------------------
      REAL*8 VNF(NSC),VDF(NSC)
C
      REAL*4 CVAL,DVAL,CV,DV,CVX,DVX
C
      EQUIVALENCE (VNF,VN),(VDF,VD)
C
      SAVE
C
C     ******************************************************************
C     ROUTINE TO GENERATE COMPUTED SCALER VALUES & RATES
C     ******************************************************************
C     LO(I)   = LO-INDEX IN POL & GOL FOR COMPUTING SCALER-I
C     HI(I)   = HI-INDEX IN POL & GOL FOR COMPUTING SCALER-I
C
C     POL(J)  - POINTS TO VALUE IN VN TO BE USED
C     GOL(J)  - CONTAINS GOTO LIST (1,2,3,4) FOR + - / * OPERATION
C     ******************************************************************
C
C
      IF(NR.GE.NT) RETURN               !TST FOR NONE TO COMPUTE
C
      NA=NR+1                           !FIRST INDEX TO COMPUTE
C
      DO 200 I=NA,NT                    !LOOP ON COMPUTED VALUES
C
      VNF(I)=0.0
      VDF(I)=0.0
C
      CVAL=0.0                          !RESET CUMULATIVE VALUE
      DVAL=0.0                          !RESET DIFFERENCE VALUE
      JA=LO(I)                          !FIRST INDEX IN COMP-LIST
      JB=HI(I)                          !LAST  INDEX IN COMP-LIST
C
      DO 100 J=JA,JB                    !LOOP ON COMP-LIST ENTRIES
      JG=GOL(J)                         !OPERATION-TYPE
      JP=POL(J)                         !INDEX IN VN-ARRAY
      CV=0.0
      IF(JP.LE.NR) CV=VN(JP)            !RAW      VALUES ARE INTEGER
      IF(JP.GT.NR) CV=VNF(JP)           !COMPUTED VALUES ARE FLO6TING
      DV=VDF(JP)                        !DIFFERENCE VALUE
C
      GO TO(10,20,30,40),JG             !GO TO SPECIFIC OPERATION
      GO TO 100                         !SKIP IF NOT 1,2,3,4
C
   10 CVAL=CVAL+CV                      !ADD TO   ACCUMULATED VALUE
      DVAL=DVAL+DV
      GO TO 100
   20 CVAL=CVAL-CV                      !SUB FROM ACCUMULATED VALUE
      DVAL=DVAL-DV
      GO TO 100
   30 CVX=CV
      DVX=DV
      IF(CVX.LT.1.0) CVX=1.0
      IF(DVX.LT.1.0) DVX=1.0
      CVAL=CVAL/CVX                     !DIV INTO ACCUMULATED VALUE
      DVAL=DVAL/DVX
      GO TO 100
   40 CVAL=CVAL*CV                      !MUL BY   ACCUMULATED VALUE
      DVAL=DVAL*DV
C
  100 CONTINUE
C
      VNF(I)=CVAL                       !SAVE FLOATING VALUE
      VDF(I)=DVAL
C
  200 CONTINUE
      RETURN
      END
