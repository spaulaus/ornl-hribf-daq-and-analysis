C$PROG COMPRO    - Processes SYMBOL-OPERATOR list into POINTER-GOTO list
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE COMPRO
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                               VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 KI
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD05/ LISI(400),SYM(3,50),OPR(50),NLS, NSY, DSPF
      CHARACTER*4                                        DSPF
      INTEGER*4    LISI,     SYM,      OPR,    NLS, NSY
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
C
      SAVE
C
C     ****************************************************************
C     ROUTINE TO PROCESS SYMBOL-OPERATOR LIST INTO POINTER-GOTO LIST
C     ****************************************************************
C
      IF(NSY.LE.0)  RETURN              !TST FOR EMPTY
      IF(NERR.NE.0) RETURN              !TST FOR ERROR STATE
C
      IF(LABNDX(SYM(1,1)).NE.0)GOTO 100 !ERROR IF DEFINED SYMBOL EXIST
C
      NT=NT+1                           !INC TOTAL # SYMBOLS
      IF(NT.GT.MAXT) GO TO 110          !TST FOR TABLE OVERFLOW
C
      DO 10 I=1,3                       !SAVE DEFINED SYMBOL
      LA(I,NT)=SYM(I,1)
   10 CONTINUE
      KI(NT)=DSPF
      IF(KI(NT).NE.'NONE') NDD=NDD+1    !INC # SCALERS TO DISPLAY?
      IF(NDD.GT.MAXD) GO TO 140         !TST FOR TOO MANY
C
      LO(NT)=NPO+1                      !SET LO-LIMIT INDEX IN POL-GOL
C
      DO 20 I=2,NSY                     !LOOP ON REMAINING SYMBOLS
      NPO=NPO+1                         !INC POL-GOL COUNTER
      IF(NPO.GT.MAXT) GO TO 120         !TST FOR TABLE OVERFLOW
      POL(NPO)=LABNDX(SYM(1,I))         !SET SYMBOL INDEX IN POL
      IF(POL(NPO).LE.0) GO TO 130       !TST FOR NON-EXIST
      GOL(NPO)=OPR(I)                   !SET OPERATOR NO. IN GOL
   20 CONTINUE
C
      HI(NT)=NPO                        !HI-LIMIT INDEX IN POL-GOL
C
      RETURN
C
  100 WRITE(6,105)(SYM(K,1),K=1,3)
  105 FORMAT(1H ,'ATTEMPT TO RE-DEFINE SYMBOL - ',3A4)
      GO TO 200
  110 WRITE(6,115)MAXT
  115 FORMAT(1H ,'MORE THAN',I4,' SYMBOLS DEFINED')
      GO TO 200
  120 WRITE(6,125)MAXT
  125 FORMAT(1H ,'COMPUTATION TABLE OVERFLOW AT',I4,' ENTRIES')
      GO TO 200
  130 WRITE(6,135)(SYM(K,I),K=1,3)
  135 FORMAT(1H ,'UNDEFINED SYMBOL REFERENCED = ',3A4)
      GO TO 200
C
  140 WRITE(6,145)MAXD
  145 FORMAT(1H ,'REQUEST DISPLAY OF .GT.',I4,' SCALERS - REJECTED')
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'sorry about that')
      CALL EXIT(2)
      END
