C$PROG SETUP
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/22/2005
C     ******************************************************************
C
      SUBROUTINE SETUP(LU)
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
      COMMON/SCATA/ MAXT,MAXS,SEC,ISET,NERR,MODEGO
      CHARACTER*4                 ISET,     MODEGO
      INTEGER*4     MAXT,MAXS,SEC,     NERR
C     ------------------------------------------------------------------
      COMMON/SCATD/ NORISAV,NORFSAV
      INTEGER*4     NORISAV
      REAL*4        NORFSAV
C     ------------------------------------------------------------------
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
      CHARACTER*4   TY,KI,LA
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SCAT3/ CC(NSC),NN(NSC),AA(NSC),FF(NSC),VBUF(NSC),NLIST
C     ------------------------------------------------------------------
      INTEGER*4     IWD(20),LWD(2,40),ITYP(40),NF
C
      CHARACTER*320 CLWDL
C
      CHARACTER*4   CWD(20),CLWD(2,40),NORMF
C
      EQUIVALENCE  (CWD,IWD),(CLWD,LWD),(CLWDL,LWD)
C
      CHARACTER*1   COMBYT
C
      EQUIVALENCE  (COMBYT,IWD)
C
      REAL*4        XV
C
      SAVE
C
C     ******************************************************************
C     SETUP - READS THE SCA-FILE AND SETS UP THE REQUIRED ARRAYS, ETC
C     NEEDED TO DIRECT THE READING AND DISPLAY
C     ******************************************************************
C     LA(I,J)  = LABEL  FOR SCALER-J
C     CN(J)    = CRATE# FOR SCALER-J
C     SN(J)    = SLOT#  FOR SCALER-J
C      A(J)    = SUBAD  FOR SCALER-J
C      F(J)    = F-CLR  FOR SCALER-J
C     TY(J)    = TYPE   FOR SCALER-J  ('ECL ' OR '    ')
C     KI(J)    = KIND   FOR SCALER-J  ('    ' OR 'FLOT' OR 'NONE')
C     VN(J)    = NEW-V  FOR SCALER-J
C     VO(J)    = OLD-V  FOR SCALER-J
C     VD(J)    = VN-VO  FOR SCALER-J
C     PV(J)    = V-PNTR FOR SCALER-J  (POINTS TO VALUE IN VBUF)
C     LO(J)    = C-PNTR FOR SCALER-J  (FIRST ENTRY IN POL-OPR FOR CAL)
C     HI(J)    = C-PNTR FOR SCALER-J  (LAST  ENTRY IN POL-OPR FOR CAL)
C
C     NR       = # OF RAW SCALERS
C     NC       = # OF CAL SCALERS
C     NT       = TOTAL # OF SCALERS (NR+NC)
C
C     VBUF     = VALUE-BUFFER FOR ECL SCALERS (READ 32/CNAF)
C     POL      - POINTS TO SYMBOLS REFERENCED IN SCALER COMPUTATION
C     OPR      - OPERAND LIST ASSOCIATED WITH REFERENCED SYMBOLS
C
C     ECN(K)   = CRATE# FOR ECL SCALER-K
C     ESN(K)   = SLOT#  FOR ECL SCALER-K
C     NEC      = NUMBER OF  ECL SCALERS
C     ******************************************************************
C
      ISET='NO  '
      N=0
      NR=0
      NC=0
      NT=0
      NLN=0
C
      DO 20 J=1,NSC
      DO 10 I=1,3
      LA(I,J)='    '
   10 CONTINUE
      KI(J)='    '
      TY(J)='    '
   20 CONTINUE
C
   50 READ(1,55,END=210)IWD
   55 FORMAT(20A4)
C
      NLN=NLN+1
C
      IF(CWD(1).EQ.' ')    GO TO 50         !Ignore blank lines
C
      IF(COMBYT.EQ.'#')    GO TO 50         !Ignore comment lines
C
      IF(CWD(1).EQ.'$END') GO TO 200
C
      IA=NXBL(IWD,1,80)
      IF(IA.GT.12) GO TO 300
      IF(IA.LE.0)  GO TO 300
C
      N=N+1
      IF(N.GT.MAXT) GO TO 310
C
      CALL LODUP(IWD,1,IA,LA(1,N),1)
C
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER)
      IF(NTER.NE.0) GO TO 300
C
      READ(CLWDL,60,ERR=300)CN(N),SN(N),A(N),F(N),TY(N)
   60 FORMAT(4I8,A4)
C
      IA=IFIND(IWD,'3B'X,1,80)
C
      IF(IA.LE.0) GO TO 50
C
      IA=IA+1
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER)
      IF(NTER.NE.0) GO TO 300
C
      NORMF='NO  '
      DO 70 I=1,NF
      IF(CLWD(1,I).EQ.'NOD ') KI(N)='NONE'
      IF(CLWD(1,I).EQ.'NOR ') THEN
                              NORI=N
                              NORISAV=N
                              NORMF='YES '
                              NORF=1.0
                              NORFSAV=1.0
                              ENDIF
   70 CONTINUE
C
      IF(NORMF.EQ.'NO  ') GO TO 50
C
      DO 80 I=1,NF
      IF(ITYP(I).NE.2) GO TO 80
      CALL MILV(LWD(1,I),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 300
      NORF=XV
      NORFSAV=XV
   80 CONTINUE
C
      GO TO 50
C
  200 NR=N
      NT=NR
      CALL SCATCOM(LU)
C
      GO TO 290
C
  210 NR=N
      NT=N
C
  290 CALL SETZECL
C
      CALL SETPAR
C
      CLOSE(UNIT=LU)
C
      ISET='YES '
      RETURN
C
  300 WRITE(CMSSG,305),NLN
  305 FORMAT('SYNTAX ERROR in SNIT file on line no.',I4)
      GO TO 400
  310 WRITE(CMSSG,315)MAXT
  315 FORMAT('MORE THAN',I4,' RAW SCALERS REQUESTED - NOT ALLOWED')
      GO TO 400
C
  400 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
