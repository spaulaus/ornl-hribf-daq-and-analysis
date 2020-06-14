C$PROG GETAZ     - Decodes element name and returns A and Z
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE GETAZ(IWD,IA,IB,JA,JZ,IERR)
C
      INTEGER*4 NUCN(120),IWD(*)
C
      CHARACTER*60 CNUCN(8)
C
      EQUIVALENCE (CNUCN,NUCN)
C
C     ------------------------------------------------------------------

      DATA CNUCN/
     1 'H   HE  LI  BE  B   C   N   O   F   NE  NA  MG  AL  SI  P   '
     2,'S   CL  AR  K   CA  SC  TI  V   CR  MN  FE  CO  NI  CU  ZN  '
     3,'GA  GE  AS  SE  BR  KR  RB  SR  Y   ZR  NB  MO  TC  RU  RH  '
     4,'PD  AG  CD  IN  SN  SB  TE  I   XE  CS  BA  LA  CE  PR  ND  '
     5,'PM  SM  EU  GD  TB  DY  HO  ER  TM  YB  LU  HF  TA  W   RE  '
     6,'OS  IR  PT  AU  HG  TL  PB  BI  PO  AT  RN  FR  RA  AC  TH  '
     7,'PA  U   NP  PU  AM  CM  BK  CF  ES  FM  MD  NO  LR  RF  DB  '
     8,'SG  BH  HS  MT  EA  EB  EC  ED  EE  EF  EG  EH  EI          '/
C     ------------------------------------------------------------------
      INTEGER*4 X20,X30,X39,X41,X5A
C
      DATA      X20,X30,X39,X41,X5A/'20'X,'30'X,'39'X,'41'X,'5A'X/
C
      CHARACTER*4 CJAW,CJNW,CJZW
C
      INTEGER*4    JAW, JNW, JZW
C
      EQUIVALENCE (CJAW,JAW),(CJNW,JNW),(CJZW,JZW)
C
      SAVE
C
C     ------------------------------------------------------------------
C     THIS ROUTINE DECODES CHARACTERS "IA THRU IB" IN "IWD" TO
C     GET ATOMIC NUMBER AND MASS "JA AND JZ"
C
C     IERR = 0 SAYS EVERYTHING OK
C     IERR = 1 SAYS TOO MANY DIGITS
C     IERR = 2 SAYS TOO MANY LETTERS
C     IERR = 3 SAYS "NAME NOT FOUND"
C     IERR = 4 SAYS ILLEGAL CHARACTERS
C     ------------------------------------------------------------------
C
      JA=0
      JZ=0
      MN=0
      IERR=0
      CJAW=' '
      CJNW=' '
      CJZW=' '
C
C     ------------------------------------------------------------------
C     LOAD DIGITS AND LETTERS INTO JNW AND JAW RESPECTIVELY
C     ------------------------------------------------------------------
C
      NN=0
      NE=-1
      DO 80 I=IA,IB
      CALL ILBYTE(ICH,IWD,I-1)
      IF(ICH.EQ.X20) GO TO 80
      IF(ICH.GE.X30.AND.ICH.LE.X39) GO TO 60
      IF(ICH.GE.X41.AND.ICH.LE.X5A) GO TO 50
      GO TO 220
   50 NE=NE+1
      IF(NE.GT.1) GO TO 200
      CALL ISBYTE(ICH,JAW,NE)
      GO TO 80
   60 NN=NN+1
      IF(NN.GT.3) GO TO 210
      CALL ISBYTE(ICH,JNW,NN-1)
   80 CONTINUE
      IF(CJAW.EQ.'N   '.AND.NN.EQ.0) GO TO 130
C
      N=3
      M=NN-1
      DO 90 I=1,NN
      CALL ILBYTE(IT,JNW,M)
      CALL ISBYTE(IT,JZW,N)
      M=M-1
      N=N-1
   90 CONTINUE
C
      READ(CJZW,105)JA
  105 FORMAT(I4)
C
      DO 110 I=1,118
      IF(JAW.EQ.NUCN(I)) GO TO 120
  110 CONTINUE
      IERR=3
      RETURN
  120 JZ=I
      RETURN
  130 JA=1
      JZ=0
      RETURN
  200 IERR=1
      RETURN
  210 IERR=2
      RETURN
  220 IERR=4
      RETURN
      END
