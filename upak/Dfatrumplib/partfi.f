C$PROG PARTFI
      SUBROUTINE PARTFI(JRAK,NJ,ATM,JATA,JATZ,Q,IFAIL,ISOR)
C   
      IMPLICIT REAL*8 (A-H,O-Z)
C   
      CHARACTER*4  RESENT,CKTMP(2)
C
      INTEGER*4    KTMP(2)
C
      EQUIVALENCE (CKTMP,KTMP)
C   
      INTEGER*4 NUCNA(120)
C   
      CHARACTER*60 CNUCNA(8)
C   
      DIMENSION JRAK(20),A(10),KZ(10),KA(10),EXCS(10),ISORC(10)
C
      DIMENSION ATM(4),JATA(4),JATZ(4)
C   
      EQUIVALENCE (CNUCNA,NUCNA)
C   
      DATA CNUCNA/
     1 'H   HE  LI  BE  B   C   N   O   F   NE  NA  MG  AL  SI  P   '
     2,'S   CL  AR  K   CA  SC  TI  V   CR  MN  FE  CO  NI  CU  ZN  '
     3,'GA  GE  AS  SE  BR  KR  RB  SR  Y   ZR  NB  MO  TC  RU  RH  '
     4,'PD  AG  CD  IN  SN  SB  TE  I   XE  CS  BA  LA  CE  PR  ND  '
     5,'PM  SM  EU  GD  TB  DY  HO  ER  TM  YB  LU  HF  TA  W   RE  '
     6,'OS  IR  PT  AU  HG  TL  PB  BI  PO  AT  RN  FR  RA  AC  TH  '
     7,'PA  U   NP  PU  AM  CM  BK  CF  ES  FM  MD  NO  LR  RF  HA  '
     8,'NH  NS  UO  UE                                              '/
C   
C     **************************************************************
C     THIS ROUTINE  EXTRACTS ATOMIC AND MASS NOS. FROM A REACTION
C     WRITTEN IN THE NATURAL WAY
C     **************************************************************
C     IFAIL=1  SAYS SYNTAX ERROR IN REACTION FORMAT
C     IFAIL=2 SAYS ERROR IN REACTION A BALANCE
C     IFAIL=3 SAYS ERROR IN REACTION Z BALANCE
C     IFAIL=11,12,13,14 SAYS NAME OF PARTICLE 1,2,3,4 NOT IN ELE LIST
C     IFAIL=21,22,23,24 SAYS PARTICLE 1,2,3,4 OUTSIDE MASS FORMULA RANGE
C     PARTICLE 1,2,3,4 MEANS TARG, PROJ, OUTGOING, RESIDUAL
C     **************************************************************
C   
      RESENT='YES '                     !SET "RESIDUAL ENTERED" FLAG
      IFAIL=0                           !RESET FAIL-FLAG
C   
      IHI=NXBL(JRAK,1,80)-1             !FIND 1ST BLANK (END OF REACT)
      IF(IHI.LE.0) GO TO 200            !TST FOR BLANK FIELD
C   
      LRP=IFIND(JRAK,Z'29',1,IHI)       !FIND RIGHT-PAREN ")"
      IF(LRP.LE.0) GO TO 200            !TST FOR "NOT FOUND"
      IF(LRP.EQ.IHI) RESENT='NO  '      !SET FLAG IF RESIDUAL NULL
C   
      LLP=IFIND(JRAK,Z'28',1,LRP)
      IF(LLP.LE.0) GO TO 200
C   
      KASUM=0
      KZSUM=0
      LA=1
      LB=LLP-1
      NS=1
C   
      CALL GETAZ(JRAK,LA,LB,KA(NS),KZ(NS),IERR)
      IF(IERR.NE.0) GO TO 200
C   
   20 LA=LB+2
      IF(LA.GE.LRP) GO TO 50
      LB=IFIND(JRAK,Z'2C',LA,LRP)-1
      IF(LB.LE.0) LB=LRP-1
      NS=NS+1
      CALL GETAZ(JRAK,LA,LB,KA(NS),KZ(NS),IERR)
      IF(IERR.NE.0) GO TO 200
C   
      IF(NS.LE.2) GO TO 20
      KASUM=KASUM+KA(NS)
      KZSUM=KZSUM+KZ(NS)
      GO TO 20
C   
   50 IF(RESENT.EQ.'NO  ') GO TO 60
      LA=LRP+1
      LB=IHI
      NS=NS+1
      CALL GETAZ(JRAK,LA,LB,KA(NS),KZ(NS),IERR)
      IF(IERR.NE.0) GO TO 200
      GO TO 100
C   
   60 NS=NS+1
      KA(NS)=KA(1)+KA(2)-KASUM
      KZ(NS)=KZ(1)+KZ(2)-KZSUM
      CKTMP(1)='    '
      CKTMP(2)='    '
      WRITE(CKTMP(1),70)KA(NS)
   70 FORMAT(I4)
      KZZ=KZ(NS)
      CKTMP(2)='??  '
      IF(KZZ.GE.1.AND.KZZ.LE.109) KTMP(2)=NUCNA(KZZ)
      CALL SQUEZL(KTMP,1,8)
      CALL LODUP(KTMP,1,8,JRAK,IHI+1)
C   
  100 DO 180 KDO=1,NS
      ISORC(KDO)=1
      IZ=KZ(KDO)
      IA=KA(KDO)
C   
      CALL MASSEX(IZ,IA,DPEX,DPER,ISORC(KDO),IERR)
C   
      IF(IERR.NE.0) GO TO 200
C   
      EX=DPEX
      ER=DPER
      EXCS(KDO)=EX
      EX=EX/931478.0
      AA=KA(KDO)
      A(KDO)=AA+EX
  180 CONTINUE
      KASUM=0
      KZSUM=0
      ASUM=0.0
      IUP=NS-1
      IF(IUP.LT.3) GO TO 192
      DO 190 I=3,IUP
      KZSUM=KZSUM+KZ(I)
      KASUM=KASUM+KA(I)
      ASUM=ASUM+A(I)
  190 CONTINUE
  192 CONTINUE
      Q=(A(1)+A(2)-ASUM-A(NS))*931.478
      IF((KA(1)+KA(2)).NE.(KASUM+KA(NS))) IFAIL=2
      IF((KZ(1)+KZ(2)).NE.(KZSUM+KZ(NS))) IFAIL=3
      ATM(1)=A(2)
      ATM(2)=A(1)
      ATM(3)=ASUM
      ATM(4)=A(NS)
      JATA(1)=KA(2)
      JATA(2)=KA(1)
      JATA(3)=KASUM
      JATA(4)=KA(NS)
      JATZ(1)=KZ(2)
      JATZ(2)=KZ(1)
      JATZ(3)=KZSUM
      JATZ(4)=KZ(NS)
      ISOR=1
      IF(NS.LT.4.AND.IFAIL.EQ.0) IFAIL=4
      DO 210 I=1,NS
      IF(ISORC(I).GT.ISOR) ISOR=ISORC(I)
      IF(ISOR.GT.3) ISOR=3
  210 CONTINUE
      RETURN
  200 IFAIL=1
      RETURN
      END
