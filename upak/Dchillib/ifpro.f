C$PROG IFPRO
      SUBROUTINE IFPRO(NOL,JWD,IERR,MSER)
C
      LOGICAL Q
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      COMMON/PPP/ LPATCH(500),LAPAT(500),LPATI
C
      COMMON/YYY/ NAMHIL(20),LISKIN,LTA,LICO,NOHIS,ITRACE
C
      INTEGER*4 LABLS(100),LVAL(100),JWD(32),MSER(10),MSG(10,8)
      CHARACTER*40 MSC(8)
C
      INTEGER*4 MISOFF,HITOFF,PATLOC
C
      INTEGER*2 MIL(65536)
C
      EQUIVALENCE (MIL(1),MILF(1)),(MC,MILC),(MCF,MILCF)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/'ILLEGAL SYNTAX (IF- OR BTAB-STATEMENT)  ',
     2         'TEST PARM OUT OF RANGE FOR IF OR BTAB   ',
     3         'ILLEGAL GATE LIMITS FOR IF OR BTAB      ',
     4         'UNSUPPORTED IF OR BTAB STATEMENT        ',
     5         'ILLEGAL PARAMETER # IN IFX OR IFN       ',
     6         'NO PATH TO PRECEDING LINE               ',
     7         '# OF LABELS AND BANANAS DOESN,T MATCH   ',
     8         '# OF LABELS AND GATES DOESN,T MATCH     '/
C
      INTEGER*4    KMDI
      CHARACTER*4  KMD,NAME
      EQUIVALENCE (KMDI,KMD)
C
      SAVE
C
C     **************************************************************
C
C     LPATCH(I) = ITH LOCATION IN CHIL TO PATCH FOR USER-DEFINED
C                 "GOTO'S" IN THE COMPLETE CHIL LIST
C
C     LAPAT(I)  = ASSOCIATED GOTO-NAME (USER-DEFINED LABEL)
C
C     LPATI     = PATCH INDEX (CNTR) FOR PATCH LIST
C     **************************************************************
C
      IERR=0
      KMDI=JWD(21)
      IA  =JWD(23)
      IB  =JWD(24)
C
      IF(KMD.EQ.'IFX ') GO TO 100
      IF(KMD.EQ.'IFN ') GO TO 100
      IF(KMD.EQ.'IFS ') GO TO 200
      IF(KMD.EQ.'IFU ') GO TO 200
      IF(KMD.EQ.'IFC ') GO TO 400
      IF(KMD.EQ.'IFP ') GO TO 500
      IF(KMD.EQ.'BTAB') GO TO 600
      GO TO 1010
C
C     **************************************************************
C     PROCESS  -  IFX(PARM)LABEL  &  IFN(PARM)LABEL
C     **************************************************************
C
  100 IP=ITERV(JWD,IA,IB,IERR,MSER)     !PICK UP PARM # FOR IFX, IFN
      IF(IERR.NE.0) RETURN              !TST FOR ERROR
      IF(IP.LE.0.OR.IP.GT.NPAR) GO TO 1050 !TST FOR PARM # IN RANGE
      ILO=0                             !SET GATE LIMITS
      IHI=LPAR(IP)-1                    !SET GATE LIMITS
      GO TO 210                         !CODE AS IN-LINE GATE
C
C     **************************************************************
C     PROCESS  -  IFS(CONDITION))LABEL  &  IFU(CONDITION))LABEL
C     **************************************************************
C
  200 CALL GSPAN(JWD,IA,IB,IRP,NV,LVAL,NAME,IERR,MSER)
      IF(IERR.NE.0) RETURN
C
      IF(NV.NE.3) GO TO 1010
      IF(NAME.EQ.'B   ') GO TO 300
      IF(NAME.NE.'G   ') GO TO 1010
C
C     **************************************************************
C     PROCESS  -  IFS(G(P,LO,HI))LABEL  &  IFU(G(P,LO,HI))LABEL
C     **************************************************************
C
      IP =LVAL(1)
      ILO=LVAL(2)
      IHI=LVAL(3)
C
      IF(IP.LE.0.OR.IP.GT.NPAR)   GO TO 1020
      IMX=LPAR(IP)-1
      IF(ILO.LT.  0.OR.ILO.GT.IMX) GO TO 1030
      IF(IHI.LT.ILO.OR.IHI.GT.IMX) GO TO 1030
C
  210 MC=MC+1                           !INC  THE MIL-CNTR
      MARK=2*(MC-1)                     !MARK NEXT OPCODE LOCATION
C
      HITOFF=MARK                       !HIT  DISP FOR IFS & IFX
      MISOFF=12                         !MISS DISP FOR IFS & IFX
      PATLOC=MC+3                       !PATCH LOC FOR IFS & IFX
C
      IF(KMD.EQ.'IFS ') GO TO 220       !TST FOR INVERSE GATE
      IF(KMD.EQ.'IFX ') GO TO 220       !TST FOR INVERSE GATE
      HITOFF=12                         !HIT  DISP FOR IFU & IFN
      MISOFF=MARK                       !MISS DISP FOR IFU & IFN
      PATLOC=MC+2                       !PATCH LOC FOR IFU & IFN
C
  220 MIL(MC)=30                        !OPCODE - IN-LINE GATE
                                                   CALL QQ(0,30)
      MIL(MC+1)=2*(IP-1)                !PDISP
                                                   CALL QQ(1,2)
      MIL(MC+2)=MISOFF                  !CDISP - FAIL
                                                   CALL QQ(2,3)
      MIL(MC+3)=HITOFF                  !CDISP - HIT
                                                   CALL QQ(3,4)
C
      LPATI=LPATI+1                     !INC PATCH LOCATION CNTR
      LPATCH(LPATI)=PATLOC              !SAVE LOCATION TO PATCH
      LAPAT(LPATI)=JWD(27)              !SAVE ASSOCIATED LABEL NAME
C
      MIL(MC+4)=IHI                     !HI-LIMIT
                                                   CALL QQ(4,36)
      MIL(MC+5)=ILO                     !LO-LIMIT
                                                   CALL QQ(5,37)
C
      MILC=MILC+5                       !UPDATE MIL-CNTR
      RETURN
C
C     **************************************************************
C     PROCESS  -  IFS(B(PX,PY,ID))LABEL  &  IFU(B(PX,PY,ID))LABEL
C     **************************************************************
C
  300 IPX=LVAL(1)
      IPY=LVAL(2)
      IF(IPX.LT.1.OR.IPX.GT.NPAR) GO TO 1020
      IF(IPY.LT.1.OR.IPY.GT.NPAR) GO TO 1020
      IDA=LVAL(3)
      IDB=LVAL(3)
      NB=1
C
      CALL BANLOC(IPX,IDA,IDB,NB,ISHI,ISIZ,LOC,IERR,MSER)
C
      IF(IERR.NE.0) RETURN
C
C     **********************************!CODE FOR BASIC BANANA GATE
C
  310 MC=MC+1                           !INC THE MIL-CNTR
      MIL(MC)=28                        !OPCODE - LOAD ACC IMMEDIATE
                                                   CALL QQ(0,28)
      MC=MC+1
      MIL(MC)=0                         !WITH THE VALUE 0
                                                   CALL QQ(0,1)
C
      MC=MC+1                           !INC THE MIL-CNTR
      MARK=2*(MC-1)                     !MARK THE NEXT MIL-LOCATION
C
      HITOFF=MARK                       !HIT  DISP FOR IFS & IFX
      MISOFF=24                         !MISS DISP FOR IFS & IFX
      PATLOC=MC+3                       !PATCH LOC FOR IFS & IFX
C
      IF(KMD.EQ.'IFS ') GO TO 320       !TST FOR INVERSE GATE
      HITOFF=24                         !HIT  DISP FOR IFU & IFN
      MISOFF=MARK                       !MISS DISP FOR IFU & IFN
      PATLOC=MC+2                       !PATCH LOC FOR IFU & IFN
C
  320 MIL(MC)=23                        !LOAD OP-CODE
                                                   CALL QQ(0,23)
      MIL(MC+1)=2*(IPY-1)               !LOAD 1ST TST-PARM (PY) DISP
                                                   CALL QQ(1,8)
      MIL(MC+2)=MISOFF                  !LOAD MARK IN FAIL DISP LOC
                                                   CALL QQ(2,3)
      MIL(MC+3)=HITOFF                  !LOAD HIT DISP
                                                   CALL QQ(3,4)
C
      LPATI=LPATI+1                     !INC  PATCH LOCATION INDEX
      LPATCH(LPATI)=PATLOC              !SAVE LOCATION TO PATCH
      LAPAT(LPATI)=JWD(27)              !SAVE ASSOCIATED LABEL NAMET
C
      MIL(MC+4)=1                       !LOAD PARTIAL MISS INCREMENT
                                                   CALL QQ(4,5)
      MIL(MC+5)=-1                      !LOAD MINUS # OF BANANAS
                                                   CALL QQ(5,10)
      MIL(MC+6)=ISHI                    !LOAD RIGHT-SHIFT CNT FOR PX
                                                   CALL QQ(6,11)
      MIL(MC+7)=2*(IPX-1)               !LOAD 2ND TST-PARM DISP (PX)
                                                   CALL QQ(7,9)
      MIL(MC+8)=0                       !LOAD 0 FOR ALIGNMENT
                                                   CALL QQ(8,13)
      MIL(MC+9)=ISIZ                    !LOAD SIZE OF EACH BAN-VECT
                                                   CALL QQ(9,12)
      MCF=(MC+11)/2                     !FULL WORD INDEX IN MILF
      MILF(MCF)=4*(LOC-1)-MARK          !LOAD FULL-WD DISP OF BAN-LIST
                                                   CALL QQ(0,-7)
C
      MC=MC+11                          !INC  MIL-CNTR
      RETURN
C
C     **************************************************************
C     PROCESS  -  IFC( B(IPX,IPY IDA,IDB))L1,L2,L3,L4..........
C     PROCESS  -  IFC(GS(IPX,IPY IDA,IDB))L1,L2,L3,L4..........
C     **************************************************************
C
  400 CALL GSPAN(JWD,IA,IB,IRP,NV,LVAL,NAME,IERR,MSER)
      IF(IERR.NE.0) RETURN
      IF(NV.NE.4) GO TO 1010
C
      CALL LABLST(JWD,IRP,80,LABLS,NLA,IERR,MSER)
      IF(IERR.NE.0) RETURN
      NLAA=NLA
      IF(2*(NLAA/2).EQ.NLA) NLAA=NLA+1
C
      IF(NAME.EQ.'GS  ') GO TO 450
      IF(NAME.NE.'B   ') GO TO 1010
C
      IPX=LVAL(1)
      IPY=LVAL(2)
      IF(IPX.LT.1.OR.IPX.GT.NPAR) GO TO 1020
      IF(IPY.LT.1.OR.IPY.GT.NPAR) GO TO 1020
      IDA=LVAL(3)
      IDB=LVAL(4)
C
      NB=IDB-IDA+1
C
      CALL BANLOC(IPX,IDA,IDB,NB,ISHI,ISIZ,LOC,IERR,MSER)
      IF(IERR.NE.0) RETURN
C
      IF(NLA.NE.NB) GO TO 1070
C
C     **********************************!CODE FOR BASIC BANANA GATE
C
  410 MC=MC+1                           !INC THE MIL-CNTR
      MIL(MC)=28                        !OPCODE - LOAD ACC IMMEDIATE
                                                   CALL QQ(0,28)
      MC=MC+1
      MIL(MC)=0                         !WITH THE VALUE 0
                                                   CALL QQ(0,1)
C
      MC=MC+1                           !INC THE MIL-CNTR
      MARK=2*(MC-1)                     !MARK THE NEXT MIL-LOCATION
C
      MIL(MC)=23                        !LOAD OP-CODE FOR BAN-GATE
                                                   CALL QQ(0,23)
      MIL(MC+1)=2*(IPY-1)               !LOAD 1ST TST-PARM (PY) DISP
                                                   CALL QQ(1,8)
      MIL(MC+2)=2*(NLAA+1)+24           !LOAD FAIL DISP LOC
                                                   CALL QQ(2,3)
      MIL(MC+3)=24                      !LOAD HIT DISP
                                                   CALL QQ(3,4)
C
      MIL(MC+4)=1                       !LOAD PARTIAL MISS INCREMENT
                                                   CALL QQ(4,5)
      MIL(MC+5)=-NB                     !LOAD MINUS # OF BANANAS
                                                   CALL QQ(5,10)
      MIL(MC+6)=ISHI                    !LOAD RIGHT-SHIFT CNT FOR PX
                                                   CALL QQ(6,11)
      MIL(MC+7)=2*(IPX-1)               !LOAD 2ND TST-PARM DISP (PX)
                                                   CALL QQ(7,9)
      MIL(MC+8)=0                       !LOAD 0 FOR ALIGNMENT
                                                   CALL QQ(8,13)
      MIL(MC+9)=ISIZ                    !LOAD SIZE OF EACH BAN-VECT
                                                   CALL QQ(9,12)
      MCF=(MC+11)/2                     !FULL WORD INDEX IN MILF
      MILF(MCF)=4*(LOC-1)-MARK          !LOAD FULL-WD DISP OF BAN-LIST
                                                   CALL QQ(0,-7)
C
      MC=MC+11                          !INC  MIL-CNTR
C
C     **********************************!CODE FOR COMPUTED GOTO
C
  420 MC=MC+1                           !INC  MIL-CNTR
      MARK=2*(MC-1)                     !MARK NEXT MIL-LOCATION
      MIL(MC)=34                        !OPCODE FOR COMPUTED GOTO
                                                   CALL QQ(0,34)
C
      DO 430 I=1,NLA                    !LOOP ON # OF LABELS
      MC=MC+1                           !INC  MIL-CNTR
      MIL(MC)=MARK                      !STORE OPCODE LOCATION
                                                   CALL QQ(0,4)
      LPATI=LPATI+1                     !INC PATCH LOCATION CNTR
      LPATCH(LPATI)=MC                  !SAVE LOCATION TO PATCH
      LAPAT(LPATI)=LABLS(I)             !SAVE ASSOCIATED LABEL NAME
  430 CONTINUE
C
      IF(NLAA.EQ.NLA) RETURN            !TST FOR PAD REQUIRED
C
      MC=MC+1                           !INC MIL-CNTR
      MIL(MC)=0                         !LOAD PAD
                                                   CALL QQ(0,13)
      RETURN
C
C     **********************************!PROCESS GATE LIST
C
  450 IP=LVAL(1)
      IS=LVAL(2)
      NA=LVAL(3)
      NB=LVAL(4)
      NG=NB-NA+1
      IF(NG.NE.NLA) GO TO 1080          !TST FOR CORRECT # GATES
      CALL GATLOC(IP,IS,NA,NB,ISHI,ISIZ,LOC,LCG,IGT,IERR,MSER)
      IF(IERR.NE.0) RETURN
C
  470 MC=MC+1                           !INC THE MIL-CNTR
      MIL(MC)=28                        !OPCODE - LOAD ACC IMMEDIATE
                                                   CALL QQ(0,28)
      MC=MC+1
      MIL(MC)=0                         !WITH THE VALUE 0
                                                   CALL QQ(0,1)
C
      MC=MC+1                           !INC THE MIL-CNTR
      MARK=2*(MC-1)                     !MARK THE NEXT MIL-LOCATION
C
      MIL(MC)=22                        !LOAD OP-CODE
                                                   CALL QQ(0,22)
      MIL(MC+1)=2*(IP-1)                !LOAD TST PARM DISP
                                                   CALL QQ(1,2)
      MIL(MC+2)=2*(NLAA+1)+16           !LOAD MARK IN FAIL DISP LOC
                                                   CALL QQ(2,3)
      MIL(MC+3)=16                      !LOAD HIT DISP
                                                   CALL QQ(3,4)
      MIL(MC+4)=1                       !LOAD PARTIAL MISS INCREMENT
                                                   CALL QQ(4,5)
      MIL(MC+5)=-NLA                    !LOAD MINUS # OF GATES
                                                   CALL QQ(5,6)
      MCF=(MC+7)/2                      !FULL WORD INDEX IN MILF
      MILF(MCF)=2*(LCG-1)-MARK          !LOAD GATE-LIST DISP
                                                   CALL QQ(0,-7)
C
      MC=MC+7                           !INC  MIL-CNTR
C
      GO TO 420                         !GO OFF AND DO COMPUTED GOTO
C
C
C     **************************************************************
C     PROCESS  -  IFP(PARM)L1,L2,L3,L4............
C     **************************************************************
C
  500 IP=ITERV(JWD,IA,IB,IERR,MSER)     !PICK UP PARM # FOR IFX, IFN
      IF(IERR.NE.0) RETURN              !TST FOR ERROR
      IF(IP.LE.0.OR.IP.GT.NPAR) GO TO 1050 !TST FOR PARM # IN RANGE
C
      IRP=IB+1
      CALL LABLST(JWD,IRP,80,LABLS,NLA,IERR,MSER)
      IF(IERR.NE.0) RETURN
C
      NLAA=NLA
      IF(2*(NLAA/2).EQ.NLA) NLAA=NLA+1
C
C     **********************************!DO SIMPLE GATE ON TST-PARM
C
  510 ILO=0
      IHI=NLA-1
      MC=MC+1                           !INC  THE MIL-CNTR
C
      MIL(MC)=30                        !OPCODE - IN-LINE GATE
                                                   CALL QQ(0,30)
      MIL(MC+1)=2*(IP-1)                !PDISP
                                                   CALL QQ(1,2)
      MIL(MC+2)=2*(NLAA+1)+16           !CDISP - FAIL
                                                   CALL QQ(2,3)
      MIL(MC+3)=12                      !CDISP - HIT
                                                   CALL QQ(3,4)
C
      MIL(MC+4)=IHI                     !HI-LIMIT
                                                   CALL QQ(4,36)
      MIL(MC+5)=ILO                     !LO-LIMIT
                                                   CALL QQ(5,37)
C
      MC=MC+5                           !UPDATE MIL-CNTR
C
C     **********************************!LOAD TST PARM INTO ACC
C
      MC=MC+1                           !INC MIL-CNTR
      MIL(MC)=18                        !OPCODE FOR LOAD PARM
                                                   CALL QQ(0,18)
      MC=MC+1                           !INC MIL-CNTR
      MIL(MC)=2*(IP-1)                  !PDISP
                                                   CALL QQ(0,2)
C
C     **********************************!CODE FOR COMPUTED GOTO
C
      MC=MC+1                           !INC  MIL-CNTR
      MARK=2*(MC-1)                     !MARK NEXT MIL-LOCATION
      MIL(MC)=34                        !OPCODE FOR COMPUTED GOTO
                                                   CALL QQ(0,34)
C
      DO 520 I=1,NLA                    !LOOP ON # OF LABELS
      MC=MC+1                           !INC  MIL-CNTR
      MIL(MC)=MARK                      !STORE OPCODE LOCATION
                                                   CALL QQ(0,4)
      LPATI=LPATI+1                     !INC PATCH LOCATION CNTR
      LPATCH(LPATI)=MC                  !SAVE LOCATION TO PATCH
      LAPAT(LPATI)=LABLS(I)             !SAVE ASSOCIATED LABEL NAME
  520 CONTINUE
C
      IF(NLAA.EQ.NLA) RETURN            !TST FOR PAD REQUIRED
C
      MC=MC+1                           !INC MIL-CNTR
      MIL(MC)=0                         !LOAD PAD
                                                   CALL QQ(0,13)
      RETURN
C
C     **************************************************************
C     PROCESS  -  BTAB(P,MASK)LSOME,LNONE,LALL
C     **************************************************************
C
  600 IC=IFIND(JWD,z'2C',IA,IB)         !FIND COMMA
      IF(IC.LE.0) GO TO 1010            !ERROR IF NOT FOUND
      IP=ITERV(JWD,IA,IC-1,IERR,MSER)   !GET PARM VALUE
      IF(IERR.NE.0) RETURN              !TST FOR ERROR
      IF(IP.LE.0.OR.IP.GT.NPAR) GO TO 1020  !TST FOR LEGAL
      MASK=ITERV(JWD,IC+1,IB,IERR,MSER) !GET MASK
      IF(IERR.NE.0) RETURN              !TST FOR ERROR
C
  610 MC=MC+1                           !INC THE MIL-CNTR
      MIL(MC)=33                        !OPCODE FOR BIT-TEST
                                                   CALL QQ(0,33)
      MARK=2*(MC-1)                     !MARK OPCODE LOCATION
      MC=MC+1                           !INC MIL-CNTR
      MIL(MC)=2*(IP-1)                  !PDISP
                                                   CALL QQ(0,2)
C
      DO 620 I=1,3                      !LOAD THE 3 DESTINATIONS
      MC=MC+1                           !INC THE MIL-CNTR
      MIL(MC)=MARK                      !STORE OPCODE LOCATION
                                                   CALL QQ(0,I+38)
      LPATI=LPATI+1                     !INC PATCH LOCATION CNTR
      LPATCH(LPATI)=MC                  !SAVE LOCATION TO PATCH
      LAPAT(LPATI)=JWD(I+26)            !SAVE ASSOCIATED LABEL NAME
  620 CONTINUE
      MC=MC+1                           !INC THE MIL-CNTR
      MIL(MC)=MASK                      !MASK
                                                   CALL QQ(0,42)
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP MESSAGE
C     **************************************************************
C
 1010 JJ=1
      GO TO 1200
 1020 JJ=2
      GO TO 1200
 1030 JJ=3
      GO TO 1200
 1050 JJ=5
      GO TO 1200
 1070 JJ=7
      GO TO 1200
 1080 JJ=8
C
 1200 IERR=JJ
      DO 1210 I=1,10
      MSER(I)=MSG(I,JJ)
 1210 CONTINUE
      RETURN
      END
