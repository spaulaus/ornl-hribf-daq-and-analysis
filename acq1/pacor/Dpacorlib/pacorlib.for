C$PROG BOOLOD

C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE BOOLOD(IV)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXC=500)
C
      COMMON/PACK/ BOOLIST(MXC),NBOO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
      NBOO=NBOO+1
      IF(NBOO.GT.MXC) GO TO 100
C
      BOOLIST(NBOO)=IV
      RETURN
C
  100 WRITE(CMSSG,105)NBOO
  105 FORMAT('CALCULATED-GATE BOOLEAN TABLE OVERFLO AT ',I4)
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG CALLER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      IF(IDONE.NE.'YES ') CALL EQUATE(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL LWDMOD
C   
      IF(IDONE.NE.'YES ') CALL CONDX(IDONE,IERR)
C   
      RETURN
      END
C$PROG CANITQIT
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CANITQIT
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER 
C
      COMMON/PACB/ CNAFI(200),DATAI(200),NNAFI,
     &             CNAFQ(200),DATAQ(200),NNAFQ,
     &             CNAFR(200),DATAR(200),NNAFR
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      CHARACTER*4   CLWD(2,40)
C
      EQUIVALENCE (CMSSG,MSSG),(CLWD,LWD)
C
      INTEGER*4 IV(4),C,N,A,F,ICNAF
C
      BYTE BCNAF(4)
C
      CHARACTER*4  KMD
C
      EQUIVALENCE (KMD,IWD)
C
      EQUIVALENCE (BCNAF,ICNAF)
C
      EQUIVALENCE (C,IV(1)),(N,IV(2)),(A,IV(3)),(F,IV(4))
C
      DATA NNAFI,NNAFQ,NNAFR,MXNAF/0,0,0,200/
C
      SAVE
C
C     ************************************************************
C     PROCESS $INI, $DUN & $RUN DIRECTIVES - INIT & WRAPUP CNAFS
C     ************************************************************
C
      DO 50 I=1,4
      CALL MILV(LWD(1,I+2),IV(I),XV,KIND,IERR)
   50 CONTINUE
C
      JV=0
      IF(CLWD(1,7).EQ.'    '.AND.CLWD(2,7).EQ.'    ') GO TO 55
C
      CALL HEXVAL(LWD(1,7),JV,IERR)
C
   55 CALL CKFRITE(C,N,A,F,IERR)
      IF(IERR.NE.0) RETURN
C
      JJ=4
      DO 60 I=1,4
      BCNAF(I)=IV(JJ)
      JJ=JJ-1
   60 CONTINUE
C
      IF(KMD.EQ.'$DUN') GO TO 100
      IF(KMD.EQ.'$RUN') GO TO 200
C
C     ************************************************************
C     PROCESS $INI C,N,A,F,DATA
C     ************************************************************
C
      NNAFI=NNAFI+1
C
      IF(NNAFI.GT.MXNAF) THEN
      WRITE(CMSSG,65)NNAFI
   65 FORMAT('MAX# $INI CNAFs EXCEEDED AT NNAFI =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NNAFI=MXNAF
                         ENDIF
C
      CNAFI(NNAFI)=ICNAF
      DATAI(NNAFI)=IAND(JV,'FFFF'X)
      RETURN
C
C     ************************************************************
C     PROCESS $DUN C,N,A,F,DATA
C     ************************************************************
C
  100 NNAFQ=NNAFQ+1
C
      IF(NNAFQ.GT.MXNAF) THEN
      WRITE(CMSSG,105)NNAFQ
  105 FORMAT('MAX# $DUN CNAFs EXCEEDED AT NNAFQ =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NNAFQ=MXNAF
                         ENDIF
C
      CNAFQ(NNAFQ)=ICNAF
      DATAQ(NNAFQ)=IAND(JV,'FFFF'X)
      RETURN
C
C     ************************************************************
C     PROCESS $RUN C,N,A,F,DATA
C     ************************************************************
C
  200 NNAFR=NNAFR+1
C
      IF(NNAFR.GT.MXNAF) THEN
      WRITE(CMSSG,205)NNAFR
  205 FORMAT('MAX# $RUN CNAFs EXCEEDED AT NNAFR =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NNAFR=MXNAF
                         ENDIF
C
      CNAFR(NNAFR)=ICNAF
      DATAR(NNAFR)=IAND(JV,'FFFF'X)
      RETURN
      END
C$PROG CASEUP1
C
      SUBROUTINE CASEUP1(IBY)
C
      INTEGER*1 IBY(80)    !Was type BYTE on Alpha
C
      INTEGER*1 X61,X7A,X20
C
      DATA      X61,X7A,X20/'61'X,'7A'X,'20'X/
C
C     CONVERT COMMAND-PART ONLY TO UPPER-CASE (I.E. STOP AT 1ST BLANK)
C
      DO 20 I=1,80
      IT=IBY(I)
      IF(IT.EQ.X20) RETURN
      IF(IT.LT.X61.OR.IT.GT.X7A) GO TO 20
      IBY(I)=IT-X20
   20 CONTINUE
      RETURN
      END
C$PROG CKCAM
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKCAM(C,N,A,F,FLO,FHI,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
C     ************************************************************
C     CHECK CAMAC C,N,A,F VALUES FOR LEGAL
C     FLO,FHI GIVE MIN & MAX VALUES ALLOWED FOR FUNCTION-CODE
C     ************************************************************
C
      IERR=0
C
      IF(C.GE. 0.AND.C.LE. 7) GO TO 200
CX    IF(C.GE.10.AND.C.LE.17) GO TO 200
C
      WRITE(CMSSG,110)C
  110 FORMAT('ILLEGAL CAMAC CRATE#      =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
C
  200 IF(N.LT.1.OR.N.GT.31) THEN
      WRITE(CMSSG,210)N
  210 FORMAT('ILLEGAL CAMAC SLOT#       =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(A.LT.0.OR.A.GT.31) THEN
      WRITE(CMSSG,310)A
  310 FORMAT('ILLEGAL CAMAC SUB-ADDRESS =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(F.LT.FLO.OR.F.GT.FHI) THEN
      WRITE(CMSSG,410)F
  410 FORMAT('ILLEGAL CAMAC FUNC CODE   =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                               ENDIF
C
      RETURN
      END
C$PROG CKCLOCID  - Ckecks 100HZ clock parm-ID vs all others
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 01/06/2004
C     ******************************************************************
C
      SUBROUTINE CKCLOCID
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PACD/ FERID(32,576),FERC(576),FERN(576),FERT(576),NFER,
     &             FERERID
C
      COMMON/PACE/ FASID(256,32),FASC(32), FASN(32), FAST(32), NFAS,
     &             FASERID
C
      COMMON/PACN/ CAMID(32,32,8),CAMC(256),CAMN(256),CAMT(256),NCAM,
     &             CAMERID
C     ------------------------------------------------------------------
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/KLOC/ KLOCID(2)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(KLOCID(1).EQ.0) RETURN
C
      CALL CKVMEID(KLOCID(1))
C
      CALL CKVMEID(KLOCID(2))
C
      DO 100 I=1,2
C
      KLOCI=IAND(KLOCID(I),'7FFF'X)
C
C
      IF(KLOCID(I).EQ.FERERID) THEN
      WRITE(CMSSG,110)KLOCI
      CALL MESSLOG(LOGUT,LOGUP)
      NERR=NERR+1
      ENDIF
C
      IF(KLOCID(I).EQ.FASERID) THEN
      WRITE(CMSSG,120)KLOCI
      CALL MESSLOG(LOGUT,LOGUP)
      NERR=NERR+1
      ENDIF
C
      IF(KLOCID(I).EQ.CAMERID) THEN
      WRITE(CMSSG,130)KLOCI
      CALL MESSLOG(LOGUT,LOGUP)
      NERR=NERR+1
      ENDIF
C
  100 CONTINUE
C
  110 FORMAT('Error - Clock-ID =',I8,' = FERA error ID')
  120 FORMAT('Error - Clock-ID =',I8,' = FASTBUS error ID')
  130 FORMAT('Error - Clock-ID =',I8,' = CAMAC error ID')
C
      RETURN
C
      END
C$PROG CKCNAS
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKCNAS(NDX)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      CHARACTER*4  KIMO
C
      SAVE
C
C     ************************************************************
C     CHECK CAMAC, FASTBUS & FERA PARAMETERS FOR LEGAL IN CONTEXT
C     ************************************************************
C
      IF(KIMO(NDX).EQ.'$LAT') GO TO 100
      IF(KIMO(NDX).EQ.'$CAM') GO TO 100
      IF(KIMO(NDX).EQ.'$FER') GO TO 200
      IF(KIMO(NDX).EQ.'$FAS') GO TO 300
C
      RETURN
C
  100 CALL CKCAM(CRAT(NDX),SLOT(NDX),SUBA(NDX),FRED(NDX),0,7,ERR)
C
      IF(FCLR(NDX).LT.0) RETURN
C
      CALL CKCAM(CRAT(NDX),SLOT(NDX),ACLR(NDX),FCLR(NDX),8,31,ERR)
C
      RETURN
C
  200 CALL CKFER(CRAT(NDX),SLOT(NDX),SUBA(NDX),ERR)
      RETURN
C
  300 CALL CKFAS(CRAT(NDX),SLOT(NDX),SUBA(NDX),ERR)
      RETURN
C
      END
C$PROG CKFAS
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKFAS(C,N,A,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
C     ************************************************************
C     CHECK FASTBUS C,N,A VALUES FOR LEGAL
C     ************************************************************
C
      IERR=0
C
      IF(C.LT.1.OR.C.GT.1) THEN
      WRITE(CMSSG,110)C
  110 FORMAT('ILLEGAL FASTBUS CRATE#    =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                           ENDIF
C
      IF(N.LT.0.OR.N.GT.25) THEN
      WRITE(CMSSG,210)N
  210 FORMAT('ILLEGAL FASTBUS SLOT#     =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(A.LT.0.OR.A.GT.255)THEN
      WRITE(CMSSG,310)A
  310 FORMAT('ILLEGAL FASTBUS SUB-ADDR  =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      END
C$PROG CKFER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKFER(C,N,A,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
C     ************************************************************
C     CHECK FERA  C,N,A VALUES FOR LEGAL
C     ************************************************************
C
      IERR=0
C
      IF(C.GE. 0.AND.C.LE. 7) GO TO 200
      IF(C.GE.10.AND.C.LE.17) GO TO 200
C
      WRITE(CMSSG,110)C
  110 FORMAT('ILLEGAL FERRA CRATE#      =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
C
  200 IF(N.LT.1.OR.N.GT.31) THEN
      WRITE(CMSSG,210)N
  210 FORMAT('ILLEGAL FERRA SLOT#       =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(A.LT.0.OR.A.GT.31) THEN
      WRITE(CMSSG,310)A
  310 FORMAT('ILLEGAL FERRA SUB-ADDRESS =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      END
C$PROG CKFRITE
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKFRITE(C,N,A,F,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
C     ************************************************************
C     CHECK CAMAC C,N,A,F VALUES FOR LEGAL (REQUIRE F.GT.7)
C     ************************************************************
C
      IERR=0
C
      IF(C.GE. 0.AND.C.LE. 7) GO TO 200
      IF(C.GE.10.AND.C.LE.17) GO TO 200
C
      WRITE(CMSSG,110)C
  110 FORMAT('ILLEGAL CAMAC CRATE#      =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
C
  200 IF(N.LT.1.OR.N.GT.31) THEN
      WRITE(CMSSG,210)N
  210 FORMAT('ILLEGAL CAMAC SLOT#       =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(A.LT.0.OR.A.GT.15) THEN
      WRITE(CMSSG,310)A
  310 FORMAT('ILLEGAL CAMAC SUB-ADDRESS =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(F.LT.9.OR.F.GT.31) THEN
      WRITE(CMSSG,410)F
  410 FORMAT('WRITE ONLY SUPPORTED - ILLEGAL CAMAC FUNC CODE   =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      RETURN
      END
C$PROG CKMOTY
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ************************************************************
C
      SUBROUTINE CKMOTY(KINMOD,MOTYP)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      COMMON/PACO/ KODLO(4),KODHI(4)
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      CHARACTER*4  KINMOD
C
      SAVE
C
      IF(KINMOD.EQ.'$FER') GO TO 20
      IF(KINMOD.EQ.'$FAS') GO TO 40
      IF(KINMOD.EQ.'$CAM') GO TO 60
      IF(KINMOD.EQ.'$VME') GO TO 80
      RETURN
C
   20 IF(MOTYP.GE.KODLO(1).AND.MOTYP.LE.KODHI(1)) RETURN
C
      WRITE(CMSSG,25)
   25 FORMAT('ILLEGAL OR UNDEFINED FERRA MODULE TYPE')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
C
   40 IF(MOTYP.GE.KODLO(2).AND.MOTYP.LE.KODHI(2)) RETURN
C
      WRITE(CMSSG,45)
   45 FORMAT('ILLEGAL OR UNDEFINED FASTBUS MODULE TYPE')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
C
   60 IF(MOTYP.GE.KODLO(3).AND.MOTYP.LE.KODHI(3)) RETURN
      IF(MOTYP.EQ.0.OR.MOTYP.EQ.-1)   RETURN
C
      WRITE(CMSSG,65)
   65 FORMAT('ILLEGAL OR UNDEFINED CAMAC MODULE TYPE')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
C
   80 IF(MOTYP.GE.KODLO(4).AND.MOTYP.LE.KODHI(4)) RETURN
C
      WRITE(CMSSG,85)
   85 FORMAT('ILLEGAL OR UNDEFINED VME MODULE TYPE')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
C
      END
C$PROG CKNXTI
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKNXTI(NXTNDX,NDXNOW)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 NXTNDX(2),NDXNOW
C
      SAVE
C
C     ************************************************************
C     CHECKS TRUE & FALSE BRANCHES & DIS-ALLOWS BACKWARD BRANCHES 
C     ************************************************************
C
      IF(NXTNDX(1).LE.NDXNOW) THEN
      WRITE(CMSSG,10)
   10 FORMAT('ILLEGAL BACKWARD REFERENCE')
      CALL ERRLOG(LOGUT,LOGUP)
                              ENDIF
C
      IF(NXTNDX(2).LE.NDXNOW) THEN
      WRITE(CMSSG,10)
      CALL ERRLOG(LOGUT,LOGUP)
                              ENDIF
C
      RETURN
      END
C$PROG CKUNIQUE
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ******************************************************************
C
      SUBROUTINE CKUNIQUE(NUM)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
C     ------------------------------------------------------------------
      COMMON/PACV/ VADCMAP(10),VTDCMAP(10),VADCID(340),VTDCID(340)
      INTEGER*4    VADCMAP,    VTDCMAP,    VADCID,     VTDCID
C     ------------------------------------------------------------------
      INTEGER*4    X8000
      DATA         X8000/'8000'X/
C
      SAVE
C
C     ******************************************************************
C     CHECK VALUES REQUIRED TO BE UNIQUE
C     ******************************************************************
C
      ITST=IDNM(NUM)                 !PARAMETER-ID
      KTST=KIMO(NUM)                 !KIND OF MODULE
      CTST=CRAT(NUM)                 !CRATE#
      NTST=SLOT(NUM)                 !SLOT#
      ATST=SUBA(NUM)                 !SUB=ADDRESS
      MTST=MOTY(1,NUM)               !MODULE TYPE
C
      DO 50 N=1,NXIA
      IF(MTST.EQ.28)      GO TO 50
      IF(CTST.NE.XIAC(N)) GO TO 50
      IF(NTST.NE.XIAN(N)) GO TO 50
      WRITE(CMSSG,40)CTST,NTST
   40 FORMAT('MULTIPLE DEFINITION FOR: TYPE, C,N = ',2I8)
      CALL ERRLOG(LOGUT,LOGUP)
   50 CONTINUE
C
      NDO=NUM-1
      DO 110 N=1,NDO
      IF(KTST.NE.KIMO(N)) GO TO 110 
      IF(CTST.NE.CRAT(N)) GO TO 110
      IF(NTST.NE.SLOT(N)) GO TO 110
      IF(ATST.NE.SUBA(N)) GO TO 110
      WRITE(CMSSG,100)KTST,CTST,NTST,ATST
  100 FORMAT('MULTIPLE DEFINITION FOR: TYPE,C,N,A = ',A4,3I8)
      CALL ERRLOG(LOGUT,LOGUP)
  110 CONTINUE
C
      IF(ITST.EQ.-1) RETURN
C
      NDEF=0
      DO 120 N=1,NDO
      IF(ITST.EQ.IDNM(N)) NDEF=NDEF+1
  120 CONTINUE
C
      DO 130 I=1,340
      IF(VADCID(I).LE.0)    GO TO 130
      IF(ITST.NE.VADCID(I)) GO TO 130
      NDEF=NDEF+1
  130 CONTINUE
C
      DO 140 I=1,340
      IF(VTDCID(I).LE.0)    GO TO 140
      IF(ITST.NE.VTDCID(I)) GO TO 140
      NDEF=NDEF+1
  140 CONTINUE
C
      IF(NDEF.EQ.0) RETURN
C
      JTST=ITST-X8000
C
      WRITE(CMSSG,150)JTST,NDEF
  150 FORMAT('ID-NUMBER',I8,'    MULTIPLY DEFINED (',I5,' TIMES)')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG CKVMEID
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ******************************************************************
C
      SUBROUTINE CKVMEID(ID)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C     ------------------------------------------------------------------
      COMMON/PACV/ VADCMAP(10),VTDCMAP(10),VADCID(340),VTDCID(340)
      INTEGER*4    VADCMAP,    VTDCMAP,    VADCID,     VTDCID
C     ------------------------------------------------------------------
      INTEGER*4    X8000
      DATA         X8000/'8000'X/
C
      SAVE
C
C     ******************************************************************
C     CHECKS VME ID NUMBER FOR UNIQUE
C     ******************************************************************
C
      IF(ID.LE.0) RETURN
C
      NDEF=0
      DO 120 N=1,NUMT
      IF(ID.EQ.IDNM(N)) NDEF=NDEF+1
  120 CONTINUE
C
      DO 130 I=1,340
      IF(VADCID(I).LE.0)  GO TO 130
      IF(ID.NE.VADCID(I)) GO TO 130
      NDEF=NDEF+1
  130 CONTINUE
C
      DO 140 I=1,340
      IF(VTDCID(I).LE.0)  GO TO 140
      IF(ID.NE.VTDCID(I)) GO TO 140
      NDEF=NDEF+1
  140 CONTINUE
C
      IF(NDEF.EQ.0) RETURN
C
      JTST=ID-X8000
C
      WRITE(CMSSG,150)JTST,NDEF
  150 FORMAT('ID-NUMBER',I8,'    MULTIPLY DEFINED (',I5,' TIMES)')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG CLOCASS   - Processes Clock assignment statement
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 01/06/2004
C     ************************************************************
C
      SUBROUTINE CLOCASS(IWD)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC7/ISORL,IEXPL,LISTYP,NERR
C
      COMMON/KLOC/ KLOCID(2)
C     ------------------------------------------------------------------
      INTEGER*4    IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      INTEGER*4    X8000
      DATA         X8000/'8000'X/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
C
      IF(NTER.NE.0) GO TO 500
C
      CALL MILV(LWD(1,1),ITST,XV,NF,KIND,IERR)
C
      IF(IERR.NE.0)     GO TO 500
      IF(ITST.LE.0)     GO TO 500
      IF(ITST.GT.32767) GO TO 500
C
      CALL MILV(LWD(1,2),JTST,XV,NF,KIND,IERR)
C
      IF(IERR.NE.0)     GO TO 500
      IF(JTST.LE.0)     GO TO 500
      IF(JTST.GT.32767) GO TO 500
C
      IF(ITST.EQ.JTST)  GO TO 510
C
      KLOCID(1)=ITST+X8000
      KLOCID(2)=JTST+X8000
C
      RETURN
C
C     ------------------------------------------------------------------
C     Error return
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error or illegal value in clock-ID specification')
      GO TO 600
C
  510 WRITE(CMSSG,515)ITST,JTST
  515 FORMAT('Specified clock IDs',2I8,' are the same - not allowed')
      GO TO 600
C
  600 CALL MESSLOG(LOGUT,LOGUP)
      NERR=NERR+1
      RETURN
      END
C
C$PROG CLRADD
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CLRADD(TC,TN,TA,TF,C,N,A,F,NUM)
C
      INTEGER*4 C(*),N(*),A(*),F(*),TC,TN,TA,TF,NUM
C
C
      SAVE
C
C     ************************************************************
C     ADD ENTRIES TO THE CAMAC MODULE CLEAR-LIST 
C     ************************************************************
C
      DO 100 I=1,NUM
      IF(TC.EQ.C(I).AND.TN.EQ.N(I)) RETURN
  100 CONTINUE
C
      NUM=NUM+1
      C(NUM)=TC
      N(NUM)=TN
      A(NUM)=TA
      F(NUM)=TF
      RETURN
      END
C$PROG CNTADD
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CNTADD(CTST,NTST,TTST,C,N,T,NUM)
C
      INTEGER*4 CTST,NTST,TTST,C(*),N(*),T(*),NUM
C
      SAVE
C
C     ************************************************************
C     ADD ENTRIES TO FASTBUS & FERA MODULE TABLES
C     ************************************************************
C
      DO 100 I=1,NUM
      IF(CTST.EQ.C(I).AND.NTST.EQ.N(I)) RETURN
  100 CONTINUE
C
      NUM=NUM+1
      C(NUM)=CTST
      N(NUM)=NTST
      T(NUM)=TTST
C
      RETURN
      END
C$PROG CNTDOWN
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE CNTDOWN(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXG=100)
      PARAMETER (MXD=100)
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/PACL/ CDPAT(MXD),CDMSK(MXD),CDCNT(MXD),NCDN
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      INTEGER*4 GATNAM(4)
C
      REAL*4 XV
C
      DATA SETW,NCDN/'SETW',0/
C
      SAVE
C
C     ************************************************************
C     DEFINITIONS FOR COMMON/PACL/ - KTH ENTRY
C     ************************************************************
C     CDPAT(K) = ASSOTIATED PATTERN WORD#
C     CDMSK(K) = ASSOCIATED MASK
C     CDCNT(K) = COUNT VALUE
C     NCDN     = NO. OF ENTRIES
C
C     ************************************************************
C
C     PROCESS $CDN  GATENAME(I)  COUNT
C
C     ************************************************************
C
      IF(NCDN.GE.MXD) GO TO 510                !TSTFR TABLE OVER
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER) !SET GREAD WIDTH=12
C
      CALL GREAD(IWD,LWD,ITYP,NF,6,80,NTER)    !REFORMAT LINE
C
      IF(NTER.NE.0) GO TO 520                  !TSTFR ERROR
C
      CALL MILV3(LWD(1,2),GDX,XV,KIND,IERR)    !GATE-NAME INDEX
      IF(IERR.NE.0) GO TO 530
      CALL MILV3(LWD(1,3),CNT,XV,KIND,IERR)    !COUNT
      IF(IERR.NE.0) GO TO 530
C
      DO 20 I=1,3
      GATNAM(I)=LWD(I,1)                       !LOAD GATE-NAME
   20 CONTINUE
      GATNAM(4)=GDX                            !LOAD GATE-NAME INDEX
C
      CALL NAMLOC(GNAM,GATNAM,NGAT,NDX)        !TSTFR EXIST
      IF(NDX.LE.0) GO TO 560
C
      NCDN=NCDN+1                              !INC COUNT-DOWN INDEX
C
      CDPAT(NCDN)=PATN(NDX)                    !LOAD PATTERN WD#
      CDMSK(NCDN)=GMSK(NDX)                    !LOAD MASK
      CDCNT(NCDN)=CNT                          !LOAD COUNT
      GO TO 1000
C
C     ************************************************************
C     SEND ANY ERROR MESSAGES
C     ************************************************************
C
  510 WRITE(CMSSG,515)NCDN
  515 FORMAT('COUNT-DOWN TABLE OVERFLOW AT - ',I6)
      GO TO 800
  520 WRITE(CMSSG,525)
  525 FORMAT('TRUNCATION ERROR FROM - GREAD')
      GO TO 800
  530 WRITE(CMSSG,535)
  535 FORMAT('SYNTAX ERROR FROM MILV3')
      GO TO 800
  560 WRITE(CMSSG,565)GATNAM
  565 FORMAT('COUNT-DOWN GATE-NAME NOT FOUND = ',3A4,I4)
      GO TO 800
C
  800 CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)    !RESET WIDTH TO 8
      RETURN
      END
C$PROG CONDICO
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CONDICO
C
      PARAMETER (MXI=2000)
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C 
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PACA/ JCRA(MXI),JSLO(MXI),JSUB(MXI),JFRE(MXI),
     &             JIDN(MXI),NNAF
C     
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 KIN(13)
C
      INTEGER*4 CC(200),NN(200),AA(200),FF(200),DD(200)
C
      CHARACTER*4  INS,INST,CKIN(2)
C
      EQUIVALENCE (INS,KIN(2)),(CKIN,KIN)
C
      DATA NCI,ITY,NCNAF/0,0,0/
C
      INTEGER*4   BLANK
      DATA        BLANK/'    '/
C
      SAVE
C
C     ************************************************************
C     READS TMP-FILE WRITTEN BY CONDX (RESULTING FROM LOOP
C     EXPANSION, ETC) AND GENERATES FINAL CONDITIONAL READOUT 
C     TABLES 
C     ************************************************************
C
      DO 10 I=1,MXI
      LABL(I)  =BLANK
      INST(I)  ='    '
      NAMD(1,I)=BLANK
      NAMD(2,I)=BLANK
      NAMD(3,I)=BLANK
      IDES(I)  =BLANK
      INDX(I)=0
      MSKI(I)=0
      NREA(I)=0
      ILOR(I)=0
      IHIR(I)=0
   10 CONTINUE
C
      REWIND 8
C
   50 IF(NCI.GE.MXI) THEN
      WRITE(CMSSG,55)NCI
   55 FORMAT('/PAC5/ ARRAY OVERFLOW AT NCI =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NCI=MXI-1
                      ENDIF
C
      READ(8,60,END=1000)KIN,ISORL,IEXPL
   60 FORMAT(2(A4,2X),3A4,2X,4(2A4,2X),2I6)
C
      IF(INS.EQ.'IFN ') GO TO 200
      IF(INS.EQ.'IFA ') GO TO 200
C
      IF(INS.EQ.'IFU ') GO TO 300
      IF(INS.EQ.'IFS ') GO TO 300
      IF(INS.EQ.'IFT ') GO TO 300
      IF(INS.EQ.'IFF ') GO TO 300
C
      IF(INS.EQ.'GOTO') GO TO 400
      IF(INS.EQ.'CONT') GO TO 500
      IF(INS.EQ.'READ') GO TO 600
      IF(INS.EQ.'CNAF') GO TO 700
                        GO TO 50
C
  200 NCI=NCI+1               !IFN,  IFA
      JSORL(NCI) =ISORL
      JEXPL(NCI) =IEXPL
      LABL(NCI)  =KIN(1)
      INST(NCI)  =CKIN(2)
      NAMD(1,NCI)=KIN(3)
      NAMD(2,NCI)=KIN(4)
      NAMD(3,NCI)=KIN(5)
      INDX(NCI)  =ISVAL(KIN(6),ITY,IERR)
      MSKI(NCI)  =ISVAL(KIN(8),ITY,IERR)
      IDES(NCI)  =KIN(12)
      GO TO 50
C
  300 NCI=NCI+1               !IFU,  IFS
      JSORL(NCI) =ISORL
      JEXPL(NCI) =IEXPL
      LABL(NCI)  =KIN(1)
      INST(NCI)  =CKIN(2)
      NAMD(1,NCI)=KIN(3)
      NAMD(2,NCI)=KIN(4)
      NAMD(3,NCI)=KIN(5)
      INDX(NCI)  =ISVAL(KIN(6),ITY,IERR)
      IDES(NCI)  =KIN(10)
      GO TO 50
C
  400 NCI=NCI+1               !GOTO
      JSORL(NCI)=ISORL
      JEXPL(NCI)=IEXPL
      LABL(NCI)=KIN(1)
      INST(NCI)=CKIN(2)
      IDES(NCI)=KIN(3)
      GO TO 50
C
  500 NCI=NCI+1               !CONT
      JSORL(NCI)=ISORL
      JEXPL(NCI)=IEXPL
      LABL(NCI)=KIN(1)
      INST(NCI)=CKIN(2)
      GO TO 50
C
  600 NCI=NCI+1               !READ
      JSORL(NCI)=ISORL
      JEXPL(NCI)=IEXPL
      INST(NCI) =CKIN(2)
      NAMD(1,NCI)=KIN(3)
      NAMD(2,NCI)=KIN(4)
      NAMD(3,NCI)=KIN(5)
      INDX(NCI)  =ISVAL(KIN(6),ITY,IERR)
      GO TO 50
C
  700 NCI=NCI+1               !CNAF
      JSORL(NCI)=ISORL
      JEXPL(NCI)=IEXPL
      INST(NCI)=CKIN(2)
      NCNAF=NCNAF+1
C
      IF(NCNAF.GT.200) THEN
      WRITE(CMSSG,705)NCNAF
  705 FORMAT('MORE THAN 200 CNAF ENTRIES ENTERED - NOT SUPPORTED')
      CALL ERRLOG(LOGUT,LOGUP)
      NCNAF=200
                       ENDIF
C 
      INDX(NCI)=NCNAF
      CC(NCNAF)=ISVAL(KIN(3), ITY,IERR)
      NN(NCNAF)=ISVAL(KIN(6), ITY,IERR)
      AA(NCNAF)=ISVAL(KIN(8), ITY,IERR)
      FF(NCNAF)=ISVAL(KIN(10),ITY,IERR)
      DD(NCNAF)=ISVAL(KIN(12),ITY,IERR)
      M =NCNAF
      CALL CKFRITE(CC(M),NN(M),AA(M),FF(M),IERR)
      GO TO 50
C
 1000 NR=0
      DO 1010 I=1,NCI
      NREA(I)=0
C
      IF(INST(I).EQ.'READ') THEN
      NR=NR+1
      NREA(I)=NR
      CALL CONNEC(NAMD(1,I),INDX(I),JC,JN,JA,JF,JI)
C
      JCRA(NR)=JC
      JSLO(NR)=JN
      JSUB(NR)=JA
      JFRE(NR)=JF
      JIDN(NR)=JI
                            ENDIF
C
      IF(INST(I).EQ.'CNAF') THEN
      NR=NR+1
      NREA(I)=NR
      NDX=INDX(I)
      JCRA(NR)=CC(NDX)
      JSLO(NR)=NN(NDX)
      JSUB(NR)=AA(NDX)
      JFRE(NR)=FF(NDX)
      JIDN(NR)=DD(NDX)
                            ENDIF
C
 1010 CONTINUE
      NNAF=NR
C
      N=0
 1020 N=N+1
      IF(N.GT.NCI) GO TO 1200
      ILOR(N)=0
      IHIR(N)=0
C
      IF(INST(N).EQ.'READ'.OR.INST(N).EQ.'CNAF')    GO TO 1020
C
      NT=N+1
      IF(INST(NT).NE.'READ'.AND.INST(NT).NE.'CNAF') GO TO 1020
      ILOR(N)=NREA(NT)
      IHIR(N)=NREA(NT)
C
 1030 NT=NT+1
      IF(INST(NT).NE.'READ'.AND.INST(NT).NE.'CNAF') THEN
                                                    N=NT-1
                                                    GO TO 1020
                                                    ENDIF
      IHIR(N)=NREA(NT)
      GO TO 1030
C
 1200 CALL LABLMAN('SAVE',IDUM,IDUM,IERR)
C
      CALL CONTAB
C
      RETURN
      END
C$PROG CONDX
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CONDX(IDONE,IERR)
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      BYTE IBY(80)
C
      INTEGER*4     LWD3(3,40)
C
      CHARACTER*4  CLWD3(3,40),CLWD(2,40),CIwD(20)
C
      EQUIVALENCE (CLWD3,LWD3),(CLWD,LWD),(CIWD,IWD)
C
      EQUIVALENCE (IBY,IWD)
C
      DATA ISETW,NCALL/'SETW',0/
C
      CHARACTER*4  IDONE
C
      SAVE
C
C     ************************************************************
C     WRITES LOOP-EXPANDED & RE-FORMATTED CONDITIONAL READOUT CODE
C     ON TMP-FILE FOR SUBSEQUENT PROCESSING BY CONDICO 
C     ************************************************************
C
      IF(NCALL.GT.0) GO TO 50
C
      WRITE(8,10)ISORL,IEXPL                    !STUFF IN CONTINUE
   10 FORMAT(6X,'CONT',56X,2I6)                 !AT BEGINNING
      NCALL=1
C
   50 IERR=0
C
      CALL GREAD(IWD,LWD3,ITYP,NF,ISETW,12,NTER)
C
      CALL GREAD(IWD,LWD3,ITYP,NF,7,80,NTER)
C
      CALL GREAD(IWD,LWD3,ITYP,NF,ISETW,8,NTER)
C
      IF(CLWD3(1,1).EQ.'ENDL') THEN
                               CLWD3(1,1)='CONT'
                               CLWD3(2,1)='    '
                               ENDIF
      IF(CLWD3(1,1).EQ.'LOOP') THEN
                               CLWD3(1,1)='CONT'
                               CLWD3(2,1)='    '
                               ENDIF
      IF(CLWD3(1,1).EQ.'CONT') CLWD3(2,1)='    '
C
      IF(CIWD(1).EQ.'    ')    GO TO 100
      IF(CLWD3(1,1).NE.'READ') GO TO 100

      WRITE(8,60)IWD(1),ISORL,IEXPL
   60 FORMAT(A4,2X,'CONT',56X,2I6)
      CIWD(1)='    '
C 
  100 IF(CLWD(1,1).NE.'CNAF') GO TO 110
      CALL LWDMOD
      WRITE(8,105)IWD(1),LWD(1,1),((LWD(I,J),I=1,2),J=2,6),
     &                                          ISORL,IEXPL
  105 FORMAT(2(A4,2X),2A4,6X,4(2A4,2X),2I6)
      GO TO 500
C
  110 IF(CLWD(1,1).NE.'    ') THEN
C
      CALL LWDMOD3(LWD3)
C
      IF(CLWD3(3,3).NE.'    ') THEN
                               CLWD3(1,3)=CLWD3(2,3)
                               CLWD3(2,3)=CLWD3(3,3)
                               ENDIF
C
      IF(CLWD3(3,4).NE.'    ') THEN
                               CLWD3(1,4)=CLWD3(2,4)
                               CLWD3(2,4)=CLWD3(3,4)
                               ENDIF 
C
      WRITE(8,120)IWD(1),LWD3(1,1),
     &                   (LWD3(I,2),I=1,3),
     &                  ((LWD3(I,J),I=1,2),J=3,6),
     &                    ISORL,IEXPL
                             ENDIF
  120 FORMAT(2(A4,2X),3A4,2X,4(2A4,2X),2I6)
C
  500 IDONE='YES '
      RETURN
      END
C$PROG CONNEC
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CONNEC(NAME,IDX,JC,JN,JA,JF,JI)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C     
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 NAME(3)
C
      CHARACTER*4  KIMO,USED
C
      SAVE
C
C     ************************************************************
C     MAKES THE CONNECTION BETWEEN MODULE NAME(INDX) & C,N,A,F
C     ************************************************************
C
      DO 20 J=1,NUMT
      DO 10 I=1,3
      IF(NAME(I).NE.NAMO(I,J))                    GO TO 20
   10 CONTINUE
      IF(IDX.NE.NAMO(4,J))                        GO TO 20
      IF(KIMO(J).NE.'$CAM'.AND.KIMO(J).NE.'$LAT') GO TO 20
      GO TO 50
   20 CONTINUE
      GO TO 100
C
   50 JC=CRAT(J)
      JN=SLOT(J)
      JA=SUBA(J)
      JF=FRED(J)
      JI=IDNM(J)
      USED(J)='YES '
      RETURN
C
  100 WRITE(CMSSG,105)NAME,IDX
  105 FORMAT('MODULE NAME UNDEFINED = ',3A4,'(',I4,')')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG CONTAB
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CONTAB
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXI=2000)
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C
      COMMON/PAC6/ LABLIS(2,MXI),LABVAL(MXI),NLA
C 
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PAC4/ LATN(MXI),MASK(MXI),KINS(MXI),
     &             TBLI(2,MXI),TBLN(2,MXI),NXTI(2,MXI),
     &             NUNDX(MXI),NTS
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      CHARACTER*4  INST,KINS,IT
C
      SAVE
C
C     ************************************************************
C     CONSTRUCTS FINAL "CONDITIONAL READOUT TABLES"
C     ************************************************************
C
C     ---------------------------------------------------------------------
C     LABL(J)    = JTH STATEMENT LABEL 
C     INST(J)    = JTH INSTRUCTION - IFU,IFS,IFA,IFN,CONT,GOTO
C     NAMD(J)    = JTH DEVICE NAME - ADC1, TDC2, ETC
C     INDX(J)    = JTH DEVICE NAME INDEX
C     MSKI(J)    = JTH MASK FOR IF-TESTS
C     IDES(J)    = JTH DESTINATION (LABEL)
C     NREA(J)    = JTH READOUT LIST POINTER  - ASSOCIATED WITH "READ"
C     ILOR(J)    = JTH READOUT LIST LO-LIMIT - ASSOCIATED WITH "CONDITION"
C     IHIR(J)    = JTH READOUT LIST HI-LIMIT - ASSOCIATED WITH "CONDITION"
C     NCI        = NUMBER OF ENTRIES IN THIS INTERMEDIATE READOUT TABLE
C     ---------------------------------------------------------------------
C     LABLIS(1,J) = 1ST 4 BYTES OF JTH STATEMENT LABEL (ASCII)
C     LABLIS(2,J) = 2ND 4 BYTES OF JTH STATEMENT LABEL (ASCII)
C     LABVAL(J)   = VALUE OF       JTH STATEMENT LABEL (INTEGER)
C     ---------------------------------------------------------------------
C     LATN(J)    = LATCH-WORD INDEX FOR JTH TEST
C     MASK(J)    = MASK FOR USE IN      JTH TEST
C
C     TBLI(I,J)  = POINTS TO NAF-LIST (/PACA/)- I=1,2 for TRUE/FALSE 
C     TBLN(I,J)  = # NAFs TO TAKE FROM TABLE  - I=1,2 for TRUE/FALSE
C     NXTI(I,J)  = NEXT TEST-TABLE INDEX      - I=1,2 for TRUE/FALSE
C
C     NTS        = NUMBER OF TEST-TABLE ENTRIES TO USE
C     --------------------------------------------------------------------- 
C
      NTS=0
      N=0
C
  100 N=N+1
      IF(N.GT.NCI) GO TO 1000
      ISORL=JSORL(N)
      IEXPL=JEXPL(N)
      IT=INST(N)
      KINS(NTS+1)=IT
C
      IF(NTS.GT.MXI-1) THEN
      WRITE(CMSSG,105)NTS+1
  105 FORMAT('/PAC4/ ARRAY OVERFLOW AT NTS =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NTS=MXI-1
                       ENDIF
C
      IF(IT.EQ.'CONT') GO TO 200
      IF(IT.EQ.'GOTO') GO TO 220
      IF(IT.EQ.'IFN ') GO TO 240
      IF(IT.EQ.'IFA ') GO TO 260
      IF(IT.EQ.'IFU ') GO TO 280
      IF(IT.EQ.'IFS ') GO TO 300
      IF(IT.EQ.'IFF ') GO TO 320
      IF(IT.EQ.'IFT ') GO TO 340
                       GO TO 100
C
  200 NTS=NTS+1                              !CONTINUE ***************
      LATN(NTS)=0                            !LATCH-WD-0 PRESET
      MASK(NTS)=-1                           !MASK=-1
      TBLI(1,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=ILOR(N)                    !LO CAMAC-LST-PRT - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS 
      TBLN(1,NTS)=NR                         !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=NR                         !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=NTS+1                      !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NTS+1                      !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  220 NTS=NTS+1                              !GOTO *******************
      LATN(NTS)=1                            !LATCH-WD-1 PRESET
      MASK(NTS)=0                            !MASK=0
      TBLI(1,NTS)=0                          !NO CAMAC LIST    - TRUE
      TBLI(2,NTS)=0                          !NO CAMAC LIST    - FALSE
      TBLN(1,NTS)=0                          !#  CAMAC OPS=0   - TRUE
      TBLN(2,NTS)=0                          !#  CAMAC OPS=0   - FALSE
      NXTI(1,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NXTI(1,NTS)                !NEST TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  240 NTS=NTS+1                              !IFN ********************
      LATN(NTS)=LATWDX(NAMD(1,N),INDX(N))    !LATWD# FROM NAMD(INDX)
      MASK(NTS)=MSKI(N)                      !MASK
      TBLI(1,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=0                          !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=NR                         !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=0                          !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=NTS+1                      !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  260 NTS=NTS+1                              !IFA ********************
      LATN(NTS)=LATWDX(NAMD(1,N),INDX(N))    !LATWD# FROM NAMD(INDX)
      MASK(NTS)=MSKI(N)                      !MASK
      TBLI(1,NTS)=0                          !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=0                          !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=NR                         !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NTS+1                      !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  280 NTS=NTS+1                              !IFU ********************
      CALL PATMSK(NAMD(1,N),INDX(N),         !FROM NAME & INDEX, GET
     &            LATN(NTS),MASK(NTS))       !LATCH-WD# & MASK
      TBLI(1,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=0                          !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=NR                         !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=0                          !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=NTS+1                      !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  300 NTS=NTS+1                              !IFS ********************
      CALL PATMSK(NAMD(1,N),INDX(N),         !FROM NAME & INDEX, GET
     &            LATN(NTS),MASK(NTS))       !LATCH-WD# & MASK
      TBLI(1,NTS)=0                          !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=0                          !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=NR                         !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NTS+1                      !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  320 NTS=NTS+1                              !IFF ********************
      CALL GATMSK(NAMD(1,N),INDX(N),         !FROM NAME & INDEX, GET
     &            LATN(NTS),MASK(NTS))       !LATCH-WD# & MASK
      TBLI(1,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=0                          !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=NR                         !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=0                          !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=NTS+1                      !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  340 NTS=NTS+1                              !IFT ********************
      CALL GATMSK(NAMD(1,N),INDX(N),         !FROM NAME & INDEX, GET
     &            LATN(NTS),MASK(NTS))       !LATCH-WD# & MASK
      TBLI(1,NTS)=0                          !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=0                          !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=NR                         !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NTS+1                      !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
 1000 DO 1010 I=1,NTS-1
C
      IF(KINS(I).EQ.'CONT'
     &   .AND.TBLI(1,I).EQ.0
     &   .AND.TBLI(2,I).EQ.0) KINS(I)='SKIP'
 1010 CONTINUE
C
      NUDX=1
      DO 1015 I=1,NTS
      NUNDX(I)=NUDX
      IF(KINS(I).NE.'SKIP') NUDX=NUDX+1
 1015 CONTINUE
C
      DO 1020 I=1,NTS
      IF(NXTI(1,I).GT.0) NXTI(1,I)=NUNDX(NXTI(1,I))
      IF(NXTI(2,I).GT.0) NXTI(2,I)=NUNDX(NXTI(2,I))
 1020 CONTINUE
C
      N=0
      DO 1030 I=1,NTS
      IF(KINS(I).EQ.'SKIP') GO TO 1030
      N=N+1
      LATN(N)=LATN(I)
      MASK(N)=MASK(I)
      KINS(N)=KINS(I)
      TBLI(1,N)=TBLI(1,I)
      TBLI(2,N)=TBLI(2,I)
      TBLN(1,N)=TBLN(1,I)
      TBLN(2,N)=TBLN(2,I)
      NXTI(1,N)=NXTI(1,I)
      NXTI(2,N)=NXTI(2,I)
 1030 CONTINUE
      NTS=N
      RETURN
      END
C$PROG DELAYX
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/02/2001
C                               for CAEN support
C     ******************************************************************
C
      SUBROUTINE DELAYX
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/PACP/ DELAZ(30),NDELAZ
      INTEGER*4    DELAZ,    NDELAZ
      DATA         DELAZ/30*0/
      DATA         NDELAZ/30/
C     ------------------------------------------------------------------
      REAL*4       XV
C
      INTEGER*4    DLAVAL,KIND,IERR,IV,I,NDX
C
      INTEGER*4    DLANDX(7)
C
      CHARACTER*8  DLATYP,DLALIS(7)
C
      EQUIVALENCE (DLATYP,LWD(1,2))
C
      DATA         DLANDX/7,13,14,17,18,19,26/
C
      DATA         DLALIS/'LATCH   ',
     &                    'UNCONDIT',
     &                    'CONDIT  ',
     &                    'CAMAC   ',
     &                    'FASTBUS ',
     &                    'FERA    ',
     &                    'VME     '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 20 I=1,7
      NDX=I
      IF(DLATYP.EQ.DLALIS(I)) GO TO 50
   20 CONTINUE
      GO TO 500
C
   50 CALL MILV(LWD(1,3),IV,XV,KIND,IERR)
      IF(IERR.NE.0)            GO TO 510
      IF(IV.LT.0.OR.IV.GT.255) GO TO 520
      NDX=DLANDX(NDX)
      DELAZ(NDX)=IV
      RETURN
C
C     ------------------------------------------------------------------
C     Error returns
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)DLATYP
  505 FORMAT('Illegal delay-type specifier = ',A)
      GO TO 1000
C
  510 WRITE(CMSSG,515)
  515 FORMAT('Syntax error in delay specification')
      GO TO 1000
C
  520 WRITE(CMSSG,525)IV
  525 FORMAT(I8,' is an illegal delay value - 0-255 is legal range')
      GO TO 1000
C
 1000 CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END

C$PROG EQUATE
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE EQUATE(IDONE,IERR)
C   
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4     JWD(20)
      CHARACTER*4  CJWD(20)
      EQUIVALENCE (CJWD,JWD)
C   
      DATA IEQL,ISEM/'3D'X,'3B'X/
C
      CHARACTER*4  IDONE
C
      SAVE
C   
C     ************************************************************
C     PROCESS - SYM=EXPRESSION (EVALUATES EXPRESSION & SAVES SYM)
C     ************************************************************
C
      IERR=0
      IDONE='    '
C   
      DO 10 I=1,20
      JWD(I)=IWD(I)
   10 CONTINUE
C
      LEQ=IFIND(JWD,IEQL,7,80)
      IF(LEQ.LE.0) RETURN
C
      IF(CJWD(1).NE.'    ') GO TO 110
C   
      IA=NXNB(JWD,LEQ+1,80)
      IF(IA.LE.0)  GO TO 100
      CALL SQUEZL(JWD,IA,80)
      IB=LSNB(JWD,IA,80)
      IF(IB.LE.0)  GO TO 100
      IV=KVALU(JWD,IA,IB,IERR)
      IF(IERR.NE.0)GO TO 100
C   
      KSYM=LWD(1,1)
      CALL SYMSAV(KSYM,IV,IERR)
      IDONE='YES '
      RETURN
C   
  100 WRITE(CMSSG,105)
  105 FORMAT('SYNTAX ERROR IN SYMBOL DEFINITION')
      GO TO 200
  110 WRITE(CMSSG,115)
  115 FORMAT('LABEL NOT ALLOWED ON SYMBOL DEFINITION STATEMENT')
C
  200 CALL ERRLOG(LOGUT,LOGUP)
      IDONE='YES '
      IERR=1
      RETURN
      END
C$PROG ERRLOG
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      SUBROUTINE ERRLOG(LUA,LUB)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      INTEGER*4 MSS15(15),MSS20(20)
C
      EQUIVALENCE (MSS15(1),MSSG(1)),(MSS20(1),MSSG(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(LUA.LE.0) GO TO 20
C
      WRITE(LUA,10)MSS15,ISORL,IEXPL
   10 FORMAT(1H ,15A4,' SOR,EXP=',2I5)
C
   20 IF(LUB.LE.0.OR.LISFLG.NE.'LON ') GO TO 100
C
      WRITE(LUB,30)MSS20,ISORL,IEXPL
   30 FORMAT(1H ,20A4,'at SOR-,EXP-LINE#=',2I5)
C
  100 CMSSG=' '
C
      NERR=NERR+1
C
      RETURN
      END
C$PROG ERRMSG
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      SUBROUTINE ERRMSG(LUA,LUB)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      INTEGER*4 MSS15(15),MSS20(20)
C
      EQUIVALENCE (MSS15(1),MSSG(1)),(MSS20(1),MSSG(1))
C
      SAVE
C     ------------------------------------------------------------------
C
      IF(LUA.LE.0) GO TO 20
C
      WRITE(LUA,10)MSS15
   10 FORMAT(1H ,15A4)
C
   20 IF(LUB.LE.0.OR.LISFLG.NE.'LON ') GO TO 100
C
      WRITE(LUB,30)MSS20
   30 FORMAT(1H ,20A4)
C
  100 CMSSG=' '
C
      NERR=NERR+1
C
      RETURN
      END
C$PROG ERRORID
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE ERRORID
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER 
C
      COMMON/PACD/ FERID(32,576),FERC(576),FERN(576),FERT(576),NFER,
     &             FERERID
C
      COMMON/PACE/ FASID(256,32),FASC(32), FASN(32), FAST(32), NFAS,
     &             FASERID
C
      COMMON/PACN/ CAMID(32,32,8),CAMC(256),CAMN(256),CAMT(256),NCAM,
     &             CAMERID
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4    X8000
      DATA         X8000/'8000'X/
C
      SAVE
C
C     ************************************************************
C     DECODES - $DID FASERID,FERERID   ENTRY
C     ************************************************************
C
      CAMERID=0
      FASERID=0
      FERERID=0
C
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR) 
      IF(IERR.NE.0) GO TO 200
      CALL MILV(LWD(1,3),JV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
      CALL MILV(LWD(1,4),KV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
C
      IF(IV.LT.0.OR.IV.GT.32767) GO TO 210
      IF(JV.LT.0.OR.JV.GT.32767) GO TO 210
      IF(KV.LT.0.OR.KV.GT.32767) GO TO 210
C
      IF(IV.GT.0) IV=IV+X8000
      IF(JV.GT.0) JV=JV+X8000
      IF(KV.GT.0) KV=KV+X8000
C
      FASERID=IV
      FERERID=JV
      CAMERID=KV
      RETURN
C
  200 WRITE(CMSSG,205)
  205 FORMAT('ERROR DECODING FASTBUS, FERA, CAMAC ERROR ID ENTRY')
      GO TO 300
  210 WRITE(CMSSG,215)IV,JV,KV
  215 FORMAT('FASTBUS, FERA OR CAMAC, ERROR-ID OUT OF RANGE =',3I8)
  300 CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG FIELDER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE FIELDER(IWD,JA,JB,ILO,IHI,NF)
C
      INTEGER*4 IWD(*),ILO(*),IHI(*)
C
      SAVE
C
C     ************************************************************
C     FINDS CONTIGUOUS FIELDS IN IWD BETWEEN JA & JB
C     ************************************************************
C
      NF=0
      IB=JA
  100 IF(IB.GE.JB) GO TO 200
      IA=NXNB(IWD,IB,JB)
      IF(IA.LE.0) GO TO 200
      IB=NXBL(IWD,IA,JB)
      IF(IB.LE.0) IB=JB+1
      IB=IB-1
      NF=NF+1
      ILO(NF)=IA
      IHI(NF)=IB
      IB=IB+1
      GO TO 100
C
  200 RETURN
      END
C$PROG GATECMP

C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE GATECMP(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXG=100)
      PARAMETER (MXC=500)
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/PACK/ BOOLIST(MXC),NBOO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      INTEGER*4 RAWNAM(4)
C
      REAL*4 XV
C
      DATA SETW,NBOO/'SETW',0/
C
      INTEGER*4   CAL,YES,RAW,NOT,AND,OR
      DATA        CAL/'CAL '/
      DATA        YES/'YES '/
      DATA        RAW/'RAW '/
      DATA        NOT/'NOT '/
      DATA        AND/'AND '/
      DATA        OR /'OR  '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     DEFINITIONS FOR COMMON/PACK/ 
C     ------------------------------------------------------------------
C     BOOLIST - CONTAINS A BOOLEAN LIST FOR CALCULATED GATES
C     THE FORM IS:
C
C     PATNDX, MASK, NOTCODE, GATNDX, OPCODE, NOTCODE, GATNDX......
C     ENDCODE, PATNDX, MASK, ....
C
C     NBOO            = NO. OF WORDS STORED IN BOOLIST 
C     ************************************************************
C
C     PROCESS: $CGAT  CNAM(J) = <N>GNAM(I) OP <N>GNAM(J) ...
C
C     ************************************************************
C
      IF(NGAT.GE.MXG) GO TO 510                !TSTFR TABLE OVERFLO
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER) !SET GREAD-FIELD=12
C
      CALL GREAD(IWD,LWD,ITYP,NF,6,80,NTER)    !REFORMAT LINE
      IF(NTER.NE.0) GO TO 520
C
      CALL MILV3(LWD(1,2),GDX,XV,KIND,IERR)    !CAL GATE-NAME INDEX
      IF(IERR.NE.0) GO TO 500
C
      NGAT=NGAT+1                              !INC GATE COUNTER
      GTYP(NGAT)=CAL                           !SET GATE-TYPE=CAL
      LPTR(NGAT)=NBOO+1                        !PNT TO NEXT BOOL INDEX
      NENT(NGAT)=0                             !RESET # BOOL ENTRIES
      DO 20 I=1,3
      GNAM(I,NGAT) =LWD(I,1)                   !LOAD GATE-NAME
   20 CONTINUE
      GNAM(4,NGAT)=GDX                         !LOAD GATE-NAME INDEX
C
      IF(NGAT.GT.1) THEN
      CALL NAMLOC(GNAM,GNAM(1,NGAT),NGAT-1,IDX) !TSTFR NAME EXISTS
      IF(IDX.GT.0) GO TO 540                    !ERROR IF YES
                    ENDIF
C
      IF(LEGLNAM(GNAM(1,NGAT)).NE.YES) GO TO 550  !TSTFR LEGAL 
C
      CALL NEWMSK(PATNO,MSKNO)                  !GET NEW PATTERN WD#
C                                               !AND MASK
C
      PATN(NGAT)=PATNO                          !LOAD PATTERN WD#
      GMSK(NGAT)=MSKNO                          !LOAD MASK
C
C     ************************************************************
C     DECODE BOOLEAN EXPRESSION - <N>GNAM(I)  OP <N> GNAM(J) .....
C     ************************************************************
C
      JJ=3                                      !POINT TO 3RD FIELD
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF UNDEF
C
      BOOLOP=0                                  !RESET OP- WORD
      BOONOT=0                                  !RESET NOT-WORD
C
      IF(LWD(1,JJ).EQ.NOT) THEN                 !TSTFR NOT
      BOONOT=1                                  !IF YES, SET IT
      JJ=JJ+1                                   !INC FIELD PNTR
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF UNDEF
      ENDIF
C
      DO 30 I=1,3 
      RAWNAM(I)=LWD(I,JJ)                       !LOAD RAW GATE-NAME
   30 CONTINUE
      JJ=JJ+1                                   !INC FIELD PNTR
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF UNDEF
      CALL MILV3(LWD(1,JJ),IDX,XV,KIND,IERR)    !GET GATE-NAME INDX
      IF(IERR.NE.0) GO TO 500                   !TSTFR ERROR
      RAWNAM(4)=IDX                             !LOAD IT
C
      DO 40 I=1,NGAT                            !TSTFR RAWNAM EXISTS
      IF(GTYP(I).NE.RAW) GO TO 40
      IF(ISEQUL(RAWNAM,GNAM(1,I),4).EQ.YES) GO TO 50
   40 CONTINUE
      GO TO 560                                 !ERROR IF NON-EXIST
C
C                                               !OTHERWISE,
   50 CALL BOOLOD(PATNO+1)                      !LOAD PATTERN INDEX
                                                !START WITH NDX=2
      CALL BOOLOD(MSKNO)                        !LOAD MASK
      CALL BOOLOD(BOONOT)                       !LOAD NOTCODE
      CALL BOOLOD(I)                            !LOAD GATE-LIST NDX
      NENT(NGAT)=1                              !SET 1 BOOL ENTRY
C
C     ************************************************************
C     PROCESS ANY OTHER BOOLEAN FACTORS
C     ************************************************************
C
  100 JJ=JJ+1                                   !INC FIELD INDEX
      IF(JJ.GT.NF) THEN                         !TSTFR DONE
      CALL BOOLOD(-1)                           !IF YES, LOAD ENDCODE
      GO TO 1000
      ENDIF
C
      IF(LWD(1,JJ).EQ.AND) GO TO 105            !TSTFR OR- OPER
      IF(LWD(1,JJ).EQ.OR ) GO TO 110            !TSTFR AND-OPER
      GO TO 500                                 !ERROR IF NEITHER
C                                               !IF OK,
  105 BOOLOP=0                                  !AND-CODE
      GO TO 115
  110 BOOLOP=1                                  !OR -CODE
C
  115 JJ=JJ+1                                   !INC FIELD POINTER
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF OVERFLO
C
      BOONOT=0                                  !RESET NOT-CODE
      IF(LWD(1,JJ).EQ.NOT) THEN                 !TSTFR NOT-WORD
      BOONOT=1                                  !LOAD IF FOUND
      JJ=JJ+1                                   !INC FIELD INDEX
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF OVERFLO
      ENDIF
C
      DO 120 I=1,3
      RAWNAM(I)=LWD(I,JJ)                       !LOAD GATE-NAME           
  120 CONTINUE
      JJ=JJ+1                                   !INC FIELD INDEX
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF OVERFLO
      CALL MILV3(LWD(1,JJ),IDX,XV,KIND,IERR)    !DECODE GATE-NAME-INDEX
      IF(IERR.NE.0) GO TO 500                   !TSTFR ERROR
      RAWNAM(4)=IDX                             !LOAD INDEX IN RAWNAM
C
      DO 140 I=1,NGAT                           !TSTFR EXIST
      IF(GTYP(I).NE.RAW) GO TO 140
      IF(ISEQUL(RAWNAM,GNAM(1,I),4).EQ.YES) GO TO 150
  140 CONTINUE
      GO TO 560                                 !ERROR IF NON-EXIST
C
C                                               !OTHERWISE,
  150 CALL BOOLOD(BOOLOP)                       !LOAD OP- CODE
      CALL BOOLOD(BOONOT)                       !LOAD NOT-CODE
      CALL BOOLOD(I)                            !LOAD GATE-LIST NDX
      NENT(NGAT)=NENT(NGAT)+1                   !INC BOOL COUNTER
      GO TO 100
C
C     ************************************************************
C     SEND ANY ERROR MESSAGES
C     ************************************************************
C
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR IN CALCULATED GATE SPECIFICATION')
      GO TO 800
C
  510 WRITE(CMSSG,515)NGAT
  515 FORMAT('GATE TABLE OVERFLOW AT - ',I6)
      GO TO 800
  520 WRITE(CMSSG,525)
  525 FORMAT('TRUNCATION ERROR FROM - GREAD')
      GO TO 800
  540 WRITE(CMSSG,545)(GNAM(I,NGAT),I=1,4)
  545 FORMAT('MULTIPLE DEFINITION OF GATE NAME = ',3A4,I4)
      GO TO 800
  550 WRITE(CMSSG,555)GNAM(1,NGAT)
  555 FORMAT('ILLEGAL GATE NAME = ',A4)
      GO TO 800
  560 WRITE(CMSSG,565)RAWNAM
  565 FORMAT('RAW GATE-NAME NOT FOUND = ',3A4,I4)
      GO TO 800
C
  800 CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)    !RESET WIDTH TO 8
      RETURN
      END
C$PROG GATER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE GATER(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXG=100)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      CHARACTER*4  KIMO,GTYP,CALCALL
C
      INTEGER*4 PNAM(4)
C
      REAL*4 XV
C
      DATA NGAT,NGRED,NBOO,NCALL,NPATWD/0,0,0,0,0/
C
      DATA PATNO,MSKNO/0,0/
C
      DATA SETW/'SETW'/
C
      INTEGER*4   YES,CGA
      DATA        YES,CGA/'YES ','$CGA'/
C
      SAVE
C
C     ************************************************************
C     DEFINITIONS FOR COMMON/PACJ/ - FOR ITH GATE
C     ************************************************************
C     GNAM(M,I),M=1,3 = GATE NAME (ASCII) (RAW OR CALCULATED)
C     GNAM(4,I)       = GATE NAME INDEX
C     GTYP(I)         = GATE TYPE = 'RAW' OR 'CAL'
C     PATN(I)         = ASSOCIATED PATTERN WORD# 
C     GMSK(I)         = ASSOCIATED MASK
C
C     GLO(J)          = GATE LO-LIMIT
C     GHI(J)          = GATE HI-LIMIT
C     LPTR(I)         = POINTER TO AUX LIST, COMMON/PACK/ FOR CAL 
C     RPTR(I)         = POINTER TO READ   LIST (INDEX IN GCNAF)
C
C     MPTR(I)         = POINTER TO MODULE LIST (INDEX IN /PACJ/)
C     GCNAF(M,I),M=1,5= RAW-GATE READ-LIST (C,N,A,F,MOTY)        
C     NENT(I)         = # OF ENTRIES FOR /PACK/ DATA
C
C     NGAT            = TOTAL# (RAW + CAL) GATE ENTRIES
C     NGRED           = # GATE-PARMS TO READ (# ENTRIES IN GCNAF)
C
C     PATNO           = CURRENT PATTERN WORD# 
C     MSKNO           = CURRENT MASK (SPECIFIES BIT TO SET)
C
C     ************************************************************
C
      IF(NCALL.GT.0) GO TO 50
C
C     ************************************************************
C     INITIALIZE NPATWD (LAST PATTERN WORD# USED ON FIRST CALL)
C     NOTE: ALL INDICES ARE INCREMENTED BY 1 IN POBGEN
C     ************************************************************
C
      NPATWD=0
      DO 10 I=1,NUMT
      IF(KIMO(I).EQ.'$LAT') NPATWD=NPATWD+1
   10 CONTINUE
      PATNO=NPATWD
      MSKNO=32768
      NCALL=1
C
C     ************************************************************
C     TEST FOR RAW ($RGA) OR CALCULATED ($CGA) REQUEST
C     ************************************************************
C
   50 IF(IWD(1).EQ.CGA)  THEN
      CALL GATECMP(IWD)
      CALCALL='YES '
      RETURN
      ENDIF
C
C     ************************************************************
C     PROCESS $RGA REQUEST
C
C     $RGAT GNAM(I),PNAM(J),LO,HI
C     ************************************************************
C
      IF(CALCALL.EQ.'YES ') THEN
      WRITE(CMSSG,55)
   55 FORMAT('Calculated gate specs MUST follow raw gate specs')
      CALL ERRLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(NGAT.GE.MXG) GO TO 510               !TST FOR TABLE OVERFLO
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER)!SET FIELD WIDTH=12
C
      CALL GREAD(IWD,LWD,ITYP,NF,6,80,NTER)   !REFORMAT INPUT LINE
C
      IF(NTER.NE.0) GO TO 520                 !TST FOR ERROR
C
      CALL MILV3(LWD(1,2),GDX,XV,KIND,IERR)   !GATE-NAME INDEX   
      IF(IERR.NE.0) GO TO 530
      CALL MILV3(LWD(1,4),PDX,XV,KIND,IERR)   !PARM-NAME INDEX
      IF(IERR.NE.0) GO TO 530
      CALL MILV3(LWD(1,5),LO, XV,KIND,IERR)   !GATE LO-LIMIT
      IF(IERR.NE.0) GO TO 530
      CALL MILV3(LWD(1,6),HI, XV,KIND,IERR)   !GATE HI-LIMIT
      IF(IERR.NE.0) GO TO 530
C
      NGAT=NGAT+1                             !INC #GATES CNTR
      GTYP(NGAT)='RAW '                       !GATE-TYPE = RAW
      LPTR(NGAT)=0
      DO 110 I=1,3
      GNAM(I,NGAT)=LWD(I,1)                   !LOAD GATE-NAME
      PNAM(I)     =LWD(I,3)                   !LOAD PARM-NAME
  110 CONTINUE
      GNAM(4,NGAT)=GDX                        !LOAD GATE-INDEX
      PNAM(4)     =PDX                        !LOAD PARM-INDEX
      GLO(NGAT)   =LO                         !LOAD LO-LIMIT
      GHI(NGAT)   =HI                         !LOAD HI-LIMIT
C
      IF(NGAT.GT.1) THEN
      CALL NAMLOC(GNAM,GNAM(1,NGAT),NGAT-1,IDX)     !TST FOR MULT DEF
      IF(IDX.GT.0) GO TO 540
                    ENDIF
C
      IF(LEGLNAM(GNAM(1,NGAT)).NE.YES) GO TO 550 !TST FOR LEGAL
C
      CALL NAMLOC(NAMO,PNAM,NUMT,NDX)         !LOCATE PARM-NAME 
      IF(NDX.LE.0) GO TO 560
C
      MPTR(NGAT)=NDX                          !PARM-LIST INDEX
C
      CALL NEWMSK(PATNO,MSKNO)                !GET PAT-WD# & MASK
C
      PATN(NGAT)=PATNO                        !LOAD PATTERN WD#
      GMSK(NGAT)=MSKNO                        !LOAD MASK
C
C     ************************************************************
C     TEST FOR SPECIFIED PARAMETER ALREADY IN READ-LIST
C     ************************************************************
C
      NDO=NGAT-1                   !TST PREVIOUS PARM-LIST PNTRS
      DO 130 I=1,NDO               !FOR MATCHING CURRENT PARM-INDEX
      IF(NDX.EQ.MPTR(I)) THEN      !IF MATCH, THEN USE SAME 
      RPTR(NGAT)=RPTR(I)           !READ-LIST POINTER
      GO TO 1000                   !AND DON'T ADD TO READ-LIST
      ENDIF
  130 CONTINUE
C
      NGRED=NGRED+1                !IF NO MATCH, INC READ-LIST PNTR
      RPTR(NGAT)=NGRED             !LOAD READ-LIST POINTER
      GCNAF(1,NGRED)=CRAT(NDX)     !LOAD C
      GCNAF(2,NGRED)=SLOT(NDX)     !LOAD N
      GCNAF(3,NGRED)=SUBA(NDX)     !LOAD A
      GCNAF(4,NGRED)=FRED(NDX)     !LOAD F
      GCNAF(5,NGRED)=MOTY(1,NDX)   !LOAD MODULE-TYPE CODE

C
      GO TO 1000
C
C     ************************************************************
C     SEND ANY ERROR MESSAGES
C     ************************************************************
C
  510 WRITE(CMSSG,515)NGAT
  515 FORMAT('GATE TABLE OVERFLOW AT - ',I6)
      GO TO 800
  520 WRITE(CMSSG,525)
  525 FORMAT('TRUNCATION ERROR FROM - GREAD')
      GO TO 800
  530 WRITE(CMSSG,535)
  535 FORMAT('SYNTAX ERROR FROM MILV3')
      GO TO 800
  540 WRITE(CMSSG,545)(GNAM(I,NGAT),I=1,4)
  545 FORMAT('MULTIPLE DEFINITION OF GATE NAME = ',3A4,I4)
      GO TO 800
  550 WRITE(CMSSG,555)GNAM(1,NGAT)
  555 FORMAT('ILLEGAL GATE NAME = ',A4)
      GO TO 800
  560 WRITE(CMSSG,565)PNAM
  565 FORMAT('FROM GATER - PARM NAME NOT FOUND = ',3A4,I4)
      GO TO 800
C
  800 CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)    !RESET WIDTH TO 8
      RETURN
      END
C$PROG GATMSK
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE GATMSK(NAM,IDX,PWN,MSK)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXG=100)
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      INTEGER*4 NAM(3),NAME(4)
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
      DO 10 I=1,3
      NAME(I)=NAM(I)
   10 CONTINUE
      NAME(4)=IDX
C
      CALL NAMLOC(GNAM,NAME,NGAT,NDX)
C
      IF(NDX.LE.0) GO TO 100
C
      PWN=PATN(NDX)
      MSK=GMSK(NDX)
      RETURN
C
  100 PWN=1
      MSK=0
C
      WRITE(CMSSG,105)
  105 FORMAT('GATE-NAME NOT FOUND = ',3A4,I6)
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG GREAD     - General string re-formatter
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 03/07/2002
C     ******************************************************************
C
      SUBROUTINE GREAD(IWD,LWD,ITYP,NF,IMIN,IMAX,NTER)
C
C     ------------------------------------------------------------------
      DIMENSION KWD(320),JWD(80),IWD(1),LWD(1),ITYP(1)
C
      CHARACTER*4 JMINC
C
      EQUIVALENCE (JMINC,JMIN)
C
      INTEGER*4    X21,X0D,X3B
C
      DATA         X21,X0D,X3B/'21'X,'0D'X,'3B'X/
C
      DATA JNUM,JALP,JDEL,MAXB,LF/1,2,3,320,8/
C
      SAVE JNUM,JALP,JDEL,MAXB,LF
C     ------------------------------------------------------------------
C
      NTER=0
C
      JMIN=IMIN
C
      IF(JMINC.NE.'SETW') GO TO 5
C
      LF=IMAX
      RETURN
    5 NF=0
C
C     ------------------------------------------------------------------
C     NTER = # BYTES TRUNCATED (THIS IS BAD STUFF)
C     ------------------------------------------------------------------
C
      DO 10 I=1,MAXB
      KWD(I)='20202020'X
   10 CONTINUE
      DO 15 I=1,80
      JWD(I)='00000020'X
   15 CONTINUE
C
C     ------------------------------------------------------------------
C     UNPACK BYTES INTO FULL WORDS
C     ------------------------------------------------------------------
C
      DO 20 I=IMIN,IMAX
      CALL ILBYTE(JTEMP,IWD,I-1)
      IF(JTEMP.EQ.X0D) GO TO 25    !TERMINATE ON CR
      IF(JTEMP.EQ.X3B) GO TO 25    !TERMINATE ON ;
      IF(JTEMP.EQ.X21) GO TO 25    !TERMINATE ON !
      JWD(I)=JTEMP
   20 CONTINUE
   25 NS=0
      JLO=IMIN
      JHI=IMAX
      KHI=0
   30 NS=NS+1
      KLO=KHI+1
      KHI=KLO+LF-1
C
C     ------------------------------------------------------------------
C     STARTING AT JLO, FIND THE FIRST NON-DELIMITER
C     ------------------------------------------------------------------
C
      IF(JLO.GT.JHI) GO TO 150
      DO 40 I=JLO,JHI
      IF(KINDA(JWD(I)).NE.JDEL) GO TO 45
   40 CONTINUE
      GO TO 150
C
C     ------------------------------------------------------------------
C     LEFT  JUSTIFY FIELDS STARTING WITH A NON-NUMERIC
C     RIGHT JUSTIFY FIELDS STARTING WITH A NUMERIC CHARACTER
C     ------------------------------------------------------------------
C
   45 JLO=I
      IF(KINDA(JWD(I)).EQ.JNUM) GO TO 100
      K=KLO
      NF=NS
      ITYP(NF)=1
      DO 50 I=JLO,JHI
      IF(KINDA(JWD(I)).EQ.JDEL) GO TO 55
      IF(K.LE.KHI) GO TO 48
      NTER=NTER+1
      GO TO 50
   48 KWD(K)=JWD(I)
      K=K+1
   50 CONTINUE
      GO TO 150
   55 JLO=I+1
      GO TO 30
C
C     ------------------------------------------------------------------
C     RIGHT JUSTIFY NUMERIC FIELDS
C
C     FIND NEXT DELIMITER
C     ------------------------------------------------------------------
C
  100 DO 110 I=JLO,JHI
      IF(KINDA(JWD(I)).EQ.JDEL) GO TO 120
  110 CONTINUE
      I=JHI+1
  120 JUP=I-1
      NDO=JUP-JLO+1
      IF(NDO.LT.1) GO TO 30
      IF(NDO.LE.LF) GO TO 125
      NTER=NTER+(NDO-LF)
      NDO=LF
  125 K=KHI
      J=JUP
      NF=NS
      ITYP(NF)=2
      DO 130 N=1,NDO
      KWD(K)=JWD(J)
      J=J-1
      K=K-1
  130 CONTINUE
      JLO=JUP+2
      GO TO 30
C
C     ------------------------------------------------------------------
C     NOW LOAD IT ALL INTO LWD
C     ------------------------------------------------------------------
C
  150 DO 160 I=1,MAXB
      JTEMP=KWD(I)
      CALL ISBYTE(JTEMP,LWD,I-1)
  160 CONTINUE
      RETURN
      END
C$PROG HARDASS
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE HARDASS(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      COMMON/PAC1/ KINMOD,MODKOD,NAMOD(3),MODATA(2,12)
C 
      INTEGER*4    IWD(20),ILO(20),IHI(20),NAME(3)
C
      CHARACTER*4  IT(5)
C
      CHARACTER*4  CODES(11)
C
      DATA CODES/'C   ','N   ','A   ','F   ','K   ',
     &           'D   ','E   ','FC  ','AC  ','DT  ','ID  '/
C
      DATA NCODE/11/
C
      CHARACTER*4   ICOD,JCODE
C
      INTEGER*4     BLANK
      DATA          BLANK/'    '/
C
      SAVE
C
C     ************************************************************
C     PROCESSES ONE HARDWARE ASSIGNMENT LINE OF THE TYPE:
C     $LAT, $CAM, $FER, $FAS & SAVES DATA VIA CALL TO MODSAV
C     ************************************************************
C
      DO 20 J=1,12
      MODATA(1,J)=-1
      MODATA(2,J)=-1
   20 CONTINUE
      NAMOD(1)=BLANK
      NAMOD(2)=BLANK
      NAMOD(3)=BLANK
C
      KINMOD=IWD(1)
      MODKOD=0
C
      NF=0
      IB=5
  100 IA=NXNB(IWD,IB,80)
      IF(IA.LE.0) GO TO 200
      IB=NXBL(IWD,IA,80)
      IF(IB.LE.0) IB=81
      IB=IB-1
      NF=NF+1
      ILO(NF)=IA
      IHI(NF)=IB
      IB=IB+1
      IF(IB.GE.80) GO TO 200
      GO TO 100
C
  200 DO 300 N=1,NF
      DO 205 I=1,5
      IT(I)='    '
  205 CONTINUE
C
      CALL LODUP(IWD,ILO(N),IHI(N),IT,1)
C
      CALL HARDECO(IT,ICOD,NAME,I1,I2)
      JCODE=ICOD
      CALL KASUP4(JCODE)
C
      IF(JCODE.EQ.'DT  ') THEN
      WRITE(CMSSG,210)
  210 FORMAT('DT form of delay-time specification not supported')
      CALL ERRLOG(LOGUT,LOGUP)
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'MT  ')   THEN
      CALL MODCOD(NAME,MODKOD,IERR)
      GO TO 300
                            ENDIF
C
      NDX=0
      DO 220 I=1,NCODE
      IF(JCODE.EQ.CODES(I)) THEN
                            NDX=I+1
                            MODATA(1,NDX)=I1
                            MODATA(2,NDX)=I2
                            GO TO 225
                            ENDIF
  220 CONTINUE
C
  225 IF(NDX.GT.0) GO TO 300
C
      NAMOD(1)=NAME(1)
      NAMOD(2)=NAME(2)
      NAMOD(3)=NAME(3)
      MODATA(1,1)=I1
      MODATA(2,1)=I2
C
  300 CONTINUE
C
      IF(MODATA(2,4).LT.MODATA(1,4)) MODATA(2,4)=MODATA(1,4)
C
      CALL MODSAV
C
      RETURN
      END
C$PROG HARDECO
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE HARDECO(JT,ICOD,NAME,I1,I2)
C
      CHARACTER*4   ICOD,NAME(3)
C
      INTEGER*4     IT(5),JT(5)
C
      INTEGER*4     LWD(2,40),ITYP(40)
C
      SAVE
C
C     ************************************************************
C     DECODES ONE CONTIGUOUS FIELD FROM "JT" OF THE FORM:
C
C     cII  or  cII-JJ  or  cII,JJ  or  name:II,JJ
C
C     c     IS RETURNED IN - ICODE
C     name  IS RETURNED IN - NAME
C     II    IS RETURNED IN - I1
C     JJ    IS RETURNED IN - I2 
C     ************************************************************
C
      ICOD   ='    '
      NAME(1)='    '
      NAME(2)='    '
      NAME(3)='    '
      I1=0
      I2=0
      DO 10 I=1,5
      IT(I)=JT(I)
   10 CONTINUE
C
      LCOL=IFIND(IT,'3A'X,1,20)
      IF(LCOL.GT.0) GO TO 200
C
      LEQU=IFIND(IT,'3D'X,1,20)
      IF(LEQU.GT.0) GO TO 300
C
      LDIG=NXDG(IT,1,20)
      IF(LDIG.LE.0) RETURN
C
      IB=LDIG-1
      CALL LODUP(IT,1,IB,ICOD,1)
C
      CALL ISUBB(IT,1,20,'2D'X,'2C'X)
C
      CALL GREAD(IT,LWD,ITYP,NF,LDIG,20,NTER)
      IF(NTER.NE.0) RETURN
C
      CALL MILV(LWD(1,1),I1,XV,KIND,IERR)
      CALL MILV(LWD(1,2),I2,XV,KIND,IERR)
      RETURN
C
  200 IB=LCOL-1
      CALL LODUP(IT,1,IB,NAME,1)
C
      IB=LCOL+1
      CALL GREAD(IT,LWD,ITYP,NF,IB,20,NTER)
C
      CALL MILV(LWD(1,1),I1,XV,KIND,IERR)
      CALL MILV(LWD(1,2),I2,XV,KIND,IERR)
C
      RETURN
C
  300 IA=LEQU+1
      IB=LSNB(IT,IA,20)
      IF(IB.LT.IA) IB=IA
      IF(IB.GT.IA+11) IB=IA+11
      ICOD='MT  '
      CALL LODUP(IT,IA,IB,NAME,1)
      RETURN
      END
C$PROG HEXVAL
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      SUBROUTINE HEXVAL(IWD,IV,IERR)
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      INTEGER*4 IWD(2),JWD(2)
C
      CHARACTER*8   CJWD
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CJWD,JWD),(CMSSG,MSSG)
C
      INTEGER*4    X20,X30,X39,X41,X46,X48
      DATA X20,X30,X39,X41,X46,X48/'20'X,'30'X,'39'X,'41'X,'46'X,'48'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C     "IWD" CONTAINS RIGHT-JUSTIFIED ASCII STRING REPRESENTING A
C     HEXADECIMAL NUMBER
C     FIRST CHARACTER MUST BE A DECIMAL DIGIT
C     LAST  CHARACTER MUST BE CHARACTER "H"
C     THE NUMERICAL VALUE IS RETURNED IN "IV"
C     ------------------------------------------------------------------
C
      IV=0
      IERR=0
      CJWD=' '
C
      CALL ILBYTE(IT,IWD,7)
      IF(IT.NE.X48) GO TO 50
C
      I=6
      J=7
      DO 40 N=1,7
      CALL ILBYTE(IT,IWD,I)
      IF(IT.EQ.X20) GO TO 30
      IF(IT.GE.X30.AND.IT.LE.X39) GO TO 20
      IF(IT.GE.X41.AND.IT.LE.X46) GO TO 20
      GO TO 50
C
   20 CALL ISBYTE(IT,JWD,J)
      J=J-1
C
   30 I=I-1
   40 CONTINUE
C
      READ(CJWD,45,ERR=50)IV
   45 FORMAT(Z8)
      RETURN
C
   50 WRITE(CMSSG,55)IWD
   55 FORMAT('ILLEGAL HEX-VALUE SPECIFICATION = ',2A4)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
C$PROG IFIND
      FUNCTION IFIND(IBY,ITST,IA,IB)
C
      BYTE IBY(1)
C
      INTEGER*4   X3B,X21
      DATA        X3B,X21/'3B'X,'21'X/
C
C     FUNCTION TO RETURN THE LOCATION OF TEST BYTE "ITST" IN IBY
C
      IFIND=0
C
      DO 10 I=IA,IB
      JTEMP=IBY(I)
      IF(JTEMP.EQ.X3B) RETURN
      IF(JTEMP.EQ.X21) RETURN
      IF(JTEMP.EQ.ITST)  GO TO 20
   10 CONTINUE
      RETURN
   20 IFIND=I
      RETURN
      END
C$PROG ISEQUL
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      FUNCTION ISEQUL(IWD,JWD,N)
C
      INTEGER*4 IWD(*),JWD(*)
C
      INTEGER*4  YES,NO
      DATA       YES,NO/'YES ','NO  '/
C
C
      DO 10 I=1,N
      IF(IWD(I).NE.JWD(I)) GO TO 100
   10 CONTINUE
      ISEQUL=YES
      RETURN
C
  100 ISEQUL=NO
      RETURN
      END
C$PROG ISUBB
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE ISUBB(IBY,IA,IB,ICI,ICF)
C
      BYTE IBY(*)
C
C     ************************************************************
C     REPLACE ALL CHARACTERS "ICI" WITH "ICF" IN IBY WITHIN IA-IB
C     ************************************************************
C
      DO 10 I=IA,IB
      IF(IBY(I).EQ.ICI) IBY(I)=ICF
   10 CONTINUE
      RETURN
      END
C$PROG ISVAL
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      FUNCTION ISVAL(IWD,JTYP,IERR)
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C   
      INTEGER*4 IWD(2)
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
C     ************************************************************
C     RETURNS VALUE ASSOCIATED WITH SYMBOL OR ASCII CONSTANT 
C     CONTAINED IN IWD
C     ************************************************************
C   
      IERR=0
      ITYP=JTYP
      IF(ITYP.EQ.0) ITYP=ITYPER(IWD)
C
      IF(ITYP.EQ.0) GO TO 500   
      IF(ITYP.EQ.1) GO TO 100
      IF(ITYP.EQ.2) GO TO 200
      IF(ITYP.EQ.3) GO TO 300
C
  100 DO 110 I=1,NSYM
      IF(IWD(1).EQ.ISYN(I)) THEN
                            ISVAL=ISYV(I)
                            RETURN
                            ENDIF
  110 CONTINUE
      GO TO 500
C   
  200 CALL MILV(IWD,IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 500
      IF(KIND.NE.1) GO TO 500
      ISVAL=IV
      RETURN
C
  300 CALL HEXVAL(IWD,IV,IERR)
      IF(IERR.NE.0) GO TO 500
      ISVAL=IV
      RETURN
C
  500 WRITE(CMSSG,510)IWD
  510 FORMAT('ERROR EVALUATING SYMBOL OR CONSTANT = ',2A4)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
      ISVAL=0
      RETURN
      END
C$PROG ITYPER
C
      FUNCTION ITYPER(IBY)
C
      BYTE IBY(8)
C
      BYTE   X20,X30,X39
      DATA   X20,X30,X39/'20'X,'30'X,'39'X/
C
      BYTE   HUC,HLC
      DATA   HUC,HLC/'H','h'/
C
      SAVE
C
      ILO=0
      IHI=0
      DO 10 I=1,8
      IF(IBY(I).NE.X20) GO TO 20
   10 CONTINUE
      ITYPER=2
      RETURN
C
   20 ILO=I
      DO 30 I=ILO+1,8
      IF(IBY(I).EQ.X20) THEN
                        IHI=I-1
                        GO TO 50
                        ENDIF
   30 CONTINUE
      IHI=8
C
   50 IF(IBY(ILO).GE.X30.AND.IBY(ILO).LE.X39) GO TO 60
      ITYPER=1
      RETURN
C
   60 IF(IBY(IHI).EQ.HUC.OR.IBY(IHI).EQ.HLC) THEN
                                             ITYPER=3
                                             RETURN
                                             ENDIF
C
      ITYPER=2
      RETURN
      END
C$PROG JOCO
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      FUNCTION JOCO(IWD,IERR)
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(2),NSH(7)
C
      DATA (NSH(I),I=1,7)/0,3,6,9,12,15,18/
C
      DATA IBLN,IHEX,IOCT/'20'X,'48'X,'42'X/
C
      INTEGER*4   X30,X37
      DATA        X30,X37/'30'X,'37'X/
C
      SAVE
C
C     DECODES A STRING OF RIGHT-JUSTIFIED CHARACTERS AS DEC, HEX
C
      IERR=0
      CALL ILBYTE(JT,IWD,7)
      IF(JT.EQ.IHEX) GO TO 100
      IF(JT.EQ.IOCT) GO TO 200
C
C     IT IS A DECIMAL INTEGER - CALL MILV
C
      CALL MILV(IWD,IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 310
      IF(KIND.NE.1) GO TO 300
      JOCO=IV
      RETURN
C
C     IT IS A HEX INTEGER - CALL HEXVAL
C
  100 CALL HEXVAL(IWD,IV,IERR)
      IF(IERR.NE.0) GO TO 310
      JOCO=IV
      RETURN
C
C     IT IS AN OCTAL STRING - DO BYTE-BY-BYTE
C
  200 N=7
      ISUM=0
      DO 220 I=1,7
      CALL ILBYTE(JT,IWD,N-1)
      IF(JT.EQ.IBLN) GO TO 230
      IF(JT.LT.X30.OR.JT.GT.X37) GO TO 300
      JT=IAND(JT,7)
      ISUM=ISUM+ISHFT(JT,NSH(I))
      N=N-1
  220 CONTINUE
  230 JOCO=ISUM
      RETURN
C
  300 WRITE(CMSSG,305)IWD
  305 FORMAT('SYNTAX ERROR IN INTEGER OR OCTAL VALUE = ',2A4)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
  310 JOCO=0
      RETURN
      END
C$PROG KASUP4
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE KASUP4(IBY)
C
      BYTE IBY(4)
C
      BYTE  X20,X61,X7A
      DATA  X20,X61,X7A/'20'X,'61'X,'7A'X/
C
      SAVE
C
C     ************************************************************
C     CONVERT 4-BYTES TO UPPER-CASE
C     ************************************************************
C
      DO 10 I=1,4
      IT=IBY(I)
      IF(IT.LT.X61.OR.IT.GT.X7A) GO TO 10
      IBY(I)=IT-X20
   10 CONTINUE
      RETURN
      END
C$PROG KILLER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE KILLER(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXG=100)
      PARAMETER (MXK=50)
      PARAMETER (MXL=50)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PACH/ KILATI(MXK),KILMSK(MXK),KILTYP(MXK),NKIL
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      INTEGER*4 NAMLAT(4,MXL),NAML(4),NAMG(4),NLAT
C
      DATA NCALL,NLAT,NKIL,MXLAT,MXKIL/0,0,0,50,50/
C
      DATA SETW/'SETW'/
C
      CHARACTER*4  CLWD(3,40),KKIL,KIMO
C
      EQUIVALENCE (CLWD,LWD)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 100
C
      DO 50 N=1,NUMT
      IF(KIMO(N).NE.'$LAT') GO TO 50
      NLAT=NLAT+1
      IF(NLAT.GT.MXLAT)     GO TO 510
      NAMLAT(1,NLAT)=NAMO(1,N)
      NAMLAT(2,NLAT)=NAMO(2,N)
      NAMLAT(3,NLAT)=NAMO(3,N)
      NAMLAT(4,NLAT)=NAMO(4,N)
   50 CONTINUE
      NCALL=1
C
  100 CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER)
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
C
      IF(NTER.NE.0) GO TO 520
C
C     ************************************************************
C     DETERMINE IF KILL CONDITION IS ON LATCH-WORD OR GATE
C     LWD(1,1)='TRUE' SAYS GATE-TYPE
C     ************************************************************
C
      IF(CLWD(1,1).EQ.'TRUE'.AND.CLWD(2,1).EQ.'    ') GO TO 200
      IF(CLWD(1,1).EQ.'FALS'.AND.CLWD(2,1).EQ.'E   ') GO TO 200
      IF(CLWD(1,1).EQ.'FALS'.AND.CLWD(2,1).EQ.'    ') GO TO 200
C
      KKIL=CLWD(1,1)
C
      IF(KKIL.NE.'ANY '.AND.KKIL.NE.'NONE') GO TO 500
C
      CALL MILV3(LWD(1,3),LDX,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 530
C
      DO 110 I=1,3
      NAML(I)=LWD(I,2)
  110 CONTINUE
      NAML(4)=LDX
C
      CALL NAMLOC(NAMLAT,NAML,NLAT,JXX)
      IF(JXX.LE.0) GO TO 540
C
      CALL HEXVAL(LWD(2,4),MSK,IERR)
      IF(IERR.NE.0) GO TO 550
C
      NKIL=NKIL+1
      IF(NKIL.GT.MXKIL)   GO TO 590
      KILATI(NKIL)=JXX
      KILMSK(NKIL)=MSK
      KILTYP(NKIL)=1
      IF(KKIL.EQ.'NONE') KILTYP(NKIL)=0
      GO TO 1000
C
C     ************************************************************
C     PROCESS GATE-TYPE CONDITIONAL KILL
C     ************************************************************
C
  200 KKIL=CLWD(1,1)
C
      CALL MILV3(LWD(1,3),IDX,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 530
      DO 210 I=1,3
      NAMG(I)=LWD(I,2)
  210 CONTINUE
      NAMG(4)=IDX
C
      CALL NAMLOC(GNAM,NAMG,NGAT,NDX)
      IF(NDX.LE.0) GO TO 560
C
      NKIL=NKIL+1
      IF(NKIL.GT.MXKIL) GO TO 590
      KILATI(NKIL)=PATN(NDX)
      KILMSK(NKIL)=GMSK(NDX)
      KILTYP(NKIL)=1
      IF(KKIL.EQ.'FALS') KILTYP(NKIL)=0
      GO TO 1000
C
C     ************************************************************
C     SEND ERROR MESSAGES
C     ************************************************************
C
  500 WRITE(CMSSG,505)KKIL
  505 FORMAT('ILLEGAL KILL-TYPE = ',A4)
      GO TO 800
  510 WRITE(CMSSG,515)NLAT
  515 FORMAT('LATCH-WORD TABLE OVERFLOW FROM KILLER - NLAT=',I6)
      GO TO 800
  520 WRITE(CMSSG,525)
  525 FORMAT('TRUNCATION ERROR FROM - GREAD')
      GO TO 800
  530 WRITE(CMSSG,535)
  535 FORMAT('SYNTAX ERROR FROM MILV3')
      GO TO 800
  540 WRITE(CMSSG,545)NAML
  545 FORMAT('REFERRENCED LATCH-WORD & INDEX - ',3A4,I6,' NOT FOUND')
      GO TO 800
  550 WRITE(CMSSG,555)LWD(2,4),LWD(3,4)
  555 FORMAT('ERROR DECODING HEX MASK - ',2A4)
      GO TO 800
  560 WRITE(CMSSG,565)NAMG
  565 FORMAT('REFERRENCED GATENAME & INDEX - ',3A4,I6,' NOT FOUND')
      GO TO 800
  590 WRITE(CMSSG,595)NKIL
  595 FORMAT('KIL TABLE OVERFLOW AT - ',I6)
C
  800 CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
      RETURN
      END
C$PROG KINDA
      INTEGER*4 FUNCTION KINDA(IWD)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4  IWD,KIND
C
      INTEGER*4  X30,X39,X2B,X2D,X2E,X20,X2C,X3D,X2F,X28,X29
C
      DATA       X30/'30'X/  ! digit 0
      DATA       X39/'39'X/  ! digit 9
      DATA       X2B/'2B'X/  ! +
      DATA       X2D/'2D'X/  ! -
      DATA       X2E/'2E'X/  ! .
      DATA       X20/'20'X/  ! blank
      DATA       X2C/'2C'X/  ! ,
      DATA       X3D/'3D'X/  ! =
      DATA       X2F/'2F'X/  ! /
      DATA       X28/'28'X/  ! (
      DATA       X29/'29'X/  ! )
C
C     ------------------------------------------------------------------
C     FUNCTION TO DETERMINE BYTE "TYPE"
C
C     KINDA=1 SAYS NUMERIC (DIGITS 0 THRU 9 + - . )
C     KINDA=2 SAYS NON-NUMERIC BUT NOT A DELIMITER
C     KINDA=3 SAYS A DELIMITER (BLANK , = / ( )  )
C     ------------------------------------------------------------------
C
      IF(IWD.GE.X30.AND.IWD.LE.X39)              GO TO 10
      IF(IWD.EQ.X2B.OR. IWD.EQ.X2D)              GO TO 10
C
      IF(IWD.EQ.X20.OR.IWD.EQ.X2C.OR.IWD.EQ.X3D) GO TO 20
      IF(IWD.EQ.X2F.OR.IWD.EQ.X28.OR.IWD.EQ.X29) GO TO 20
      IF(IWD.EQ.X2E)                             GO TO 20
C
      KIND=2
      GO TO 30
   10 KIND=1
      GO TO 30
   20 KIND=3
   30 KINDA=KIND
      RETURN
      END
C$PROG KVALUE
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      FUNCTION KVALUE(IWD,IA,IB,IERR)
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C   
      INTEGER*4 IWD(20),LWD(2,40),ITYP(40),KNOP(40)
C
      SAVE
C   
C     ************************************************************
C     FUNCTION TO EVALUATE OPERAND EXPRESSIONS
C     ************************************************************
C   
      IF(IA.GT.IB) GO TO 210
      IERR=0
      IVAL=0
      CALL REFMT(IWD,LWD,ITYP,KNOP,IA,IB,NF,NTER)
      IF(NTER.NE.0) GO TO 210
      IF(NF.LE.0)   GO TO 210
C   
      DO 200 N=1,NF
      IF(ITYP(N).EQ.1) GO TO 40
C   
C     IT IS A CONSTANT
C   
      ITERM=JOCO(LWD(1,N),IERR)
      IF(IERR.NE.0) GO TO 210
      GO TO 100
C   
C     IT IS A SYMBOL - LOOK UP VALUE IN TABLE
C   
   40 DO 50 J=1,NSYM
      IF(LWD(1,N).NE.ISYN(J)) GO TO 50
      GO TO 60
   50 CONTINUE
      GO TO 220
C   
   60 ITERM=ISYV(J)
  100 IGO=KNOP(N)
      IF(IGO.LT.1.OR.IGO.GT.4) GO TO 210
C   
      GO TO (110,120,130,140),IGO
C   
  110 IVAL=IVAL+ITERM
      GO TO 200
  120 IVAL=IVAL-ITERM
      GO TO 200
  130 IVAL=IVAL*ITERM
      GO TO 200
  140 IVAL=IVAL/ITERM
  200 CONTINUE
      KVALUE=IVAL
      RETURN
C   
C     ************************************************************
C     RETURN ERROR MESSAGES
C     ************************************************************
C   
  210 WRITE(CMSSG,215)
  215 FORMAT('SYNTAX ERROR IN EXPRESSION')
      GO TO 300
C   
  220 WRITE(CMSSG,225)LWD(1,N)
  225 FORMAT('ERROR - UNDEFINED SYMBOL = ',A4)
C   
  300 CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
      KVALUE=0
      RETURN
      END
C$PROG KVALU
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      FUNCTION KVALU(IWD,KA,KB,IERR)
C
      INTEGER*4 IWD(*)
C
      DATA ICOM,ILSQ,IRSQ/'2C'X,'5B'X,'5D'X/
C
      CHARACTER*4  MODT
C
      SAVE
C
      IA=KA
      IERR=0
      IVAL=0
      IX=IA+3                           !CHECK FOR "MOD" FUNCTION
      CALL LODUP(IWD,IA,IX,MODT,1)
      IF(MODT.NE.'MOD(') GO TO 100      !IF NO, GO EVALUATE NORMALLY
      IA=IA+4                           !SKIP TO ARG1
      IB=IFIND(IWD,ICOM,IA,KB)-1        !LOOK FOR COMMA
      IF(IB.LE.0) GO TO 301             !TST FOR ERROR
      IV1=KVALUE(IWD,IA,IB,IERR)        !EVALUATE ARG1
      IF(IERR.NE.0) GO TO 310           !TST FOR ERROR
      IA=IB+2                           !SKIP TO ARG2
      IB=IFIND(IWD,'29'X,IA,KB)-1       !LOOK FOR ")"
      IF(IB.LT.0) GO TO 301             !TST FOR ERROR
      IV2=KVALUE(IWD,IA,IB,IERR)        !EVALUATE ARG2
      IF(IERR.NE.0) GO TO 310           !TST FOR ERROR
      IVAL=MOD(IV1,IV2)                 !COMPUTE "REMAINDER"
      GO TO 200                         !AND HEAD FOR HOME
C
  100 IF(IA.GT.KB) GO TO 200            !TST FOR DONE
      IX=IFIND(IWD,ILSQ,IA,KB)          !LOOK FOR [ - BIT LIST
      IF(IX.GT.0) GO TO 110             !IF YES, TST FOR PRECEEDING FIEL
      IVAL=IVAL+KVALUE(IWD,IA,KB,IERR)  !OTHERWISE, EVAL EXPRESSION
      IF(IERR.NE.0) GO TO 310           !TST FOR ERROR
      GO TO 200                         !OR GO HOME
C
  110 LLSQ=IX                           !LOCATION OF [
      IF(IX.EQ.IA) GO TO 120            !NO PRECEEDING FIELD
      IB=LLSQ-1                         !HI-BYTE OF FIELD
      IVAL=IVAL+KVALUE(IWD,IA,IB,IERR)  !EVALUATE EXPRESSION
      IF(IERR.NE.0) GO TO 310
C
  120 IBIV=0                            !ZERO BIT ACCUMULATOR
      IA=LLSQ+1                         !LO-BYTE OF BIT-LIST
  130 IF(IA.GT.KB) GO TO 160            !TST FOR DONE
      IX=IFIND(IWD,IRSQ,IA,KB)          !LOOK FOR ] - END OF LIST
      IF(IX.LE.0) GO TO 301             !TST FOR ERROR
      LRSQ=IX                           !LOCATION OF ]
      IHI=LRSQ-1                        !HI-BYTE OF BIT-LIST
      IX=IFIND(IWD,ICOM,IA,IHI)         !LOOK FOR "," - END OF FIELD
      LCOM=IX                           !LOCATION OF COMMA
      IF(IX.LE.0) IX=IHI+1
      IB=IX-1                           !HI-BYTE OF FIELD
      IBIT=KVALUE(IWD,IA,IB,IERR)       !BIT # (LO-BIT = 1 BASIS)
      IF(IERR.NE.0) GO TO 310           !TST FOR ERROR
      IF(IBIT.GT.24) GO TO 302          !TST FOR TOO BIG
      IF(IBIT.LT.1)  GO TO 302          !TST FOR TOO SMALL
      IBN=IBIT-1
      IBIV=IBSET(IBIV,IBN)              !SET THE MIGHTY BIT
      IF(LCOM.EQ.0) GO TO 140           !TST FOR LAST FIELD
      IA=LCOM+1                         !OTHERWISE, BUMP SCAN PNTR
      GO TO 130                         !AND GO FOR NEXT FIELD
C
  140 IA=LRSQ+1                         !BUMP SCAN PNTR
      IVAL=IVAL+IBIV                    !ADD BIT-TERM
      GO TO 100                         !AND GO BACK TO "TOP"
C
  160 IVAL=IVAL+IBIV                    !BIT-LIST ENDS FULL RANGE
C
  200 KVALU=IVAL                        !SET THE FUNCTION
      RETURN                            !AND GO HOME
C
  301 IERR=1                            !SYNTAX ERROR
      GO TO 310
  302 IERR=17                           !ILLEGAL BIT VALUE
  310 KVALU=0
      RETURN
      END
C$PROG LABLMAN
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE LABLMAN(MODE,JLABL,JLABV,IERR)     
C
      PARAMETER (MXI=2000)
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C
      COMMON/PAC6/ LABLIS(2,MXI),LABVAL(MXI),NLA 
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      DATA MXLA/MXI/
C
      CHARACTER*4  MODE,INST,LABL,LABLIS,JLABL(2)
C
      SAVE
C
C     ************************************************************
C     LABEL MANAGER - "SAVES & GETS" LABELS AND ASSOCIATED VALUES
C     ************************************************************
C
      IERR=0
C
      IF(MODE.EQ.'GET ') GO TO 100
C
      NLA=0
      II=0
      DO 20 N=1,NCI
C
      IF(INST(N).EQ.'READ') GO TO 10
      IF(INST(N).EQ.'CNAF') GO TO 10
      II=II+1
C
   10 IF(LABL(N).EQ.'    ') GO TO 20
      NLA=NLA+1
C
      IF(NLA.GT.MXLA) THEN
      WRITE(CMSSG,15)NLA
   15 FORMAT('MAX# LABELS EXCEEDED AT NLA =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NLA=MXLA
                      ENDIF
C
      LABLIS(1,NLA)=LABL(N)
      LABLIS(2,NLA)='    '
      LABVAL(NLA)=II
   20 CONTINUE
C
      DO 50 J=1,NLA
      DO 40 I=J+1,NLA
      IF(LABLIS(1,I).NE.LABLIS(1,J)) GO TO 40
      IF(LABLIS(2,I).NE.LABLIS(2,J)) GO TO 40
      WRITE(CMSSG,30)LABLIS(1,I),LABLIS(2,I)
   30 FORMAT(2A4,' - MULTIPLY DEFINED')
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
   40 CONTINUE
   50 CONTINUE
      RETURN
C
  100 JLABV=0
      DO 120 N=1,NLA
      IF(JLABL(1).NE.LABLIS(1,N)) GO TO 120
      IF(JLABL(2).NE.LABLIS(2,N)) GO TO 120
      JLABV=LABVAL(N)
      RETURN
  120 CONTINUE
      WRITE(CMSSG,130)JLABL
  130 FORMAT('LABL NOT FOUND = ',2A4)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
C$PROG LABLOOP
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE LABLOOP(MODE,JWD)
C
      INTEGER*4 LABL(1000),LABV(1000)
C
      INTEGER*4 JWD(20),IWD(20),LWD(2,40),ITYP(40),NF
C
      CHARACTER*8  CDUM
      INTEGER*4    IDUM(2)
      EQUIVALENCE (CDUM,IDUM)
C
      DATA KLAB,NLAB/0,0/
C
      CHARACTER*4   MODE
C
      CHARACTER*5  CIWD
C
      CHARACTER*4   CLWD(2,40)
C
      EQUIVALENCE  (CLWD,LWD),(CIWD,IWD(1))
C
      INTEGER*4     BLANK
      DATA          BLANK/'    '/
C
      SAVE
C
C     ************************************************************
C     CREATES PSEUDO-LABELS IN SUPPORT OF LOOP EXPANDER - LOOPEX
C     ************************************************************
C
      DO 10 I=1,20
      IWD(I)=JWD(I)
   10 CONTINUE
C
      IF(MODE.EQ.'INIT') GO TO 100
      IF(MODE.EQ.'SAVE') GO TO 200
      IF(MODE.EQ.'SET ') GO TO 300
      IF(MODE.EQ.'SUB ') GO TO 400
                         GO TO 500
C
  100 NLAB=0                                !SET # OF LABELS TO ZERO
      GO TO 500
C
  200 IF(IWD(1).EQ.BLANK) GO TO 500         !SAVE LINE LABEL IN LABL
      NLAB=NLAB+1
      LABL(NLAB)=IWD(1)
      GO TO 500
C
  300 DO 310 I=1,NLAB                       !ASSIGN SEQUENTIAL INTEGER
      KLAB=KLAB+1                           !VALUE TO ALL LABELS SAVED
      LABV(I)=KLAB
  310 CONTINUE
      GO TO 500

C
  400 IT=IWD(1)                             !SUBSTITUTE NUMERICAL :XXX
      IF(IT.EQ.BLANK) GO TO 430             !LABEL FOR ALL LINE-LABELS
      DO 410 I=1,NLAB                       !AND DESTINATION LABELS IN
      IF(IT.EQ.LABL(I)) GO TO 420           !LIST
  410 CONTINUE
      GO TO 430
C
  420 WRITE(CIWD,425)LABV(I)                !BUILD LINE-LABEL
  425 FORMAT(':',I4)
      CALL SQUEZL(IWD,1,5)                  !AND INSERT IT 
C
  430 CALL GREAD(IWD,LWD,ITYP,NF,7,80,NTER) !RE-FORMAT INPUT LINE
C
      IF(CLWD(1,1).EQ.'IFU ') GO TO 440     !TST FOR DESTINATION FIELS
      IF(CLWD(1,1).EQ.'IFS ') GO TO 440
      IF(CLWD(1,1).EQ.'IFA ') GO TO 440
      IF(CLWD(1,1).EQ.'IFN ') GO TO 440
      IF(CLWD(1,1).EQ.'IFT ') GO TO 440
      IF(CLWD(1,1).EQ.'IFF ') GO TO 440
      IF(CLWD(1,1).EQ.'GOTO') GO TO 440
      GO TO 500                             !IF NOT RETURN
C
  440 DO 445 I=1,NLAB                       !OTHERWISE, LOOK FOR DEST
      IF(LWD(1,NF).EQ.LABL(I)) GO TO 450    !IN LABEL LIST AND REPLACE
  445 CONTINUE
      GO TO 500
C
  450 WRITE(CDUM,455) LABV(I)               !WITH GENERATED SYMBOL
  455 FORMAT(':',I6,'!')                    !AND LINE TERMINATOR
C
      CALL SQUEZL(IDUM,1,8)                 !SQUEEZE LEFT
C
      IL=LASWD(IWD,7,80)                    !LOCATE WORD TO REPLACE
      IF(IL.LE.0) RETURN                    !THIS IS AN ERROR!!!
C
      CALL LODUP(IDUM,1,8,IWD,IL)           !LOAD IT IN SOURCE LINE
C
  500 DO 510 I=1,20
      JWD(I)=IWD(I)
  510 CONTINUE
      RETURN
      END

C$PROG LABLV
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      FUNCTION LABLV(LABEL)
C
      INTEGER*4 LABEL,LABL(2)
C
C     ************************************************************
C     RETURNS THE VALUE ASSOCIATED WITH ASCII LABEL CONTAINED IN
C     "LABEL" (!!!! CHECK DIMENSION REQUIREMENTS !!!!)
C     ************************************************************
C
      LABL(1)=LABEL
      LABL(2)='20202020'X
C
      CALL LABLMAN('GET ',LABL,IV,IERR)
C
      LABLV=IV
      RETURN
      END
C$PROG LASNON
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      FUNCTION LASNON(IBY,IA,IB)
C
      BYTE IBY(1)
C
      BYTE X20,X3B,X21
      DATA X20,X3B,X21/'20'X,'3B'X,'21'X/
C
      SAVE
C
C     ************************************************************
C     FUNCTION TO RETURN LOCATION OF LAST NON-BLANK UP TO ; OR !
C     ************************************************************
C
      LN=0
      DO 10 I=IA,IB
      IF(IBY(I).EQ.X21) GO TO 20
      IF(IBY(I).EQ.X3B) GO TO 20
      IF(IBY(I).EQ.X20) GO TO 10
      LN=I
   10 CONTINUE
C
   20 LASNON=LN
      RETURN
      END
C$PROG LASWD
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      FUNCTION LASWD(IBY,IA,IB)
C
      BYTE    IBY(*)
C
      BYTE    X20
      DATA    X20/'20'X/
C
      SAVE
C
C     ************************************************************
C     LOCATES BEGINNING OF LAST NON-BLANK FIELD BETWEEN IA & IB 
C     ************************************************************
C
      LASWD=0
      JB=LASNON(IBY,IA,IB)
      IX=JB
      IF(IX.LE.0)  RETURN
   50 IF(IX.LT.IA) RETURN
      IF(IBY(IX).EQ.X20) GO TO 100
      IX=IX-1
      GO TO 50
C
  100 IX=IX+1
      DO 110 I=IX,JB
      IBY(I)=X20
  110 CONTINUE
      LASWD=IX
      RETURN
      END
C$PROG LATWDX
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      FUNCTION LATWDX(NAME,INDX)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 NAME(3)
C
      CHARACTER*4  KIMO
C
      SAVE
C
C     ************************************************************
C     RETURNS LATCH-WORD-INDEX CORRESPONDING TO NAME(INDX)
C     ************************************************************
C
      LATN=0
      DO 20 K=1,NUMT                      !LOOP ON FULL TABLE
      IF(KIMO(K).NE.'$LAT')    GO TO 20   !LOOK FOR $LAT TYPE
      LATN=LATN+1
      DO 10 I=1,3                         !CHECK FOR NAME  MATCH
      IF(NAME(I).NE.NAMO(I,K)) GO TO 20
   10 CONTINUE
      IF(INDX.NE.NAMO(4,K))    GO TO 20   !CHECK FOR INDEX MATCH
      GO TO 50
   20 CONTINUE
      GO TO 100
C
   50 LATWDX=LATN                         !LATCH-WORD INDEX 
      RETURN
C
  100 WRITE(CMSSG,105)NAME,INDX
  105 FORMAT('UNDEFINED LATCH-WORD-NAME,INDEX = ',3A4,I6)
      CALL ERRLOG(LOGUT,LOGUP)
      LATWDX=1  
      RETURN
      END
C$PROG LEGLASS
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 01/27/96
C     ************************************************************
C
      SUBROUTINE LEGLASS(LINE,N)
C
      BYTE LINE(*)
C
      BYTE X20,X7E
C
      DATA X20,X7E/'20'X,'7E'X/
C
      SAVE
C
      DO 10 I=1,80
      IF(LINE(I).LT.X20) GO TO 100
      IF(LINE(I).GT.X7E) GO TO 100
   10 CONTINUE
      RETURN
C
  100 WRITE(6,105)N
  105 FORMAT(1H ,'Illegal character on line no.',I6)
      WRITE(6,110)
  110 FORMAT(1H ,'Compilation aborted!')
      STOP
      END
C$PROG LEGLNAM
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      FUNCTION LEGLNAM(NAME)
C
      CHARACTER*4  NAME(*)
C
      INTEGER*4    YES,NO
C
      DATA         YES,NO/'YES ','NO  '/
C
      SAVE
C
      IF(NAME(1).EQ.'AND ') GO TO 10
      IF(NAME(1).EQ.'OR  ') GO TO 10
      IF(NAME(1).EQ.'NOT ') GO TO 10
C
      LEGLNAM=YES
      RETURN
C
   10 LEGLNAM=NO
      RETURN
      END
C$PROG LISSOR
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE LISSOR(KIND,IWD)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      INTEGER*4    IWD(20)
C
      CHARACTER*4  KIND,LISTYP
C
      SAVE
C
C     ************************************************************
C     LISTS SOURCE (OR GENERATED) LINE CONTAINED IN "IWD"
C
C     KIND   = 'SOR ' SAYS    SOURCE-LINE
C     KIND   = 'GEN ' SAYS GENERATED-LINE
C
C     ISORL  =   SOURCE-LINE COUNTER
C     IEXPL  = EXPANDED-LINE COUNTER
C  
C     LISTYP = 'NONE' SAYS LIST NONE
C     LISTYP = 'SOR ' SAYS LIST SOURCE-LINES ONLY
C     LISTYP = 'ALL ' SAYS LIST SOURCE & GENERATED LINES
C     ************************************************************
C
      IF(KIND.EQ.'SOR ') THEN
                         ISORL=ISORL+1
                         IEXPL=IEXPL+1
                         ENDIF
C
      IF(KIND.EQ.'GEN ') IEXPL=IEXPL+1
C
      IF(LISTYP.EQ.'NONE') RETURN
C
      IF(LISTYP.EQ.'ALL ') GO TO 100
C
      IF(KIND.EQ.'SOR ')   GO TO 100
C
      RETURN
C
  100 WRITE(LOGUP,110)ISORL,IEXPL,IWD
  110 FORMAT(1H ,2I6,2X,20A4)
      RETURN
      END
C$PROG LOOPEX
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE LOOPEX(LIN,LCI)
C   
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/ML02/ IWDRAW(20)
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C   
      INTEGER*4 LINE(20,500),LINTY(500),ISORN(500),NOL
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      CHARACTER*4  KMD,IDONE
C   
      EQUIVALENCE (KMD,LWD(1,1))
C
      INTEGER*4    BLANK
      DATA         BLANK/'    '/
C
      SAVE
C   
C     ************************************************************
C   
C     ROUTINE TO READ AND PROCESS LOOPS (NESTING NOT SUPPORTED)
C   
C     ************************************************************
C     LINTY - GIVES LINE-TYPE (LOOP, ENDLOOP OR COMMAND)
C     LINE  - CONTAINS ALL LINES READ FROM LOOP TO MATCHING ENDL
C   
C     LINTY(I) = '    '   'LOOP'                 'ENDL'
C     ************************************************************
      DO 10 I=1,100
      LINTY(I)=BLANK
   10 CONTINUE
      IERR=0
      NOL=1
      CALL LABLOOP('INIT',IWD)
C   
C     ************************************************************
C     PICK UP FIRST LINE (READ BY MAIN PROG) AND LOOP-COUNT
C     ************************************************************
C   
      DO 15 I=1,20
      LINE(I,1)=IWD(I)
   15 CONTINUE
      ISORN(1)=ISORL
C   
      CALL GREAD(IWD,LWD,ITYP,NF,7,80,NTER)
C   
      NLOO=ISVAL(LWD(1,2),ITYP(2),JERR)
      IF(JERR.NE.0) IERR=1
C   
C     ************************************************************
C     READ INPUT LINES AND STORE UNTIL MATCHING ENDL IS ENCOUNTERED
C     ************************************************************
C   
   20 NOL=NOL+1
C   
      IF(NOL.GT.500)       THEN
                           IERR=1
                           WRITE(CMSSG,22)
                           CALL ERRLOG(LOGUT,LOGUP)
                           STOP 1
                           ENDIF
C   
   22 FORMAT('MAX NO. OF LOOP LINES = 100 - EXCEEDED')
C
      READ(LIN,25,END=520)IWD
   25 FORMAT(20A4)
C
      CALL LISSOR('SOR ',IWD)
C
      CALL CASEUP(IWD)
C
      CALL LABLOOP('SAVE',IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,7,80,NTER)
C  
      DO 26 I=1,20
      IWDRAW(I)=IWD(I)
   26 CONTINUE
C
C*    CALL CASEUP1(IWDRAW)
C
C*    CALL CASEUP(IWD)
C   
      DO 30 I=1,20
      LINE(I,NOL)=IWDRAW(I)
   30 CONTINUE
      ISORN(NOL)=ISORL
C   
      IF(KMD.NE.'ENDL') GO TO 20
C
      IF(IERR.NE.0)    GO TO 500
C   
      WRITE(LOGUP,90)
   90 FORMAT(1H ,'************ - LISTING OF EXPANDED LOOP FOLLOWS')
C   
C     ************************************************************
C     PROCESS LOOP-LIST 
C     ************************************************************
C
      ISOSAV=ISORL
C
      DO 300 NL=1,NLOO
C
      CALL LABLOOP('SET ',IWD)
C
      DO 280 N=1,NOL
C
      ISORL=ISORN(N)
C
      IF(N.EQ.1.AND.NL.GT.1) GO TO 280
C   
      DO 220 I=1,20
      IWD(I)=LINE(I,N)
      IWDRAW(I)=IWD(I)
  220 CONTINUE
C
      CALL LISSOR('GEN ',IWD)
C
      CALL LABLOOP('SUB ',IWD)
C
      CALL CASEUP(IWD)
C   
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C   
      IDONE='    '
      IERR=0
C   
      CALL CALLER(IDONE,IERR)
C
      IF(IERR.NE.0) GO TO 540
C
      IF(IDONE.NE.'YES ') GO TO 530
C
  280 CONTINUE
  300 CONTINUE
C
      WRITE(LOGUP,305)
  305 FORMAT(1H ,'************ - END OF EXPANDED LOOP LISTING')
      GO TO 610
C   
  500 WRITE(CMSSG,505)
  505 FORMAT(1H ,'LOOP NOT EXECUTED')
      GO TO 600
C
  520 WRITE(CMSSG,525)
  525 FORMAT('END-OF-FILE ENCOUNTERED LOOKING FOR ENDL')
      GO TO 600
C
  530 WRITE(CMSSG,535)
  535 FORMAT('COMMAND NOT RECOGNIZED - LOOP EXECUTION ABORTED')
      GO TO 600
C
  540 WRITE(CMSSG,545)
  545 FORMAT(1H ,'LOOP EXECUTION ABORTED')
C
  600 CALL ERRLOG(LOGUT,LOGUP)   
  610 ISORL=ISOSAV
      RETURN
      END
C$PROG LSNB
      FUNCTION LSNB(IBY,IA,IB)
C
      BYTE     IBY(*)
C
      BYTE     X20,X21,X3B
C
      DATA     X20,X21,X3B/'20'X,'21'X,'3B'X/
C
      SAVE
C
C     FUNCTION TO RETURN LOCATION OF LAST NON-BLANK
C     TERMINATES ON ; OR !
C
      LNB=0
      DO 10 I=IA,IB
      IF(IBY(I).EQ.X3B) GO TO 50
      IF(IBY(I).EQ.X21) GO TO 50
      IF(IBY(I).NE.X20) LNB=I
   10 CONTINUE
C
   50 LSNB=LNB
      RETURN
      END
C$PROG LWDMOD3
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE LWDMOD3(LWD3)
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C
      INTEGER*4 LWD3(3,40)
C
      CHARACTER*8  CTMP
C
      INTEGER*4    ITMP(2)
C
      EQUIVALENCE (CTMP,ITMP)
C
      INTEGER*4    BLANK
      DATA         BLANK/'    '/
C
      SAVE
C
C     ************************************************************
C     REPLACE SYMBOLS IN 12-BYTE FIELDS WITH NUMERIC VALUES
C     ************************************************************
C
      DO 50 J=3,4
      IF(LWD3(2,J).NE.BLANK) GO TO 50
      DO 20 I=1,NSYM
      IF(LWD3(1,J).NE.ISYN(I)) GO TO 20
      WRITE(CTMP,10)ISYV(I)
   10 FORMAT(I8)
      LWD3(1,J)=ITMP(1)
      LWD3(2,J)=ITMP(2)
      GO TO 50
   20 CONTINUE
   50 CONTINUE
      RETURN
      END
C$PROG MESSLOG
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      SUBROUTINE MESSLOG(LUA,LUB)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4 MSS20(20),MSS26(26)
C
      EQUIVALENCE (MSS20(1),MSSG(1)),(MSS26(1),MSSG(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(LUA.LE.0) GO TO 20
C
      WRITE(LUA,10)MSS20
   10 FORMAT(1H ,19A4,A3)
C
   20 IF(LUB.LE.0.OR.LISFLG.NE.'LON ') GO TO 100
C
      WRITE(LUB,55)MSS26
   55 FORMAT(1H ,26A4)
C
  100 CMSSG=' '
C
      RETURN
      END
C$PROG MODCOD
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ******************************************************************
C     Modified by JWK to add AD413 codes  09 and 29  06/28/00
C
      SUBROUTINE MODCOD(IWD,ICODE,IERR)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      COMMON/PACO/ KODLO(4),KODHI(4)
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(3),NAME(3,34),CODE(34)
C
      CHARACTER*12 CNAME(34)
C
      EQUIVALENCE (CNAME,NAME)
C
      DATA (CNAME(I),CODE(I),I=1,34)/
     &'LRS_4300    ',1,              !FERRA   - LECROY   ADC
     &'GAN_812F    ',2,              !FERRA   - GANELEC  TDC
     &'SILENA_4418 ',3,              !FERRA   - SILENA   ADC
     &'LRS_3351    ',3,              !FERRA   - like SILENA-4418
     &'LRS_3371    ',3,              !FERRA   - like SILENA-4418
     &'MCSQ_FER    ',4,              !FERRA   - test-only---
     &'LRS_3377    ',5,              !FERRA
     &'BAKLASH     ',6,              !FERRA   - Clover   ADC
     &'BAKLASH2    ',7,              !FERRA   - Clover   ADC
     &'BAKLASH3    ',8,              !FERRA   - Clover   ADC
     &'AD_413      ',9,              !FERA    - ORTEC    ADC, JWK Added
     &'LRS_1885    ',11,             !FASTBUS - LECROY   ADC
     &'PHIL_10C6   ',12,             !FASTBUS - PHILLIPS TDC
     &'LRS_1872    ',13,             !FASTBUS - LECROY   TDC
     &'LRS_1875    ',13,             !FASTBUS - LECROY   TDC
     &'LRS_1881    ',14,             !FASTBUS - LECROY   ADC
     &'LRS_1877    ',15,             !FASTBUS - LECROY   ADC
     &'MCSQ_FAS    ',16,             !FASTBUS - test-only---
     &'PHIL_7164   ',21,             !CAMAC   - PHILLIPS ADC
     &'PHIL_7166   ',21,             !CAMAC   - PHILLIPS ADC
     &'PHIL_7167   ',21,             !CAMAC   - PHILLIPS ADC
     &'PHIL_7186   ',22,             !CAMAC   - PHILLIPS TDC
     &'PHIL_7187   ',22,             !CAMAC   - PHILLIPS TDC
     &'LRS_2277    ',23,             !CAMAC   - LECROY   TDC
     &'SILENA_4418C',24,             !CAMAC   - SILENA   ADC
     &'LRS_3351C   ',24,             !CAMAC   - like SILENA-4418C
     &'LRS_3371C   ',24,             !CAMAC   - like SILENA-4418C
     &'MCSQ_CAM    ',25,             !CAMAC   - test-only---
     &'LRS_4300C   ',26,             !CAMAC   - LECROY   ADC
     &'LRS_3377C   ',27,             !CAMAC   - LECROY   ADC
     &'XIA_TIME    ',28,             !CAMAC   - XIA TIME
     &'AD_413C     ',29,             !CAMAC   - ORTEC    ADC, JWK added
     &'CAEN-775    ',41,             !VME     - CAEN 32-CHANNEL TDC
     &'CAEN-785    ',42/             !VME     - CAEN 32-CHANNEL ADC
C
      DATA NNAME/34/
C
      DATA KODLO/1,  11,  21,   41/
      DATA KODHI/9,  16,  29,   42/
C
      SAVE
C
C     ------------------------------------------------------------------
C     TESTS IWD AGAINST MODULE-TYPE TABLE & RETURNS CODE OR ERROR
C     ------------------------------------------------------------------
C
      DO 20 J=1,NNAME
      DO 10 I=1,3
      IF(IWD(I).NE.NAME(I,J)) GO TO 20
   10 CONTINUE
      IERR=0
      ICODE=CODE(J)
      RETURN
C
   20 CONTINUE
C
      WRITE(CMSSG,30)IWD
   30 FORMAT('ILLEGAL MODULE NAME = ',3A4)
      CALL ERRLOG(LOGUT,LOGUP)
      ICODE=0
      IERR=1
      RETURN
      END
C$PROG MODSAV
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/24/2000
C     ************************************************************
C
      SUBROUTINE MODSAV
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC1/ KINMOD,MODKOD,NAMOD(3),MODATA(2,12)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C     ------------------------------------------------------------------
C
      CHARACTER*4  USED
C
      DATA         NUMT/0/
C
      INTEGER*4    X8000
      DATA         X8000/'8000'X/
C
      SAVE
C
C     ************************************************************
C     SAVES ONE MODULE-DATA-SET PER CALL AS DEFINED BELOW
C     *************************************************************
C     KINMOD      = MODULE TYPE ($LAT, $CAM, $FER, $FAS...)
C     MODKOD      = MODULE MODEL CODE (1,2,3..) FOR FERRA & FASTBUS
C     NAMOD       = MODULE NAME (FOR EXAMPLE - ADC1) UP TO 12 BYTES
C     MODATA(1,1) = MODULE-NAME FIRST INDEX
C     MODATA(2,1) = MODULE-NAME INDEX INCREMENT
C     MODATA(1,2) = CRATE#
C     MODATA(1,3) = SLOT#
C     MODATA(1,4) = FIRST SUB-ADDRESS TO READ
C     MODATA(2,4) = LAST  SUB-ADDRESS TO READ
C     MODATA(1,5) = FUNCTION CODE FOR READ
C     MODATA(1,6) = CLASS#
C     MODATA(1,7) = DETECTOR#, FIRST VALUE
C     MODATA(2,7) = DETECTOR#, INCREMENT
C     MODATA(1,8) = ENTRY#
C     MODATA(1,9) = FUNCTION CODE FOR CLEAR
C     MODATA(1,10)= SUB-ADDRRESS FOR CLEAR
C     MODATA(1,11)= DELAY TIME BEFORE READ (no longer used *******)
C     MODATA(1,12)= ID-NUMBER, FIRST
C     MODATA(2,12)= ID-NUMBER, INCREMENT
C     *************************************************************
C
      NDO=MODATA(2,4)-MODATA(1,4)+1   !# OF SUB-ADDRESSES
C
      IA  =MODATA(1,4)                !SUB-ADDRESS, FIRST READ
      IINC=1                          !SUB-ADDRESS, INCREMENT
C
      JA  =MODATA(1,1)                !MODULE-NAME, FIRST INDEX
      JINC=MODATA(2,1)                !MODULE-NAME, INDEX INCREMENT
C
      KA  =MODATA(1,7)                !DETECTOR#,   FIRST VALUE
      KINC=MODATA(2,7)                !DETECTOR#,   INCREMENT
C
      LA  =MODATA(1,12)               !ID-NUMBER,   FIRST VALUE
      LINC=MODATA(2,12)               !ID-NUMBER,   INCREMENT
C
      N=NUMT
      DO 100 I=1,NDO
      N=N+1
C
      IF(N.GT.TMX) THEN
      WRITE(CMSSG,10)N
   10 FORMAT('/PAC2/ ARRAY OVERFLOW AT N =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      N=TMX
      ENDIF
C
      IF(MODKOD.NE.28)        GO TO 50
      IF(IA.GE.0.AND.IA.LE.2) GO TO 50
C
      WRITE(CMSSG,20)IA,NAMOD
   20 FORMAT('Sub-addr = ',I4,' is illegal for ',3A4)
      CALL ERRLOG(LOGUT,LOGUP)
C
C
   50 NAMO(1,N)=NAMOD(1)
      NAMO(2,N)=NAMOD(2)
      NAMO(3,N)=NAMOD(3)
      NAMO(4,N)=JA
      KIMO(N)=KINMOD
      CRAT(N)=MODATA(1,2)
      SLOT(N)=MODATA(1,3)
      SUBA(N)=IA
      FRED(N)=MODATA(1,5)
      FCLR(N)=MODATA(1,9)
      ACLR(N)=MODATA(1,10)
C
      LTT=LA
      IF(LTT.NE.-1) LTT=LTT+X8000
      IDNM(N)=LTT
      MOTY(1,N)=MODKOD
      USED(N)='NO  '
C
      CALL CKUNIQUE(N)
      CALL CKMOTY(KINMOD,MOTY(1,N))
      CALL CKCNAS(N)
C
      IA=IA+IINC
      JA=JA+JINC
      KA=KA+KINC
      LA=LA+LINC
C
  100 CONTINUE
      NUMT=N
      RETURN
      END
C$PROG MODULY
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE MODULY(KIND,LIST,NUM)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      INTEGER*4    LIST(20)
C
      CHARACTER*4  USED
C
      SAVE
C
      NUM=0
      DO 100 I=1,NUMT
      IF(KIMO(I).NE.KIND)   GO TO 100
      IF(MOTY(1,I).LE.0)    GO TO 100
      IF(USED(I).EQ.'YES ') GO TO 100
      DO 20 J=1,NUM
      IF(MOTY(1,I).EQ.LIST(J)) GO TO 100
   20 CONTINUE
      NUM=NUM+1
      LIST(NUM)=MOTY(1,I)
  100 CONTINUE
      RETURN
      END
C$PROG MOTABLE
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ************************************************************
C
      SUBROUTINE MOTABLE
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXI=2000)
      PARAMETER (MXR=10)
C
      COMMON/PAC1/ KINMOD,MODKOD,NAMOD(3),MODATA(2,12)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PAC4/ LATN(MXI),MASK(MXI),KINS(MXI),
     &             TBLI(2,MXI),TBLN(2,MXI),NXTI(2,MXI),
     &             NUNDX(MXI),NTS
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C
      COMMON/PAC6/ LABLIS(2,MXI),LABVAL(MXI),NLA 
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PAC8/ KLASID(50),KLASND(50),KLASMU(50),NKLAS,LKIND
C
      COMMON/PAC9/ NAMP(3,9),LATW(200,9),BITN(200,9),NPB(9),NPAT
C
      COMMON/PACA/ JCRA(MXI),JSLO(MXI),JSUB(MXI),JFRE(MXI),
     &             JIDN(MXI),NNAF
C
      COMMON/PACB/ CNAFI(200),DATAI(200),NNAFI,
     &             CNAFQ(200),DATAQ(200),NNAFQ,
     &             CNAFR(200),DATAR(200),NNAFR
C
      COMMON/PACD/ FERID(32,576),FERC(576),FERN(576),FERT(576),NFER,
     &             FERERID
C
      COMMON/PACE/ FASID(256,32),FASC(32), FASN(32), FAST(32), NFAS,
     &             FASERID
C
      COMMON/PACF/ CLRC(200),CLRN(200),CLRA(200),CLRF(200),NCLR
C
      COMMON/PACG/ CRATLST(50),NCRAT
C
      COMMON/PACN/ CAMID(32,32,8),CAMC(256),CAMN(256),CAMT(256),NCAM,
     &             CAMERID
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
C     ------------------------------------------------------------------
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      BYTE BYTTST(4)
C
      EQUIVALENCE (IWDTST,BYTTST)
C
      CHARACTER*4  KIMO
C
      SAVE
C
C     ************************************************************
C     SET DEFAULT (ERROR) PARAMETER ID'S FOR FASTBUS & FERA
C     AND LOAD ID-TABLES WITH DEFAULT ID'S 
C     ************************************************************
C
      MAXID=0
      DO 30 I=1,NUMT
C
      IF(CAMERID.GT.0.AND.CAMERID.EQ.IDNM(I)) THEN
      WRITE(CMSSG,10)CAMERID
   10 FORMAT('SPECIFIED CAMAC ERROR-ID SAME AS NORMAL-ID =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
                                              ENDIF
C
      IF(FERERID.GT.0.AND.FERERID.EQ.IDNM(I)) THEN
      WRITE(CMSSG,15)FERERID
   15 FORMAT('SPECIFIED FERA ERROR-ID SAME AS NORMAL-ID =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
                                              ENDIF
C
      IF(FASERID.GT.0.AND.FASERID.EQ.IDNM(I)) THEN
      WRITE(CMSSG,20)FASERID
   20 FORMAT('SPECIFIED FASTBUS ERROR-ID SAME AS NORMAL-ID =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
                                              ENDIF
      IF(IDNM(I).GT.MAXID) MAXID=IDNM(I)
   30 CONTINUE
C
      MAXXID=MAXID
      IF(CAMERID.GT.MAXXID) MAXXID=CAMERID
      IF(FASERID.GT.MAXXID) MAXXID=FASERID
      IF(FERERID.GT.MAXXID) MAXXID=FERERID
C
      IF(CAMERID.LE.0) CAMERID=MAXXID+1
      IF(FASERID.LE.0) FASERID=MAXXID+2
      IF(FERERID.LE.0) FERERID=MAXXID+3
C
      DO 50 K=1,8
      DO 45 J=1,32
      DO 40 I=1,32
      CAMID(I,J,K)=CAMERID
   40 CONTINUE
   45 CONTINUE
   50 CONTINUE
C
      DO 70 J=1,576
      DO 65 I=1,32
      FERID(I,J)=FERERID
   65 CONTINUE
   70 CONTINUE
C
      DO 80 J=1,32
      DO 75 I=1,256
      FASID(I,J)=FASERID
   75 CONTINUE
   80 CONTINUE
C
C     ************************************************************
C     BUILD CAMAC, FERA & FASTBUS MODULE TABLES AND
C     LOAD SPECIFIED ID'S INTO ID-TABLES
C     ************************************************************
C
      NCAM=0
      DO 100 I=1,NUMT
      IF(KIMO(I).NE.'$CAM') GO TO 100
      CALL CNTADD(CRAT(I),SLOT(I),MOTY(1,I),CAMC,CAMN,CAMT,NCAM)
      II=SUBA(I)+1
      JJ=SLOT(I)+1
      KK=CRAT(I)+1
      CAMID(II,JJ,KK)=IDNM(I)
  100 CONTINUE
C
      NFER=0
      DO 150 I=1,NUMT
      IF(KIMO(I).NE.'$FER') GO TO 150
      CALL CNTADD(CRAT(I),SLOT(I),MOTY(1,I),FERC,FERN,FERT,NFER)
      II=SUBA(I)+1
      JJ=32*CRAT(I)+SLOT(I)
      FERID(II,JJ)=IDNM(I)
  150 CONTINUE
C
      NFAS=0
      DO 200 I=1,NUMT
      IF(KIMO(I).NE.'$FAS') GO TO 200
      CALL CNTADD(CRAT(I),SLOT(I),MOTY(1,I),FASC,FASN,FAST,NFAS)
      II=SUBA(I)+1
      JJ=SLOT(I)+1
      FASID(II,JJ)=IDNM(I)
  200 CONTINUE
C
C     ************************************************************
C     BUILD CAMAC CLEAR-TABLES
C     ************************************************************
C
      NCLR=0
      DO 300 I=1,NUMT
C
      IF(KIMO(I).EQ.'$LAT') GO TO 250
      IF(KIMO(I).EQ.'$CAM') GO TO 250
      IF(KIMO(I).EQ.'$FER') GO TO 250
                            GO TO 300
C
  250 IF(FCLR(I).LT.0) GO TO 300
      IF(ACLR(I).EQ.-1) ACLR(I)=0
      IF(ACLR(I).LT.0) GO TO 300
C
      CALL CLRADD(CRAT(I),SLOT(I),ACLR(I),FCLR(I),
     &            CLRC,   CLRN,   CLRA,   CLRF,   NCLR)
C
  300 CONTINUE
C
C     ************************************************************
C     BUILD CAMAC CRATE-LIST
C     ************************************************************
C
      NCRAT=0
      DO 330 I=1,NUMT
C
      IF(KIMO(I).NE.'$LAT'.AND.
     &   KIMO(I).NE.'$CAM'.AND.
     &   KIMO(I).NE.'$FER') GO TO 330
C
      DO 320 J=1,NCRAT
      IF(CRAT(I).EQ.CRATLST(J)) GO TO 330
  320 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CRAT(I)
  330 CONTINUE
C
      DO 350 I=1,NNAFI
      IWDTST=CNAFI(I)
      CTST=BYTTST(4)
      DO 340 J=1,NCRAT
      IF(CTST.EQ.CRATLST(J)) GO TO 350
  340 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CTST
  350 CONTINUE
C
      DO 370 I=1,NNAFQ
      IWDTST=CNAFQ(I)
      CTST=BYTTST(4)
      DO 360 J=1,NCRAT
      IF(CTST.EQ.CRATLST(J)) GO TO 370
  360 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CTST
  370 CONTINUE
C
      DO 390 I=1,NNAFR
      IWDTST=CNAFR(I)
      CTST=BYTTST(4)
      DO 380 J=1,NCRAT
      IF(CTST.EQ.CRATLST(J)) GO TO 390
  380 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CTST
  390 CONTINUE
C
      DO 410 I=1,NXIA
      CTST=XIAC(I)
      DO 400 J=1,NCRAT
      IF(CTST.EQ.CRATLST(J)) GO TO 410
  400 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CTST
  410 CONTINUE
C
C
      RETURN
      END
C$PROG NAMLOC
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE NAMLOC(NAMLST,NAMTST,NLST,NDX)
C
      INTEGER*4 NAMLST(4,*),NAMTST(4)
C
      SAVE
C
      NDX=0
      DO 20 J=1,NLST
      DO 10 I=1,4
      IF(NAMTST(I).NE.NAMLST(I,J)) GO TO 20
   10 CONTINUE
      NDX=J
      RETURN
   20 CONTINUE
      RETURN
      END
C$PROG NEWMSK
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE NEWMSK(PATNDX,MSK)
C
      INTEGER*4 PATNDX
C
      SAVE
C
      IF(MSK.GE.32768) THEN
                       PATNDX=PATNDX+1
                       MSK=1
                       RETURN
                       ENDIF
C
      MSK=2*MSK
      RETURN
      END
C$PROG NXBL
      FUNCTION NXBL(IBY,IA,IB)
C
      BYTE     IBY(*)
C
      BYTE     X20,X21,X3B
C
      DATA     X20,X21,X3B/'20'X,'21'X,'3B'X/
C
      SAVE
C
C     FUNCTION TO RETURN LOCATION OF NEXT BLANK
C     TERMINATES ON ; OR !
C
      DO 10 I=IA,IB
      IF(IBY(I).EQ.X20) GO TO 20
      IF(IBY(I).EQ.X3B) GO TO 15
      IF(IBY(I).EQ.X21) GO TO 15
   10 CONTINUE
   15 I=0
   20 NXBL=I
      RETURN
      END
C$PROG NXDG
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      FUNCTION NXDG(IBY,IA,IB)
C
      BYTE     IBY(*)
C
      BYTE     X30,X39
C
      DATA     X30,X39/'30'X,'39'X/
C
      SAVE
C
C     ************************************************************
C     RETURNS LOCATION OF FIRST "DIGIT" IN IBY BETWEEN IA & IB
C     RETURNS 0, IF NONE FOUND
C     ************************************************************
C
      DO 10 I=IA,IB
      IF(IBY(I).GE.X30.AND.IBY(I).LE.X39) GO TO 20
   10 CONTINUE
      NXDG=0
      RETURN
C
   20 NXDG=I
      RETURN
      END
C$PROG NXNB
      FUNCTION NXNB(IBY,IA,IB)
C
      BYTE     IBY(*)
C
      BYTE     X20,X21,X3B
C
      DATA     X20,X21,X3B/'20'X,'21'X,'3B'X/
C
      SAVE
C
C     FUNCTION TO RETURN LOCATION OF NEXT NON-BLANK
C     TERMINATES ON ; OR !
C
      DO 10 I=IA,IB
      IF(IBY(I).EQ.X3B) GO TO 15
      IF(IBY(I).EQ.X21) GO TO 15
      IF(IBY(I).NE.X20) GO TO 20
   10 CONTINUE
   15 I=0
   20 NXNB=I
      RETURN
      END
C$PROG PACNIT
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 01/02/2003
C     ************************************************************
C
      SUBROUTINE PACNIT(KLIS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      COMMON/III/  LIN,LCM,LCI
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PACM/ NAMPAC(20)
C
      CHARACTER*80 CARG(2),NAMFIL,NAMLOG,NAMTMP,NAMTAB,NAMPOB
C
      CHARACTER*80 CNAMINQ,FFULL
C
      INTEGER*4    NARG(20,2)
C
      EQUIVALENCE (NARG,CARG),(CNAMINQ,NAMPAC)
C
      DATA ISORL,IEXPL,LISTYP,NERR/0,0,'ALL ',0/
C
      DATA LCI,LIN,LCM/1,1,1/
C
      INTEGER*4  RECLVALU
C
      DATA       NAMPROG/'PACO','R   '/
C
      SAVE
C
C     *************************************************************
C     INITIALIZE SOME STUFF AND OPEN FILES
C     *************************************************************
C
C
      CMSSG=' '
      LOGUT=6
      LOGUP=7
      LISFLG='LON '
      MSGF='    '
C
      NUMARG=IARGC()
      CARG(1)=' '
      CARG(2)=' '
      IF(NUMARG.GT.2) GO TO 100
      CALL GETARG(1,CARG(1))
      IF(NUMARG.EQ.2) CALL GETARG(2,CARG(2))
C
      KLIS=NARG(1,2)
      CALL KASUP4(KLIS)
C
      IA=1
      IB=NXBL(NARG(1,1),IA,79)-1
      NAMFIL=CARG(1)
      NAMFIL(IB+1:)='.pac'
C
      CALL STRIPATH(NARG(1,1),80)
      IB=NXBL(NARG(1,1),IA,79)-1
C
      NAMLOG=CARG(1)
      NAMTMP=CARG(1)
      NAMTAB=CARG(1)
      NAMPOB=CARG(1)
      NAMLOG(IB+1:)='.lst'
      NAMTMP(IB+1:)='.tmp'
      NAMTAB(IB+1:)='.tab'
      NAMPOB(IB+1:)='.pob'
C
      OPEN(UNIT            = 1,
     &     STATUS          = 'OLD',
     &     ACCESS          = 'SEQUENTIAL',
     &     FILE            = NAMFIL,
     &     ERR             = 200)
C
      INQUIRE(UNIT=1,NAME=CNAMINQ)                !GET SOURCE FILENAME
C
      IF(CNAMINQ(1:1).NE.'/') THEN
      CALL GETCWD(FFULL)
      CALL STRAPPEND(FFULL,'/')
      CALL STRAPPEND(FFULL,CNAMINQ)
      CNAMINQ=FFULL
      ENDIF
C
CX    WRITE(6,777)CNAMINQ
CX777 FORMAT(1H ,A)
C
      OPEN(UNIT      = 7,                         !OPEN/DELETE LOG-FILE
     &     FILE      = NAMLOG,
     &     STATUS    = 'UNKNOWN',
     &     IOSTAT    = IOS)
C
      OPEN(UNIT      = 8,                         !OPEN/DELETE TMP-FILE
     &     FILE      = NAMTMP,
     &     STATUS    = 'UNKNOWN',
     &     IOSTAT    = IOS)
C
      OPEN(UNIT      = 9,                         !OPEN/DELETE TAB-FILE
     &     FILE      = NAMTAB,
     &     STATUS    = 'UNKNOWN',
     &     IOSTAT    = IOS)
C
      OPEN(UNIT      = 10,                        !OPEN/DELETE OUT-FILE
     &     FILE      = NAMPOB,
     &     STATUS    = 'UNKNOWN',
     &     IOSTAT    = IOS)
C
      CLOSE(UNIT=7, DISP='DELETE')
      CLOSE(UNIT=8, DISP='DELETE')
      CLOSE(UNIT=9, DISP='DELETE')
      CLOSE(UNIT=10,DISP='DELETE')
C
      OPEN(UNIT      = 7,                         !CREATE NEW LOG-FILE
     &     FILE      = NAMLOG,
     &     STATUS    = 'NEW',
     &     IOSTAT    = IOS)
C
      OPEN(UNIT      = 8,                         !CREATE NEW TMP-FILE
     &     FILE      = NAMTMP,
     &     STATUS    = 'NEW',
     &     IOSTAT    = IOS)
C
      OPEN(UNIT      = 9,                         !CREATE NEW TAB-FILE
     &     FILE      = NAMTAB,
     &     STATUS    = 'NEW',
     &     IOSTAT    = IOS)
C
      OPEN(UNIT       = 10,                        !CREATE NEW OUT-FILE
     &     FILE       = NAMPOB,
     &     STATUS     = 'NEW',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(262144),
     &     FORM       = 'UNFORMATTED',
     &     IOSTAT     = IOS)
C
      RETURN
C
  100 WRITE(6,105)
  105 FORMAT(1H ,'ERROR IN FILE SPECIFICATION')
      STOP 1
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'ERROR OPENING PAC-FILE')
      STOP 1
      END
C$PROG PASS1
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ************************************************************
C
      SUBROUTINE PASS1
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/ML02/ IWDRAW(20)
C
      COMMON/III/  LIN,LCM,LCI
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      INTEGER*4 LASLAT,LASCAM,FIRRGA,LASRGA,FIRCGA,LASCGA,FIRCDN
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      BYTE         IBY(80),DOL
C
      DATA                 DOL/'$'/
C
      CHARACTER*4  KMD,IWD1
C
      EQUIVALENCE (KMD,LWD(1,1)),(IBY,IWD),(IWD1,IWD(1))
C
      SAVE
C
C     ************************************************************
C     READ SOURCE FILE & REQUEST PROCESSING OF $xxx TYPES ONLY
C     ************************************************************
C
      REWIND LIN
C
      LASDOL=0            !LINE# FOR LAST  $-DIRECTIVE
      LASLAT=0            !LINE# FOR LAST  $LAT  ENTRY
      LASCAM=0            !LINE# FOR LAST  $CAM  ENTRY
      FIRRGA=0            !LINE# FOR FIRST $RGAT ENTRY
      LASRGA=0            !LINE# FOR LAST  $RGAT ENTRY
      FIRCGA=0            !LINE# FOR FIRST $CGAT ENTRY
      LASCGA=0            !LINE# FOR LAST  $CGAT ENTRY
      FIRCDN=0            !LINE# FOR FIRST $CDN  ENTRY
C
      NL=0
   50 NL=NL+1
      READ(LIN,110,END=60)IWD
C
      CALL LEGLASS(IWD,NL)
C
      IF(IBY(1).NE.DOL)  GO TO 50
C
                         LASDOL=NL
      IF(IWD1.EQ.'$LAT') LASLAT=NL
      IF(IWD1.EQ.'$CAM') LASCAM=NL
C
      IF(IWD1.EQ.'$RGA') THEN
                         LASRGA=NL
      IF(FIRRGA.EQ.0)    FIRRGA=NL
                         ENDIF
C
      IF(IWD1.EQ.'$CGA') THEN
                         LASCGA=NL
      IF(FIRCGA.EQ.0)    FIRCGA=NL
                         ENDIF
C
      IF(IWD1.EQ.'$CDN') THEN
      IF(FIRCDN.EQ.0)    FIRCDN=NL
                         ENDIF
C
      GO TO 50
C
   60 REWIND LIN
      NL=0
C
      IF(FIRRGA.LT.LASLAT.OR.FIRRGA.LT.LASCAM) THEN
      WRITE(CMSSG,65)
   65 FORMAT('$RGAT spec out of order - must follow $LATs & $CAMs')
      CALL ERRLOG(LOGUT,LOGUP)
                                               ENDIF
C
      IF(FIRCGA.LT.LASRGA) THEN
      WRITE(CMSSG,70)
   70 FORMAT('$CGAT spec out of order - must follow $RGATs')
      CALL ERRLOG(LOGUT,LOGUP)
                           ENDIF
C
      IF(FIRCDN.LT.LASCGA) THEN
      WRITE(CMSSG,75)
   75 FORMAT('$CDN spec out of order - must follow $CGAts')
      CALL ERRLOG(LOGUT,LOGUP)
                           ENDIF
C
  100 NL=NL+1
      IF(NL.GT.LASDOL) GO TO 1000
C
      READ(LIN,110,END=1000)IWD
  110 FORMAT(20A4)
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
  115 CONTINUE
      CALL CASEUP1(IWDRAW)
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      CALL LISSOR('SOR ',IWDRAW)
C
      IF(IBY(1).NE.DOL) GO TO 100   
C
      CALL KASUP4(IWD)
C
      IF(KMD.EQ.'$INI') GO TO 210
      IF(KMD.EQ.'$DUN') GO TO 210
      IF(KMD.EQ.'$RUN') GO TO 210
C
      IF(KMD.EQ.'$DLA') GO TO 220
C
      IF(KMD.EQ.'$CAM') GO TO 230
      IF(KMD.EQ.'$FER') GO TO 230
      IF(KMD.EQ.'$FAS') GO TO 230
      IF(KMD.EQ.'$LAT') GO TO 230
C
      IF(KMD.EQ.'$PAT') GO TO 240
      IF(KMD.EQ.'$RIF') GO TO 250
      IF(KMD.EQ.'$KIL') GO TO 260
      IF(KMD.EQ.'$DID') GO TO 270
C
      IF(KMD.EQ.'$RGA') GO TO 280
      IF(KMD.EQ.'$CGA') GO TO 280
      IF(KMD.EQ.'$CDN') GO TO 290
C
      IF(KMD.EQ.'$XIA') GO TO 300
      IF(KMD.EQ.'$VME') GO TO 310
C
      IF(KMD.EQ.'$CID') GO TO 320
C   
      WRITE(CMSSG,200)IWD(1)
      CALL ERRLOG(LOGUT,LOGUP)
  200 FORMAT(A4,' - DIRECTIVE NOT RECOGNIZED ******')
      GO TO 100
C
  210 CALL CANITQIT                   !PROCESS $INI, $DUN, $RUN 
      GO TO 100
C
  220 CALL DELAYX                     !Process $DLA
      GO TO 100
C
  230 CALL HARDASS(IWD)               !PROCESS $CAM, $FER, $FAS
      GO TO 100
C
  240 CALL PATTER(IWD)                !PROCESS $PAT ENTRY
      GO TO 100
C
  250 CALL RIFFER(IWD)                !PROCESS $RIF ENTRY
      GO TO 100
C
  260 CALL KILLER(IWD)                !PROCESS $KIL ENTRY
      GO TO 100
C
  270 CALL ERRORID                    !PROCESS $DID ENTRY
      GO TO 100
C
  280 CALL GATER(IWD)                 !PROCESS $RGAT & $CGAT ENTRYS
      GO TO 100
C
  290 CALL CNTDOWN(IWD)               !PROCESS $CDN ENTRY
      GO TO 100
C
  300 CALL XIAASS(IWD)                !PROCESS $XIA ENTRY
      GO TO 100
C
  310 CALL VMEASS(IWD)                !PROCESS $VME ENTRY
      GO TO 100
C
  320 CALL CLOCASS(IWD)               !PROCESS $CID ENTRY
      GO TO 100
C
 1000 RETURN
      END
C$PROG PASS2
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE PASS2
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/ML02/ IWDRAW(20)
C
      COMMON/III/  LIN,LCM,LCI
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      BYTE         DOLR,SEMI,EXCL,IBY(80)
C
      DATA         DOLR,SEMI,EXCL/'$',';','!'/
C
      CHARACTER*4  KMD,IDONE,CIWD(20)
C
      EQUIVALENCE (KMD,LWD(1,1)),(IBY,IWD),(CIWD,IWD)
C
      SAVE
C
C     ************************************************************
C     READ SOURCE-FILE & PROCESS CONDITIONAL READOUT CODE
C     ************************************************************
C
      REWIND LIN
      NL=0
C
  100 READ(LIN,110,END=300)IWD
  110 FORMAT(20A4)
      NL=NL+1
C
      CALL LEGLASS(IWD,NL)
C
      IF(IBY(1).EQ.DOLR) GO TO 100
      IF(IBY(1).EQ.SEMI) GO TO 100
      IF(IBY(1).EQ.EXCL) GO TO 100
C
      CALL LISSOR('SOR ',IWD)
C
      NNON=0
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
      IF(CIWD(I).NE.'    ') NNON=NNON+1
  115 CONTINUE
      IF(NNON.LE.0) GO TO 100
C
      CALL CASEUP1(IWDRAW)
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,7,80,NTER)
C
      IDONE='    '
      IERR=0
C
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPEX(LIN,LCI)
                        GO TO 100
                        ENDIF
C
      CALL CALLER(IDONE,IERR)
C
      IF(IERR.NE.0)       GO TO 100
      IF(IDONE.EQ.'YES ') GO TO 100
C   
      WRITE(CMSSG,200)
      CALL ERRLOG(LOGUT,LOGUP)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      GO TO 100
C
  300 CALL CONDICO
      RETURN
      END
C$PROG PATMSK
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE PATMSK(NAME,IDX,LWN,MSK)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/PAC9/ NAMP(3,9),LATW(200,9),BITN(200,9),NPB(9),NPAT
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 NAME(3)
C
      SAVE
C
C     ************************************************************
C     GIVEN PAT-NAME "NAME" AND PAT-INDEX "IDX"
C     CONSTRUCT LATCH-WORD-NUMBER "LWN" AND MASK "MSK"
C     ************************************************************
C
      LWN=1
      MSK=0
C
      DO 20 K=1,NPAT
      DO 10 I=1,3
      IF(NAME(I).NE.NAMP(I,K)) GO TO 20
   10 CONTINUE
      GO TO 50
   20 CONTINUE
      GO TO 100
C
   50 IF(IDX.GT.NPB(K)) GO TO 110
C
      LWN=LATW(IDX,K)
      MSK=0
      KBIT=BITN(IDX,K)-1
      MSK=IBSET(MSK,KBIT)
      RETURN
C
  100 WRITE(CMSSG,105)NAME
  105 FORMAT('BIT PATTERN NAME NOT DEFINED = ',3A4)
      GO TO 200
  110 WRITE(CMSSG,115)NAME,IDX
  115 FORMAT('FOR BIT PATTERN - ',3A4,' UNDEFINED INDEX =',I5)
C
  200 CALL ERRLOG(LOGUT,LOGUP)
      RETURN 
      END
C$PROG PATTER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE PATTER(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PAC9/ NAMP(3,9),LATW(200,9),BITN(200,9),NPB(9),NPAT
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      INTEGER*4 NAMLAT(3,50),INDXL(50),NLAT
C
      INTEGER*4 PATNAM(3),LATNAM(3),LATORD(50),
     &          LOBIT(50),HIBIT(50),IV(3)
C
      DATA NCALL,NPAT,NLAT,MXPAT,MXBIT/0,0,0,9,200/
C
      DATA SETW/'SETW'/
C
      CHARACTER*4  KIMO
C
      SAVE
C
      IF(NCALL.GT.0) GO TO 100
C
      DO 50 N=1,NUMT                        !LOOP ON FULL SOURCE TABLE
      IF(KIMO(N).NE.'$LAT') GO TO 50
      NLAT=NLAT+1
      NAMLAT(1,NLAT)=NAMO(1,N)                 !SAVE ALL LATCH-NAMES
      NAMLAT(2,NLAT)=NAMO(2,N)
      NAMLAT(3,NLAT)=NAMO(3,N)
      INDXL(NLAT)   =NAMO(4,N)                 !AND LATCH-NAME INDICES
   50 CONTINUE
      NCALL=1
C
  100 CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER) !SET RE-FMT WIDTH 10 12
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)    !RE-FORMAT
C
      DO 110 I=1,3                             !SAVE PAT-NAME
      PATNAM(I)=LWD(I,1)
  110 CONTINUE
      NFF=NF-1                                 !#FIELDS
      KDO=NFF/4                                !#LOOPS TO DO
      IF(4*KDO.NE.NFF) GO TO 500               !CHECK FOR MULT OF 4
C
      DO 130 N=1,NPAT                          !CHECK FOR PAT-NAME
      DO 120 I=1,3                             !ALREADY DEFINED
      IF(PATNAM(I).NE.NAMP(I,N)) GO TO 130
  120 CONTINUE
      GO TO 150                                !IF YES, SAVE INDEX
  130 CONTINUE
      NPAT=NPAT+1                              !IF NO,  ADD TO LIST
C
      IF(NPAT.GT.MXPAT) THEN
      WRITE(CMSSG,135)NPAT
  135 FORMAT('MAX# PAT DEFINITIONS EXCEEDED AT NPAT =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NPAT=MXPAT
                        ENDIF
C
      N=NPAT
      DO 140 I=1,3
      NAMP(I,N)=PATNAM(I)
  140 CONTINUE
C
  150 JPAT=N
      JJ=1
      DO 300 K=1,KDO                           !LOOP ON #FIELDS
      JJ=JJ+1
C
      DO 160 I=1,3
      LATNAM(I)=LWD(I,JJ)                      !LOAD LATCH-NAME
  160 CONTINUE
C
      DO 170 I=1,3
      JJ=JJ+1
      CALL MILV3(LWD(1,JJ),IV(I),XV,KIND,IERR) !DECODE NUMERIC FIELDS
  170 CONTINUE
C
      LNNDX    =IV(1)                          !LATCH-NAME INDEX
      LOBIT(K) =IV(2)                          !MIN BIT TO USE
      HIBIT(K) =IV(3)                          !MAX BIT TO USE
C
      DO 200 LL=1,NLAT                         !LOOK FOR LATNAM IN 
C                                              !"LIBRARY"
      DO 180 II=1,3                            !CHECK NAMES
      IF(LATNAM(II).NE.NAMLAT(II,LL))GOTO 200 
  180 CONTINUE
      IF(LNNDX.NE.INDXL(LL))         GOTO 200  !CHECK NAME INDEX
      LATORD(K)=LL                             !SAVE LATCH ORDINAL
      GO TO 300                                !IF ALL MATCH
  200 CONTINUE
      GO TO 510                                !ERROR IF NO MATCH
  300 CONTINUE
C
      II=NPB(JPAT)
      DO 320 NN=1,KDO
      IBL=LOBIT(NN)
      IBH=HIBIT(NN)
      DO 310 I=IBL,IBH
      II=II+1
C
      IF(II.GT.MXBIT) THEN
      WRITE(CMSSG,305)II
  305 FORMAT('MAX# PAT-BITS EXCEEDED AT II =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      II=MXBIT
                      ENDIF
C
      LATW(II,JPAT)=LATORD(NN)
      BITN(II,JPAT)=I
  310 CONTINUE
  320 CONTINUE
      NPB(JPAT)=II
      GO TO 1000
C
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR IN $pat ENTRY')
      CALL ERRLOG(LOGUT,LOGUP)
      GO TO 1000
C
  510 WRITE(CMSSG,515)LATNAM,LNNDX
  515 FORMAT('LATCH-NAME, INDEX NOT FOUND = ',3A4,I8)
      CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
      RETURN
      END
C$PROG POBGEN
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ************************************************************
C
      SUBROUTINE POBGEN(LU,KLIS)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXI=2000)
      PARAMETER (MXC=500)
      PARAMETER (MXG=100)
      PARAMETER (MXD=100)
      PARAMETER (MXK=50)
      PARAMETER (MXR=10)
C
      COMMON/PAC1/ KINMOD,MODKOD,NAMOD(3),MODATA(2,12)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PAC4/ LATN(MXI),MASK(MXI),KINS(MXI),
     &             TBLI(2,MXI),TBLN(2,MXI),NXTI(2,MXI),
     &             NUNDX(MXI),NTS
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C
      COMMON/PAC6/ LABLIS(2,MXI),LABVAL(MXI),NLA 
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PAC8/ KLASID(50),KLASND(50),KLASMU(50),NKLAS,LKIND
C
      COMMON/PAC9/ NAMP(3,9),LATW(200,9),BITN(200,9),NPB(9),NPAT
C
      COMMON/PACA/ JCRA(MXI),JSLO(MXI),JSUB(MXI),JFRE(MXI),
     &             JIDN(MXI),NNAF
C
      COMMON/PACB/ CNAFI(200),DATAI(200),NNAFI,
     &             CNAFQ(200),DATAQ(200),NNAFQ,
     &             CNAFR(200),DATAR(200),NNAFR
C
      COMMON/PACC/ RIFLATI(MXR),RIFMASK(MXR),RIFMOTY(2,MXR),NRIF
C
      COMMON/PACD/ FERID(32,576),FERC(576),FERN(576),FERT(576),NFER,
     &             FERERID
C
      COMMON/PACE/ FASID(256,32),FASC(32), FASN(32), FAST(32), NFAS,
     &             FASERID
C
      COMMON/PACF/ CLRC(200),CLRN(200),CLRA(200),CLRF(200),NCLR
C
      COMMON/PACG/ CRATLST(50),NCRAT
C
      COMMON/PACH/ KILATI(MXK),KILMSK(MXK),KILTYP(MXK),NKIL
C
      COMMON/PACI/ POB(65536),NPOB
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/PACK/ BOOLIST(MXC),NBOO
C
      COMMON/PACL/ CDPAT(MXD),CDMSK(MXD),CDCNT(MXD),NCDN
C
      COMMON/PACM/ NAMPAC(20)
C
      COMMON/PACN/ CAMID(32,32,8),CAMC(256),CAMN(256),CAMT(256),NCAM,
     &             CAMERID
C
      COMMON/PACO/ KODLO(4),KODHI(4)
C
C     ------------------------------------------------------------------
      COMMON/PACP/ DELAZ(30),NDELAZ
      INTEGER*4    DELAZ,    NDELAZ
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
C     ------------------------------------------------------------------
      COMMON/PACV/ VADCMAP(10),VTDCMAP(10),VADCID(340),VTDCID(340)
      INTEGER*4    VADCMAP,    VTDCMAP,    VADCID,     VTDCID
C     ------------------------------------------------------------------
      COMMON/KLOC/ KLOCID(2)
      INTEGER*4    KLOCID
C     ------------------------------------------------------------------
C
      INTEGER*4 MOLIST(20)
C
      INTEGER*4 DIR(3,31)
C
      BYTE BITE(4)
C
      EQUIVALENCE (WORD,BITE)
C
      EQUIVALENCE (DIR,POB)
C
      CHARACTER*4  KIMO,GTYP,USED,KLIS
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     ******************************************************************
C     BUILD THE FINAL TABLES FOR FRONT-END (OBJECT TABLES) 
C     ******************************************************************
C
C     ******************************************************************
C     DIR(I,1)  - LOCATES PAC SOURCE FILENAME
C     DIR(I,2)  - LOCATES CAMAC CRATE TABLE
C     DIR(I,3)  - LOCATES CNAF-INIT LIST
C     DIR(I,4)  - LOCATES CAMAC   MODULE TABLE
C     DIR(I,5)  - LOCATES FASTBUS MODULE TABLE
C     DIR(I,6)  - LOCATES FERA    MODULE TABLE
C     DIR(I,7)  - LOCATES GATED LATCH   TABLE
C     DIR(I,8)  - LOCATES GATE  READ    TABLE
C     DIR(I,9)  - LOCATES RAW GATE SPEC TABLE
C     DIR(I,10) - LOCATES CAL GATE SPEC TABLE
C     DIR(I,11) - LOCATES COUNT DOWN    TABLE
C     DIR(I,12) - LOCATES CONDITIIONAL  KILL TABLE
C     DIR(I,13) - LOCATES UNCONDITIONAL READOUT TABLE
C     DIR(I,14) - LOCATES CONDITIOINAL  READOUT PROGRAM
C     DIR(I,15) - LOCATES CNAF-LIST FOR CONDITIONAL READOUT
C     DIR(I,16) - LOCATES ID-LIST   FOR CONDITIONAL READOUT
C     DIR(I,17) - LOCATES CAMAC   MODULE-TYPE READOUT LIST
C     DIR(I,18) - LOCATES FASTBUS MODULE-TYPE READOUT LIST
C     DIR(I,19) - LOCATES FERA    MODULE-TYPE READOUT LIST
C     DIR(I,20) - LOCATES CAMAC   ID-TABLE
C     DIR(I,21) - LOCATES FASTBUS ID-TABLE
C     DIR(I,22) - LOCATES FERA    ID-TABLE
C     DIR(I,23) - LOCATES WINDUP  CNAF LIST
C     DIR(I,24) - LOCATES RUN     CNAF LIST
C     DIR(I,25) - LOCATES XIA     MODULE TABLE
C     DIR(I,26) - LOCATES VME CONDITIONAL READOUT TABLE
C     DIR(I,27) - LOCATES CAEN ADC HARDWARE MAP
C     DIR(I,28) - LOCATES CAEN TDC HARDWARE MAP
C     DIR(I,29) - LOCATES CAEN ADC ID-TABLE
C     DIR(I,30) - LOCATES CAEN TDC ID-TABLE
C     DIR(I,31) - LOCATES 100HZ clock IDs (2 required)
C     *************************************************************
C
      DO 20 J=1,31
      DO 10 I=1,3
      DIR(I,J)=0
   10 CONTINUE
   20 CONTINUE
C
      IOF=100
C
C     01-***************************(01-03)**!PAC FILENAME*********
C
  100 JD=1                                   !DIRECTORY#
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=20                           !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 110 I=1,20
      POB(IOF+I)=NAMPAC(I)                   !SOURCE FILE NAME
  110 CONTINUE
      IOF=IOF+20
C
C     02-***************************(04-06)**!CRATE-LIST***********
C
  200 IF(NCRAT.LE.0) GO TO 300
C
      JD=2
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NCRAT                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 210 I=1,NCRAT
      POB(IOF+I)=CRATLST(I)
  210 CONTINUE
      IOF=IOF+NCRAT
C
C     03-***************************(07-09)**!CNAF-INIT LIST*******
C
  300 IF(NNAFI.LE.0) GO TO 400
C
      JD=3
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAFI                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 310 I=1,NNAFI
      POB(IOF+N+1)=CNAFI(I)
      POB(IOF+N+2)=DATAI(I)
      N=N+2
  310 CONTINUE
      IOF=IOF+2*NNAFI
C
C     04-***************************(10-12)**!CAMAC   MODULE TABLE
C
  400 IF(NCAM.LE.0) GO TO 500
C
      JD=4
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NCAM                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 410 I=1,NCAM
      BITE(1)=CAMT(I)                        !MODULE-TYPE
      BITE(2)=0                              !DUMMY
      BITE(3)=CAMN(I)                        !SLOT#
      BITE(4)=CAMC(I)                        !CRATE#
      POB(IOF+I)=WORD
  410 CONTINUE
      IOF=IOF+NCAM
C
C     05-***************************(13-15)**!FASTBUS MODULE TABLE
C
  500 IF(NFAS.LE.0) GO TO 600
C
      JD=5
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NFAS                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 510 I=1,NFAS
      BITE(1)=FAST(I)                        !MODULE-TYPE
      BITE(2)=0                              !DUMMY
      BITE(3)=FASN(I)                        !SLOT#
      BITE(4)=FASC(I)                        !CRATE#
      POB(IOF+I)=WORD
  510 CONTINUE
      IOF=IOF+NFAS
C
C     06-***************************(16-18)**!FERA MODULE TABLE****
C
  600 IF(NFER.LE.0) GO TO 700
C
      JD=6
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NFER                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 610 I=1,NFER
      BITE(1)=FERT(I)                        !MODULE-TYPE
      BITE(2)=0                              !DUMMY
      BITE(3)=FERN(I)                        !SLOT#
      BITE(4)=FERC(I)                        !CRATE#
      POB(IOF+I)=WORD
  610 CONTINUE
      IOF=IOF+NFER
C
C     07-***************************(19-21)**!GATED-LATCH TABLE****
C
  700 NLAT=0                                 !INIT LATCH COUNTER
      DO 710 I=1,NUMT                        !LOOP ON HWD TABLE
      IF(KIMO(I).NE.'$LAT') GO TO 710
      NLAT=NLAT+1
      BITE(1)=FRED(I)
      BITE(2)=SUBA(I)
      BITE(3)=SLOT(I)
      BITE(4)=CRAT(I)
      POB(IOF+NLAT)=WORD
  710 CONTINUE
C
      IF(NLAT.LE.0) GO TO 800
C
      JD=7
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NLAT                         !#ENTRIES
      DIR(3,JD)=DELAZ(7)                     !DELAY
      IOF=IOF+NLAT
C
C     08-***************************(22-24)**!GATE READ TABLE
C
  800 IF(NGRED.LE.0) GO TO 900
C
      JD=8                                   !DIRECTORY#
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NGRED                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY?????????
      N=0
      DO 820 J=1,NGRED
      DO 810 I=1,5
      N=N+1
      POB(IOF+N)=GCNAF(I,J)
  810 CONTINUE
  820 CONTINUE
      IOF=IOF+N
C
C     09-***************************(25-27)**!RAW GATE SPEC TABLE
C
  900 IF(NGAT.LE.0) GO TO 1000
C
      JD=9                                   !DIRECTORY#
      DIR(1,JD)=IOF
      DIR(3,JD)=0                            !DELAY
      N=0
      NRAW=0
      DO 910 I=1,NUMT
      IF(GTYP(I).NE.'RAW ') GO TO 910
      POB(IOF+N+1)=RPTR(I)
      POB(IOF+N+2)=GLO(I)
      POB(IOF+N+3)=GHI(I)
      POB(IOF+N+4)=PATN(I)+1                 !START WITH LAT-NDX 2
      POB(IOF+N+5)=GMSK(I)
      N=N+5
      NRAW=NRAW+1
  910 CONTINUE
      DIR(2,JD)=NRAW                         !#ENTRIES
      IOF=IOF+N

C
C     10-***************************(28-30)**!CAL GATE SPEC TABLE
C
 1000 IF(NBOO.LE.0) GO TO 1100
C
      JD=10                                  !DIRECTORY#
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NBOO                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 1010 I=1,NBOO
      POB(IOF+I)=BOOLIST(I)
 1010 CONTINUE
      IOF=IOF+NBOO
C
C     11-***************************(31-33)**!COUNT DOWN TABLE
C
 1100 IF(NCDN.LE.0) GO TO 1200
C
      JD=11
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NCDN                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 1110 I=1,NCDN
      POB(IOF+N+1)=CDPAT(I)+1                !START WITH LAT-NDX 2
      POB(IOF+N+2)=CDMSK(I)
      POB(IOF+N+3)=CDCNT(I)
      N=N+3
 1110 CONTINUE
      IOF=IOF+3*NCDN
C
C     12-***************************(34-36)**!CONDITIONAL KILL TABL
C
 1200 IF(NKIL.LE.0) GO TO 1300
C
      JD=12
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NKIL                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 1210 I=1,NKIL
      POB(IOF+N+1)=KILATI(I)+1               !START WITH LAT-NDX 2
      POB(IOF+N+2)=KILMSK(I)
      POB(IOF+N+3)=KILTYP(I)
      N=N+3
 1210 CONTINUE
      IOF=IOF+3*NKIL
C
C     13-***************************(37-39)**!UN-COND READOUT TABLE
C
 1300 N=0                                    !INIT WORD COUNTER
      DO 1320 I=1,NUMT
C
      IF(USED(I).EQ.'YES ') GO TO 1320
      IF(IDNM(I).LE.0)      GO TO 1320
C
      IT=MOTY(1,I)
      IF(IT.GE.KODLO(3).AND.IT.LE.KODHI(3)) GO TO 1320
C
      IF(KIMO(I).EQ.'$CAM') GO TO 1310
      IF(KIMO(I).EQ.'$LAT') GO TO 1310
      GO TO 1320
C
 1310 BITE(1)=FRED(I)
      BITE(2)=SUBA(I)
      BITE(3)=SLOT(I)
      BITE(4)=CRAT(I)
      N=N+1
      POB(IOF+N)=WORD
      N=N+1
      POB(IOF+N)=IDNM(I)
 1320 CONTINUE
C
      IF(N.LE.0) GO TO 1400
C
      JD=13
      DIR(1,JD)=IOF
      DIR(2,JD)=N/2                          !#ENTRIES
      DIR(3,JD)=DELAZ(13)                    !DELAY
      IOF=IOF+N
C
C     14-***************************(40-42)**!COND-CAMAC PROGRAM***
C
 1400 IF(NTS.LE.0) GO TO 1500
C
      JD=14
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NTS+1                        !#ENTRIES
      DO 1410 I=1,NUMT                       !LOOP TO FIND MAX DELAY
      IF(KIMO(I).NE.'$CAM') GO TO 1410
      IF(USED(I).NE.'YES ') GO TO 1410
 1410 CONTINUE
      DIR(3,JD)=DELAZ(14)                    !DELAY
      N=0                                    !INIT WORD COUNTER
      DO 1420 I=1,NTS                        !LOOP ON # ENTRIES
      POB(IOF+N+1)=LATN(I)+1                 !START AT LAT-INDEX 2
      POB(IOF+N+2)=MASK(I)
      POB(IOF+N+3)=TBLI(1,I)
      POB(IOF+N+4)=TBLN(1,I)
      POB(IOF+N+5)=NXTI(1,I)
      POB(IOF+N+6)=TBLI(2,I)
      POB(IOF+N+7)=TBLN(2,I)
      POB(IOF+N+8)=NXTI(2,I)
      N=N+8
 1420 CONTINUE
      DO 1430 I=1,8                          !NULL ENTRY FOR END
      POB(IOF+N+I)=0
 1430 CONTINUE
      IOF=IOF+8*(NTS+1)
C
C     15-***************************(43-45)**!COND-CAMAC CNAF-LIST*
C
 1500 IF(NNAF.LE.0) GO TO 1600
C
      JD=15
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAF                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 1510 I=1,NNAF
      BITE(1)=JFRE(I)
      BITE(2)=JSUB(I)
      BITE(3)=JSLO(I)
      BITE(4)=JCRA(I)
      POB(IOF+I)=WORD
 1510 CONTINUE
      IOF=IOF+NNAF
C
C     16-***************************(46-48)**!COND-CAMAC ID-LIST***
C
 1600 IF(NNAF.LE.0) GO TO 1700
C
      JD=16
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAF                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 1610 I=1,NNAF
      POB(IOF+I)=JIDN(I)
 1610 CONTINUE
      IOF=IOF+NNAF
C
C     17-***************************(49-51)**!CAMAC   MODULE READ TABLE
C
 1700 N=0
      DO 1710 I=1,NRIF
      IT=RIFMOTY(1,I)
      IF(IT.LT.KODLO(3).OR.IT.GT.KODHI(3)) GO TO 1710
      POB(IOF+N+1)=RIFLATI(I)+1              !START WITH LAT-NDX 2
      POB(IOF+N+2)=RIFMASK(I)
      POB(IOF+N+3)=RIFMOTY(1,I)
      N=N+3
 1710 CONTINUE
      CALL MODULY('$CAM',MOLIST,NUM)
      DO 1720 I=1,NUM
      POB(IOF+N+1)=1                         !LAT-WD (ALL BITS SET)
      POB(IOF+N+2)=-1                        !MASK   (ALL BITS SET)
      POB(IOF+N+3)=MOLIST(I)
      N=N+3
 1720 CONTINUE
      NCAMA=N/3
C
      IF(N.LE.0) GO TO 1800
C
      JD=17
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N/3                          !#ENTRIES
      DIR(3,JD)=DELAZ(17)                    !DELAY
      IOF=IOF+N
C
C     18-***************************(52-54)**!FASTBUS MODULE READ TABLE
C
 1800 N=0
      DO 1810 I=1,NRIF
      IT=RIFMOTY(1,I)
      IF(IT.LT.KODLO(2).OR.IT.GT.KODHI(2)) GO TO 1810
      POB(IOF+N+1)=RIFLATI(I)+1              !START WITH LAT-NDX 2
      POB(IOF+N+2)=RIFMASK(I)
      POB(IOF+N+3)=RIFMOTY(1,I)
      N=N+3
 1810 CONTINUE
      CALL MODULY('$FAS',MOLIST,NUM)
      DO 1820 I=1,NUM
      POB(IOF+N+1)=1                         !LAT-WD (ALL BITS SET)
      POB(IOF+N+2)=-1                        !MASK   (ALL BITS SET)
      POB(IOF+N+3)=MOLIST(I)
      N=N+3
 1820 CONTINUE
      NFAST=N/3
C
      IF(N.LE.0) GO TO 1900
C
      JD=18
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N/3                          !#ENTRIES
      DIR(3,JD)=DELAZ(18)                    !DELAY
      IOF=IOF+N
C
C     19-***************************(55-57)**!FERA    MODULE READ TABLE
C
 1900 N=0
      DO 1910 I=1,NRIF
      IT=RIFMOTY(1,I)
      IF(IT.LT.KODLO(1).OR.IT.GT.KODHI(1)) GO TO 1910
      POB(IOF+N+1)=RIFLATI(I)+1              !START WITH LAT-NDX 2
      POB(IOF+N+2)=RIFMASK(I)
      POB(IOF+N+3)=RIFMOTY(1,I)
      N=N+3
 1910 CONTINUE
      CALL MODULY('$FER',MOLIST,NUM)
      DO 1920 I=1,NUM
      POB(IOF+N+1)=1                         !LAT-WD (ALL BITS SET)
      POB(IOF+N+2)=-1                        !MASK   (ALL BITS SET)
      POB(IOF+N+3)=MOLIST(I)
      N=N+3
 1920 CONTINUE
      NFERA=N/3
C
      IF(N.LE.0) GO TO 2000
C
      JD=19
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N/3                          !#ENTRIES
      DIR(3,JD)=DELAZ(19)                    !DELAY
      IOF=IOF+N
C
C     20-***************************(58-60)**!CAMAC   ID TABLE
C
 2000 IF(NCAMA.LE.0) GO TO 2100
C
      JD=20
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=8192                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2030 K=1,8
      DO 2020 J=1,32
      DO 2010 I=1,32
      N=N+1
      POB(IOF+N)=CAMID(I,J,K)    
 2010 CONTINUE
 2020 CONTINUE
 2030 CONTINUE
      IOF=IOF+N
C
C     21-***************************(61-63)**!FASTBUS ID-TABLE*****
C
 2100 IF(NFAST.LE.0) GO TO 2200
C
      JD=21
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=8192                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2120 J=1,32
      DO 2110 I=1,256
      N=N+1
      POB(IOF+N)=FASID(I,J)    
 2110 CONTINUE
 2120 CONTINUE
      IOF=IOF+N
C
C     22-***************************(64-66)**!FERA ID-TABLE********
C
 2200 IF(NFERA.LE.0) GO TO 2300
C
      JD=22
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=18432                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2220 J=1,576
      DO 2210 I=1,32
      N=N+1
      POB(IOF+N)=FERID(I,J)
 2210 CONTINUE
 2220 CONTINUE
      IOF=IOF+N
C
C     23-***************************(67-69)**!WINDUP/CLR CNAF TABLE
C
 2300 IF((NNAFQ+NCLR).LE.0) GO TO 2400
C
      JD=23
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAFQ+NCLR                   !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2310 I=1,NNAFQ
      N=N+1
      POB(IOF+N)=CNAFQ(I)
      N=N+1
      POB(IOF+N)=DATAQ(I)
 2310 CONTINUE
      DO 2320 I=1,NCLR
      N=N+1
      BITE(1)=CLRF(I)
      BITE(2)=CLRA(I)
      BITE(3)=CLRN(I)
      BITE(4)=CLRC(I)
      POB(IOF+N)=WORD
      N=N+1
      POB(IOF+N)=0
 2320 CONTINUE
      IOF=IOF+N
C
C     24-***************************(70-72)**!RUN CNAF TABLE
C
 2400 IF(NNAFR.LE.0) GO TO 2500
C
      JD=24
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAFR                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2410 I=1,NNAFR
      N=N+1
      POB(IOF+N)=CNAFR(I)
      N=N+1
      POB(IOF+N)=DATAR(I)
 2410 CONTINUE
      IOF=IOF+N
C
C     25-***************************(73-75)**!XIA MODULE TABLE*****
C
 2500 IF(NXIA.LE.0) GO TO 2600
C
      JD=25
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NXIA                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 2510 I=1,NXIA
      BITE(1)=XIAGRP(I)                      !GROUP# (optinal)
      BITE(2)=XIAVSN(I)                      !VERTUAL STATION#
      BITE(3)=XIAN(I)                        !SLOT#
      BITE(4)=XIAC(I)                        !CRATE#
      POB(IOF+I)=WORD
 2510 CONTINUE
      IOF=IOF+NXIA
C
C     26-***************************(76-78)**!VME MODULE READ TABLE*****
C
 2600 N=0
      DO 2610 I=1,NRIF
      IT=RIFMOTY(1,I)
      IF(IT.LT.KODLO(4).OR.IT.GT.KODHI(4)) GO TO 2610
      POB(IOF+N+1)=RIFLATI(I)+1              !START WITH LAT-NDX 2
      POB(IOF+N+2)=RIFMASK(I)
      POB(IOF+N+3)=RIFMOTY(1,I)
      N=N+3
 2610 CONTINUE
      CALL MODULY('$VME',MOLIST,NUM)
      DO 2620 I=1,NUM
      POB(IOF+N+1)=1                         !LAT-WD (ALL BITS SET)
      POB(IOF+N+2)=-1                        !MASK   (ALL BITS SET)
      POB(IOF+N+3)=MOLIST(I)
      N=N+3
 2620 CONTINUE
      NVME=N/3
C
      IF(NVME.LE.0) GO TO 2700
C
      JD=26
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N/3                          !#ENTRIES
      DIR(3,JD)=DELAZ(26)                    !DELAY
      IOF=IOF+N
C
C     27-***************************(79-81)**!CAEN ADC HARDWARE MAP*****
C
 2700 N=0
      NADC=0
      DO 2710 I=1,10
      N=N+1
      POB(IOF+N)=VADCMAP(I)
      IF(VADCMAP(I).GT.0) NADC=NADC+1
 2710 CONTINUE
      IF(NADC.LE.0) GO TO 2800
C
      JD=27
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     28-***************************(82-84)**!CAEN TDC HARDWARE MAP*****
C
 2800 N=0
      NTDC=0
      DO 2810 I=1,10
      N=N+1
      POB(IOF+N)=VTDCMAP(I)
      IF(VTDCMAP(I).GT.0) NTDC=NTDC+1
 2810 CONTINUE
      IF(NTDC.LE.0) GO TO 2900
C
      JD=28
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     29-***************************(85-87)**!CAEN ADC     ID-TABLE*****
C
 2900 IF(NADC.LE.0) GO TO 3000
      N=0
      DO 2910 I=1,340
      N=N+1
      POB(IOF+N)=VADCID(I)
 2910 CONTINUE
C
      JD=29
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     30-***************************(88-90)**!CAEN TDC     ID-TABLE*****
C
 3000 IF(NTDC.LE.0) GO TO 3100
      N=0
      DO 3010 I=1,340
      N=N+1
      POB(IOF+N)=VTDCID(I)
 3010 CONTINUE
C
      JD=30
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     31-***************************(91-93)**!100HZ CLOCK  ID-TABLE*****
C
 3100 IF(KLOCID(1).EQ.0) GO TO 5000
      N=0
      DO 3110 I=1,2
      N=N+1
      POB(IOF+N)=KLOCID(I)
 3110 CONTINUE
C
      JD=31
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=2                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C
 5000 NPOB=IOF
      IF(KLIS.EQ.'X   ') WRITE(LU,REC=1)(POB(I),I=1,IOF)
      NBWRIT=4*NPOB
      WRITE(6,5005)NBWRIT
 5005 FORMAT(1H ,'#BYTES OBJECT CODE GENERATED =',I8)
      RETURN
      END
C$PROG POBLOD
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE POBLOD
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/PACI/ POB(65536),NPOB
C
      BYTE IBY(4),JBY(4)
C
      EQUIVALENCE (IBY,IWD),(JBY,JWD)
C
C     ************************************************************
C     DOWNLOAD OBJECT-CODE (TABLES) INTO FRONT-END PROCESSOR
C     ************************************************************
C
      DO 10 I=1,NPOB
      IWD=POB(I)
      JBY(4)=IBY(1)
      JBY(3)=IBY(2)
      JBY(2)=IBY(3)
      JBY(1)=IBY(4)
      POB(I)=JWD
   10 CONTINUE
C
      NBYTES=4*NPOB
C
      CALL LOAD_ACQ_PARAMS(POB,NBYTES)
C
      WRITE(6,20)NBYTES
   20 FORMAT(1H ,'CONSIDER YOURSELF LOADED WITH ',I6,'  BYTES')
      RETURN
      END
C$PROG RIFFER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE RIFFER(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXG=100)
      PARAMETER (MXL=50)
      PARAMETER (MXR=10)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PACC/ RIFLATI(MXR),RIFMASK(MXR),RIFMOTY(2,MXR),NRIF
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      INTEGER*4 NAMLAT(4,MXL),NDXL(MXL),NAML(4),NAMG(4),NLAT
C
      DATA NCALL,NLAT,NRIF,MXLAT,MXRIF/0,0,0,50,10/
C
      DATA SETW/'SETW'/
C
      CHARACTER*4  KIMO,USED,FOUND,CLWD(3,40)
C
      EQUIVALENCE (CLWD,LWD)
C
      SAVE
C
      IF(NCALL.GT.0) GO TO 100
C
      DO 50 N=1,NUMT
      IF(KIMO(N).NE.'$LAT') GO TO 50
      NLAT=NLAT+1
      IF(NLAT.GT.MXLAT)     GO TO 510
      NAMLAT(1,NLAT)=NAMO(1,N)
      NAMLAT(2,NLAT)=NAMO(2,N)
      NAMLAT(3,NLAT)=NAMO(3,N)
      NAMLAT(4,NLAT)=NAMO(4,N)
   50 CONTINUE
      NCALL=1
C
  100 CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER)
C
      CALL GREAD(IWD,LWD,ITYP,NF,6,80,NTER)
C
      IF(NTER.NE.0) GO TO 520
C
C     ************************************************************
C     DETERMINE IF READ CONDITION IS ON LATCH-WORD OR GATE
C     LWD(1,1)='TRUE' SAYS GATE-TYPE
C     ************************************************************
C
      IF(CLWD(1,1).EQ.'TRUE'.AND.CLWD(2,1).EQ.'    ') GO TO 200
C
      CALL MILV3(LWD(1,2),LDX,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 530
C
      DO 110 I=1,3
      NAML(I)=LWD(I,1)
  110 CONTINUE
      NAML(4)=LDX
C
      CALL NAMLOC(NAMLAT,NAML,NLAT,JXX)
      IF(JXX.LE.0) GO TO 540
C
      CALL HEXVAL(LWD(2,3),MSK,IERR)
      IF(IERR.NE.0) GO TO 550
C
      CALL MODCOD(LWD(1,4),MCODE,IERR)
      IF(IERR.NE.0) GO TO 1000
C
      FOUND='NO  '
      DO 150 I=1,NUMT
      IF(MCODE.NE.MOTY(1,I))  GO TO 150
      IF(USED(I).EQ.'YES ') GO TO 570
      USED(I)='YES '
      FOUND='YES '
  150 CONTINUE
      IF(FOUND.NE.'YES ') GO TO 580
C
      NRIF=NRIF+1
      IF(NRIF.GT.MXRIF)   GO TO 590
      RIFLATI(NRIF)=LDX
      RIFMASK(NRIF)=MSK
      RIFMOTY(1,NRIF)=MCODE
      GO TO 1000
C
C     ************************************************************
C     PROCESS GATE-TYPE CONDITIONAL MODULE-READ
C     ************************************************************
C
  200 CALL MILV3(LWD(1,3),IDX,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 530
      DO 210 I=1,3
      NAMG(I)=LWD(I,2)
  210 CONTINUE
      NAMG(4)=IDX
C
      CALL NAMLOC(GNAM,NAMG,NGAT,NDX)
      IF(NDX.LE.0) GO TO 560
C
      CALL MODCOD(LWD(1,4),MCODE,IERR)
      IF(IERR.NE.0) GO TO 1000
C
      FOUND='NO  '
      DO 250 I=1,NUMT
      IF(MCODE.NE.MOTY(1,I)) GO TO 250
      IF(USED(I).EQ.'YES ')  GO TO 570
      USED(I)='YES '
      FOUND='YES '
  250 CONTINUE
      IF(FOUND.NE.'YES ') GO TO 580
C
      NRIF=NRIF+1
      IF(NRIF.GT.MXRIF)   GO TO 590
      RIFLATI(NRIF)=PATN(NDX)
      RIFMASK(NRIF)=GMSK(NDX)
      RIFMOTY(1,NRIF)=MCODE
      GO TO 1000
C
C     ************************************************************
C     SEND ERROR MESSAGES
C     ************************************************************
C
  510 WRITE(CMSSG,515)NLAT
  515 FORMAT('LATCH-WORD TABLE OVERFLOW FROM RIFFER - NLAT=',I6)
      GO TO 800
  520 WRITE(CMSSG,525)
  525 FORMAT('TRUNCATION ERROR FROM - GREAD')
      GO TO 800
  530 WRITE(CMSSG,535)
  535 FORMAT('SYNTAX ERROR FROM MILV3')
      GO TO 800
  540 WRITE(CMSSG,545)NAML
  545 FORMAT('REFERRENCED LATCH-WORD & INDEX - ',3A4,I6,' NOT FOUND')
      GO TO 800
  550 WRITE(CMSSG,555)LWD(2,3),LWD(3,3)
  555 FORMAT('ERROR DECODING HEX MASK - ',2A4)
      GO TO 800
  560 WRITE(CMSSG,565)NAMG
  565 FORMAT('REFERRENCED GATENAME & INDEX - ',3A4,I6,' NOT FOUND')
      GO TO 800
  570 WRITE(CMSSG,575)LWD(1,4),LWD(2,4),LWD(3,4)
  575 FORMAT('MULTIPLE CONDITIONAL READOUT OF MODULE - ',3A4)
      GO TO 800
  580 WRITE(CMSSG,585)LWD(1,4),LWD(2,4),LWD(3,4)
  585 FORMAT('REQUESTED MODULE TYPE - ',3A4,' NOT FOUND')
      GO TO 800
  590 WRITE(CMSSG,595)NRIF
  595 FORMAT('RIF TABLE OVERFLOW AT - ',I6)
C
  800 CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
      RETURN
      END
C$PROG STRIPATH
C
      SUBROUTINE STRIPATH(NAME,N)
C
      BYTE    NAME(*)
C
      BYTE    X20,X2F
C
      DATA    X20,X2F/'20'X,'2F'X/
C
      SAVE
C
      J=N+1
      DO 10 I=1,N
      J=J-1
      IF(NAME(J).EQ.X2F) GO TO 20
   10 CONTINUE
      RETURN
C
   20 JA=J+1
      II=0
      DO 30 J=JA,N
      II=II+1
      NAME(II)=NAME(J)
   30 CONTINUE
C
      IA=II+1
      DO 40 I=IA,N
      II=II+1
      NAME(II)=X20
   40 CONTINUE
      RETURN
      END
C$PROG SYMSAV
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE SYMSAV(KSYM,IV,IERR)
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
C     ************************************************************
C     ROUTINE TO SAVE AND ASSIGN ENTRIES IN SYMBOL TABLE
C     ************************************************************
C   
      IERR=0
C   
      DO 20 I=1,NSYM
      IF(KSYM.NE.ISYN(I)) GO TO 20
      GO TO 30
   20 CONTINUE
C   
      IF(NSYM.GT.99) GO TO 120
C   
      NSYM=NSYM+1
      ISYN(NSYM)=KSYM
      ISYV(NSYM)=IV
      RETURN
C   
   30 ISYV(I)=IV
      RETURN
C   
  120 WRITE(CMSSG,130)KSYM
      CALL ERRLOG(LOGUT,LOGUP)
  130 FORMAT('SYMBOL TABLE OVERFLOW (MAX=100) SAVING - ',A4)
      IERR=1
      RETURN
      END
C$PROG TABLOG
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ******************************************************************
C
      SUBROUTINE TABLOG(LU)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXI=2000)
      PARAMETER (MXC=500)
      PARAMETER (MXG=100)
      PARAMETER (MXD=100)
      PARAMETER (MXK=50)
      PARAMETER (MXR=10)
C
      COMMON/PAC1/ KINMOD,MODKOD,NAMOD(3),MODATA(2,12)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PAC4/ LATN(MXI),MASK(MXI),KINS(MXI),
     &             TBLI(2,MXI),TBLN(2,MXI),NXTI(2,MXI),
     &             NUNDX(MXI),NTS
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C
      COMMON/PAC6/ LABLIS(2,MXI),LABVAL(MXI),NLA 
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PAC8/ KLASID(50),KLASND(50),KLASMU(50),NKLAS,LKIND
C
      COMMON/PAC9/ NAMP(3,9),LATW(200,9),BITN(200,9),NPB(9),NPAT
C
      COMMON/PACA/ JCRA(MXI),JSLO(MXI),JSUB(MXI),JFRE(MXI),
     &             JIDN(MXI),NNAF
C
      COMMON/PACB/ CNAFI(200),DATAI(200),NNAFI,
     &             CNAFQ(200),DATAQ(200),NNAFQ,
     &             CNAFR(200),DATAR(200),NNAFR
C
      COMMON/PACC/ RIFLATI(MXR),RIFMASK(MXR),RIFMOTY(2,MXR),NRIF
C
      COMMON/PACD/ FERID(32,576),FERC(576),FERN(576),FERT(576),NFER,
     &             FERERID
C
      COMMON/PACE/ FASID(256,32),FASC(32), FASN(32), FAST(32), NFAS,
     &             FASERID
C
      COMMON/PACF/ CLRC(200),CLRN(200),CLRA(200),CLRF(200),NCLR
C
      COMMON/PACG/ CRATLST(50),NCRAT
C
      COMMON/PACH/ KILATI(MXK),KILMSK(MXK),KILTYP(MXK),NKIL
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/PACK/ BOOLIST(MXC),NBOO
C
      COMMON/PACL/ CDPAT(MXD),CDMSK(MXD),CDCNT(MXD),NCDN
C
      COMMON/PACM/ NAMPAC(20)
C
      COMMON/PACN/ CAMID(32,32,8),CAMC(256),CAMN(256),CAMT(256),NCAM,
     &             CAMERID
C     ------------------------------------------------------------------
      COMMON/PACP/ DELAZ(30),NDELAZ
      INTEGER*4    DELAZ,    NDELAZ
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
C     ------------------------------------------------------------------
      COMMON/PACV/ VADCMAP(10),VTDCMAP(10),VADCID(340),VTDCID(340)
      INTEGER*4    VADCMAP,    VTDCMAP,    VADCID,     VTDCID
C     ------------------------------------------------------------------
      INTEGER*4    ADCID(34,10),TDCID(34,10)
C
      EQUIVALENCE (ADCID,VADCID),(TDCID,VTDCID)
C     ------------------------------------------------------------------
C
      INTEGER*4 IT(1000),JT(1000)
C
      SAVE
C     
C     ******************************************************************
C     /PAC1/ - DEFINITIONS - FROM ONE HARDWARE TABLE ENTRY
C     ******************************************************************
C     KINMOD      = MODULE TYPE ($LAT, $CAM, $FER, $FAS...)
C     NAMOD       = MODULE NAME (FOR EXAMPLE - ADC1) UP TO 12 BYTES
C     MODATA(1,1) = MODULE-NAME FIRST INDEX
C     MODATA(2,1) = MODULE-NAME INDEX INCREMENT
C     MODATA(1,2) = CRATE#
C     MODATA(1,3) = SLOT#
C     MODATA(1,4) = FIRST SUB-ADDRESS TO READ
C     MODATA(2,4) = LAST  SUB-ADDRESS TO READ
C     MODATA(1,5) = FUNCTION CODE FOR READ
C     MODATA(1,6) = CLASS#
C     MODATA(1,7) = DETECTOR#, FIRST VALUE
C     MODATA(2,7) = DETECTOR#, INCREMENT
C     MODATA(1,8) = ENTRY#
C     MODATA(1,9) = FUNCTION CODE FOR CLEAR
C     MODATA(1,10)= SUB-ADDRRESS FOR CLEAR
C     MODATA(1,11)= DELAY TIME BEFORE READ (no longer used ******)
C     MODATA(1,12)= ID-NUMBER, FIRST
C     MODATA(2,12)= ID-NUMBER, INCREMENT
C
C     ******************************************************************
C     /PAC2/ - DEFINITIONS - LIBRARY OF HARDWARE DEFINITIONS
C     ******************************************************************
C     NAMO(I,J),I=1,3 = JTH MODULE-NAME (12 BYTES MAX)
C     NAMO(4,J)       = JTH MODULE-NAME INDEX
C     KIMO(J)         = JTH MODULE-TYPE ($LAT, $CAM, ...)
C     CRAT(J)         = JTH CRATE#
C     SLOT(J)         = JTH SLOT#
C     SUBA(J)         = JTH SUB-ADDRESS
C     FRED(J)         = JTH READ  FUNCTION-CODE
C     FCLR(J)         = JTH CLEAR FUNCTION-CODE
C     ACLR(J)         = JTH SUB-ADDRESS FOR CLEAR
C     DLAT(J)         = JTH DELAY-TIME BEFORE READOUT
C     CLAS(J)         = JTH CLASS# ******************NOT USED NOW
C     DETN(J)         = JTH DETECTOR# ***************NOT USED NOW
C     ENTR(J)         = JTH ENTRY# ******************NOT USED NOW
C     IDNM(J)         = JTH PARAMETER-ID
C     MOTY(1,J)       = MODULE TYPE CODE (SEE ROUTINE MODCOD)
C     MOTY(2,J)       = RANGE, GAIN, ETC FOR ASSOCIATED MODULE TYPE
C     USED(J)         = YES/NO SAYS USED/NOT-USED IN CONDITIONAL READOUT
C     NUMT            = NUMBER OF ENTRIES IN THESE TABLES
C     ******************************************************************
C     /PAC4/ - DEFINITIONS - CONDITIONAL READOUT DRIVER TABLES
C     ******************************************************************
C     LATN(J)    = LATCH-WORD INDEX FOR JTH TEST
C     MASK(J)    = MASK FOR USE IN      JTH TEST
C     KINS(J)    = INSTRUCTION TYPE (DIAGNOSTICS ONLY)
C     TBLI(I,J)  = POINTS TO NAF-LIST (/PACA/)- I=1,2 for TRUE/FALSE 
C     TBLN(I,J)  = # NAFs TO TAKE FROM TABLE  - I=1,2 for TRUE/FALSE
C     NXTI(I,J)  = NEXT TEST-TABLE INDEX      - I=1,2 for TRUE/FALSE
C     NTS        = NUMBER OF TEST-TABLE ENTRIES TO USE
C
C     ******************************************************************
C     /PAC5/ - DEFINITIONS - INTERMEDIATE CONDITIONAL READOUT TABLES
C     ******************************************************************
C     LABL(J)    = JTH STATEMENT LABEL 
C     INST(J)    = JTH INSTRUCTION - IFU,IFS,IFA,IFN,CONT,GOTO
C     NAMD(I,J)  = JTH DEVICE NAME (I=1,3) - ADC1, TDC2, ETC
C     INDX(J)    = JTH DEVICE NAME INDEX
C     MSKI(J)    = JTH MASK FOR IF-TESTS
C     IDES(J)    = JTH DESTINATION (LABEL)
C     NREA(J)    = JTH READOUT LIST POINTER  - ASSOCIATED WITH "READ"
C     ILOR(J)    = JTH READOUT LIST LO-LIMIT - ASSOCIATED WITH "CONDITION"
C     IHIR(J)    = JTH READOUT LIST HI-LIMIT - ASSOCIATED WITH "CONDITION"
C     NCI        = NUMBER OF ENTRIES IN THIS INTERMEDIATE READOUT TABLE
C
C     ******************************************************************
C     /PAC6/ - DEFINITIONS - STATEMENT LABEL TABLES
C     ******************************************************************
C     LABLIS(1,J) = 1ST 4 BYTES OF JTH STATEMENT LABEL (ASCII)
C     LABLIS(2,J) = 2ND 4 BYTES OF JTH STATEMENT LABEL (ASCII)
C     LABVAL(J)   = VALUE OF       JTH STATEMENT LABEL (INTEGER)
C     NLA         = # OF ENTRIES IN THESE TABLES
C
C     ******************************************************************
C     /PAC7/ - DEFINITIONS - LISTING DIRECTIVES
C     ******************************************************************
C     ISORL  =   SOURCE-LINE COUNTER
C     IEXPL  = EXPANDED-LINE COUNTER
C     LISTYP = 'NONE' SAYS LIST NONE
C     LISTYP = 'SOR ' SAYS LIST SOURCE-LINES ONLY
C     LISTYP = 'ALL ' SAYS LIST SOURCE & GENERATED LINES
C     NERR   = NUMBER OF COMPILATION ERRORS
C
C     ******************************************************************
C     /PAC8/ - DEFINITIONS - CLASS TABLES
C     ******************************************************************
C     KLASID(J) = JTH CLASS-ID
C     KLASND(J) = #      OF DETECTORS FOR JTH CLASS
C     KLASMU(J) = # ENTRIES/DETECTOR  FOR JTH CLASS
C     NKLAS     = # OF ENTRIES IN THESE TABLES
C
C     ******************************************************************
C     /PAC9/ - DEFINITIONS - MULTI-WORD PATTERN-WORD TABLES
C     ******************************************************************
C     NAMP(I,K),I=1,3 = KTH PATTERN-WORD NAME
C     LATW(J,K)       = JTH LATCH-WD ASSOCIATED WITH KTH PATTERN-WD
C     BITN(J,K)       = JTH LATCH-WD-BIT        FOR  KTH PATTERN-WD
C     NBP(K)          = # OF BITS    ASSOCIATED WITH KTH PATTERN-WD
C     NPAT            = # OF PATTERN-WDS DEFINED
C
C     ******************************************************************
C     /PACA/ - DEFINITIONS - C,N,A,F,ID TABLE FOR CONDITIONAL CAMAC
C                                                 READOUT
C     ******************************************************************
C     JCRA(I) - CRATE#    FOR ITH ENTRY
C     JSLO(I) - SLOT#     FOR ITH ENTRY
C     JSUB(I) - SUB-ADDR  FOR ITH ENTRY
C     JFRE(I) - FUNC-CODE FOR ITH ENTRY (FOR READ)
C     JIDN(I) - ID-NUMBER FOR ITH ENTRY
C     NNAF    = NUMBER OF TABLE ENTRIES
C
C     ******************************************************************
C     /PACB/ - DEFINITIONS - INITIALIZING CNAF'S
C     ******************************************************************
C     CNAFI(I) - ITH PACKED INITIALIZING CNAF
C     DATAI(I) - ITH ASSOCIATED DATA WORD IF ANY
C     NNAFI    = NUMBER OF INITIALIZING CNAF'S
C     CNAFQ(I) - ITH PACKED WRAPUP CNAF (DONE AT END OF EACH EVENT)
C     DATAQ(I) - ITH ASSOCIATED DATA WORD IF ANY
C     NNAFQ    = NUMBER OF WRAPUP CNAF'S
C     CNAFR(I) - ITH PACKED RUN CNAF (DONE AT RUN COMMAND)
C     DATAR(I) - ITH ASSOCIATED DATA WORD IF ANY
C     NNAFR    = NUMBER OF RUN CNAF'S
C 
C     ******************************************************************
C     /PACC/ - DEFINITIONS - FERRA & FASTBUS CONDITIONAL READOUT
C     ******************************************************************
C     RIFLATI(I)   - LATCH-WORD  FOR ITH CONDITIONAL READOUT
C     RIFMASK(I)   - MASK        FOR ITH CONDITIONAL READOUT
C     RIFMOTY(1,I) - MODULE TYPE FOR ITH CONDITIONAL READOUT
C     RIFMOTY(2,I) - RANGE, ETC  FOR ASSOCIATED MODULE
C     NRIF         - NUMBER OF           CONDITIONAL READOUTS
C
C     ******************************************************************
C     /PACD/ - DEFINITIONS - FERRA   ID AND MODULE C,N,TYPE TABLES
C     ******************************************************************
C     FERC(I)    - ITH CRATE NUMBER
C     FERN(I)    - ITH SLOT  NUMBER
C     FERT(I)    - ITH MODULE TYPE CODE
C     FERID(I,J) - ID TABLE (ITH SUB-ADDRESS, JTH SLOT#
C     FERERID    - DEFAULT "ERROR-ID" TO LOAD IN ID-TABLE
C
C     ******************************************************************
C     /PACE/ - DEFINITIONS - FASTBUS ID AND MODULE C,N,TYPE TABLES
C     ******************************************************************
C     FASC(I)    - ITH CRATE NUMBER
C     FASN(I)    - ITH SLOT  NUMBER
C     FAST(I)    - ITH MODULE TYPE CODE
C     FASID(I,J) - ID TABLE (ITH SUB-ADDRESS, JTH SLOT#)
C     FASERID    - DEFAULT "ERROR-ID" TO LOAD IN ID-TABLE
C
C     ******************************************************************
C     /PACF/ - DEFINITIONS - CAMAC C,N,A,F TABLES FOR CLEAR
C     ******************************************************************
C     CLRC(I) - CRATE NUMBER FOR ITH CLEAR
C     CLRN(I) - SLOT  NUMBER FOR ITH CLEAR
C     CLRA(I) - SUB-ADDRESS  FOR ITH CLEAR
C     CLRF(I) - FUNCTION     FOR ITH CLEAR
C     NCLR    - NUMBER OF TANLE ENTRIES
C
C     ******************************************************************
C     /PACG/ - DEFINITIONS - CAMAC CRATE LIST
C     ******************************************************************
C     CRATLST(I) - ITH CAMAC CRATE NUMBER
C     NCRAT      = NUMBER OF ENTRIES
C
C     ******************************************************************
C     /PACH/ - DEFINITIONS - CONDITIONAL KILL-TABLE FROM $KIL
C     ******************************************************************
C     KILATI(I) - LATCH-WORD INDEX FOR ITH KILL TEST
C     KILMSK(I) - MASK-WORD        FOR ITH KILL TEST
C     KILTYP(I) - KILL-TYPE        FOR ITH KILL TEST 
C                                  1/0 SAYS KILL IF TRUE/FALSE
C     NKIL      = NUMBER OF ENTRIES
C
C     ******************************************************************
C     /PACJ/ - DEFINITIONS FOR FOR ITH GATE
C     ******************************************************************
C     GNAM(M,I),M=1,3 = GATE NAME (ASCII) (RAW OR CALCULATED)
C     GNAM(4,I)       = GATE NAME INDEX
C     GTYP(I)         = GATE TYPE = 'RAW' OR 'CAL'
C     PATN(I)         = ASSOCIATED PATTERN WORD# 
C     GMSK(I)         = ASSOCIATED MASK
C
C     GLO(J)          = GATE LO-LIMIT
C     GHI(J)          = GATE HI-LIMIT
C     LPTR(I)         = POINTER TO AUX LIST, COMMON/PACK/ FOR CAL 
C     RPTR(I)         = POINTER TO READ   LIST (INDEX IN GCNAF)
C
C     MPTR(I)         = POINTER TO MODULE LIST (INDEX IN /PACJ/)
C     GCNAF(M,I),M=1,5= RAW-GATE READ-LIST (C,N,A,F,MOTY)        
C     NENT(I)         = # OF ENTRIES FOR /PACK/ DATA
C
C     NGAT            = TOTAL# (RAW + CAL) GATE ENTRIES
C     NGRED           = # GATE-PARMS TO READ (# ENTRIES IN GCNAF)
C
C     PATNO           = CURRENT PATTERN WORD# 
C     MSKNO           = CURRENT MASK (SPECIFIES BIT TO SET)
C
C     ******************************************************************
C     /PACK/ - DEFINITIONS FOR CALCULATED GATES 
C     ******************************************************************
C     BOOLIST - CONTAINS A BOOLEAN LIST FOR CALCULATED GATES
C     THE FORM IS:
C
C     PATNDX, MASK, NOTCODE, GATNDX, OPCODE, NOTCODE, GATNDX......
C     ENDCODE, PATNDX, MASK, ....
C
C     NBOO            = NO. OF WORDS STORED IN BOOLIST 
C
C     ******************************************************************
C     /PACL/ - DEFINITIONS FOR COUNTDOWN LIST (KTH ENTRY)
C     ******************************************************************
C     CDPAT(K) = ASSOTIATED PATTERN WORD#
C     CDMSK(K) = ASSOCIATED MASK
C     CDCNT(K) = COUNT VALUE
C     NCDN     = NO. OF ENTRIES
C
C     ******************************************************************
C     /PACM/ - DEFINITION OF PAC SOURCE FILE-NAME
C     ******************************************************************
C     NAMPAC(I),I=1,20 - CONTAINS FULL PATH-NAME
C
C     ******************************************************************
C     /PACN/ - DEFINITIONS FOR CAMAC ID & MODULE C,N,TYPE TABLES
C     ******************************************************************
C     CAMC(I)      - ITH CRATE NUMBER
C     CAMN(I)      - ITH SLOT  NUMBER
C     CAMT(I)      - ITH MODULE TYPE CODE
C     CAMID(I,J,K) - ID TABLE FOR (ITH SUBA, JTH SLOT, KTH CRATE)
C     CAMERID      - DEFAULT ERROR ID TO LOAD IN TABLE
C     NCAM         - NUMBER OF ENTRIES
C
C     ******************************************************************
C     /PACP/ - DEFINITIONS FOR DELAY SPECIFICATIONS
C     ******************************************************************
C     DELAZ(I)     - DELAY (MICROSECONDS) FOR MODULE TYPE-I
C     NDELAZ       - NUMBER OF DELAYS SPECIFIED
C
C     ******************************************************************
C     /PACX/ - DEFINITIONS FOR XIA RELATED PARAMETERS
C     ******************************************************************
C     XIAC(I)      - ITH XIA CRATE NUMBER
C     XIAN(I)      - ITH XIA SLOT  NUMBER
C     XIAVSN(I)    - ITH XIA VIRTUAL STATION NUMBER
C     XIAGRP(I)    - ITH XIA GROUP NUMBER
C     NXIA         - NUMBER OF XIA ENTRIES
C     MXXIA        - MAX NUMBER OF XIA ENTRIES ALLOWED
C     ******************************************************************
C     /PACV/ - DEFINITIONS FOR VME (CAEN) RELATED PARAMETERS
C     ******************************************************************
C     VADCMAP(J)   = 1/0 for ADC-J EXISTS/NON-EXISTS
C     VTDCMAP(J)   = 1/0 for TDC-J EXISTS/NON-EXISTS
C     ADCID(I,J)   - CONTAINS ID ASSOCIATED WITH CHANNEL-I & ADC-J
C     TDCID(I,J)   - CONTAINS ID ASSOCIATED WITH CHANNEL-I & TDC-J
C     ******************************************************************
C
      WRITE(LU,90)
   90 FORMAT(1H )
      WRITE(LU,100)
  100 FORMAT(1H ,'COMMON/PAC1/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,90)
      WRITE(LU,200)
  200 FORMAT(1H ,'COMMON/PAC2/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,90)
      WRITE(LU,205)
  205 FORMAT(1H ,'  N  NAMO         NAMI KIMO CRAT SLOT SUBA FRED',
     &                  ' FCLR ACLR DLAT    IDNM MOTY GAIN USED'/)
C
      DO 220 N=1,NUMT
      WRITE(LU,210)N,(NAMO(I,N),I=1,4),KIMO(N),CRAT(N),
     &              SLOT(N),SUBA(N),FRED(N),FCLR(N),ACLR(N),
     &              DLAT(N),IDNM(N),MOTY(1,N),MOTY(2,N),USED(N)
C
  210 FORMAT(1H ,I3,2X,3A4,I5,1X,A4,7I5,Z8,2I5,2X,A3)
  220 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,400)
  400 FORMAT(1H ,'COMMON/PAC4/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,405)
  405 FORMAT(1H ,'    N  LATN      MASK    KINS  TBLI-T  TBLI-F',
     &                        '  TBLN-T  TBLN-F  NXTI-T  NXTI-F',
     &                        '   NUNDX'/)
C
      DO 420 N=1,NTS
      WRITE(LU,410)N,LATN(N),MASK(N),KINS(N),TBLI(1,N),TBLI(2,N),
     &               TBLN(1,N),TBLN(2,N),NXTI(1,N),NXTI(2,N),
     &               NUNDX(N)
C
  410 FORMAT(1H ,I5,I6,Z10,4X,A4,7I8)
  420 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,500)
  500 FORMAT(1H ,'COMMON/PAC5/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,505)
  505 FORMAT(1H ,'     N  LABL  INST  NAMD          INDX      MSKI',
     &                 '  IDES  NREA  ILOR  IHIR'/)
      DO 520 N=1,NCI
      WRITE(LU,510)N,LABL(N),INST(N),(NAMD(I,N),I=1,3),INDX(N),
     &               MSKI(N),IDES(N),NREA(N),ILOR(N),IHIR(N)
C
  510 FORMAT(1H ,I6,2(2X,A4),2X,3A4,2X,I4,Z10,2X,A4,3I6)
  520 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,600)
  600 FORMAT(1H ,'COMMON/PAC6/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,605)
  605 FORMAT(1H ,'     N    LABLIS    LABVAL'/)
C
      DO 620 N=1,NLA
      WRITE(LU,610)N,LABLIS(1,N),LABLIS(2,N),LABVAL(N)
  610 FORMAT(1H ,I6,4X,2A4,I8)
  620 CONTINUE
C
      WRITE(LU,700)
  700 FORMAT(1H ,'COMMON/PAC7/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,705)
  705 FORMAT(1H ,'   ISORL   IEXPL  LISTYP    NERR'/)
      WRITE(LU,710)ISORL,IEXPL,LISTYP,NERR
  710 FORMAT(1H ,2I8,4X,A4,I8)
C
      WRITE(LU,90)
      WRITE(LU,800)
  800 FORMAT(1H ,'COMMON/PAC8/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,805)
  805 FORMAT(1H ,'     N  KLASID  KLASND  KLASMU'/)
      DO 820 N=1,NKLAS
      WRITE(LU,810)N,KLASID(N),KLASND(N),KLASMU(N)
  810 FORMAT(1H ,I6,3I8)
  820 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,900)
  900 FORMAT(1H ,'COMMON/PAC9/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,905)
  905 FORMAT(1H ,'NAMP        (NDX)    LATW    BITN     NBP',
     &                              '       N'/)
      DO 940 N=1,NPAT
      NDX=1
      WRITE(LU,910)(NAMP(I,N),I=1,3),NDX,LATW(1,N),BITN(1,N),NPB(N),N
  910 FORMAT(1H ,3A4,'(',I3,')',4I8)
      JDO=NPB(N)
      DO 930 J=2,JDO
      NDX=NDX+1
      WRITE(LU,910)(NAMP(I,N),I=1,3),NDX,LATW(J,N),BITN(J,N)
  930 CONTINUE
  940 CONTINUE 
C
      WRITE(LU,90)
      WRITE(LU,1000)
 1000 FORMAT(1H ,'COMMON/PACA/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1010)
 1010 FORMAT(1H ,'       I    JCRA    JSLO    JSUB    JFRE    JIDN'/)
      DO 1020 I=1,NNAF
      WRITE(LU,1015)I,JCRA(I),JSLO(I),JSUB(I),JFRE(I),JIDN(I)
 1015 FORMAT(1H ,5I8,Z8)
 1020 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1030)
 1030 FORMAT(1H ,'COMMON/PACB/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
      WRITE(LU,1035)
 1035 FORMAT(1H ,'     CNAFI     DATAI     CNAFQ     DATAQ',
     &           '     CNAFR     DATAR'/)
      NDO=NNAFI
      IF(NNAFQ.GT.NDO) NDO=NNAFQ
      IF(NNAFR.GT.NDO) NDO=NNAFR
      DO 1045 I=1,NDO
      WRITE(LU,1040)CNAFI(I),DATAI(I),CNAFQ(I),DATAQ(I),
     &              CNAFR(I),DATAR(I)
 1040 FORMAT(1H ,6Z10)
 1045 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1050)
 1050 FORMAT(1H ,'COMMON/PACC/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1055)
 1055 FORMAT(1H ,'   RIFLATI   RIFMASK   RIFMOTY     RANGE'/)
      DO 1070 I=1,NRIF
      WRITE(LU,1060)RIFLATI(I),RIFMASK(I),(RIFMOTY(J,I),J=1,2)
 1060 FORMAT(1H ,I10,Z10,2I10)
 1070 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1080)
 1080 FORMAT(1H ,'COMMON/PACD/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1085)
 1085 FORMAT(1H ,'      FERC      FERN      FERT'/)
      DO 1100 I=1,NFER
      WRITE(LU,1090)FERC(I),FERN(I),FERT(I)
 1090 FORMAT(1H ,3I10)
 1100 CONTINUE
      WRITE(LU,90)
      WRITE(LU,1105)
 1105 FORMAT(1H ,'FERID-TABLE (A VS N) (32,576)'/)
      WRITE(LU,1110)FERID
 1110 FORMAT(1H ,16Z6)

C
      WRITE(LU,90)
      WRITE(LU,1120)
 1120 FORMAT(1H ,'COMMON/PACE/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1125)
 1125 FORMAT(1H ,'      FASC      FASN      FAST'/)
      DO 1140 I=1,NFAS
      WRITE(LU,1130)FASC(I),FASN(I),FAST(I)
 1130 FORMAT(1H ,3I10)
 1140 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1150)
 1150 FORMAT(1H ,'FASID TABLE (A VS N) (256,32)'/)
C
      DO 1180 N=1,32
      WRITE(LU,1160)(FASID(I,N),I=1,256)
 1160 FORMAT(1H ,16Z6)
      WRITE(LU,90)
 1180 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1190)
 1190 FORMAT(1H ,'COMMON/PACF/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1200)
 1200 FORMAT(1H ,'      CLRC      CLRN      CLRA      CLRF'/)
      DO 1210 I=1,NCLR
      WRITE(LU,1205)CLRC(I),CLRN(I),CLRA(I),CLRF(I)
 1205 FORMAT(1H ,4I10)
 1210 CONTINUE
C
C
      WRITE(LU,90)
      WRITE(LU,1220)
 1220 FORMAT(1H ,'COMMON/PACG/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1225)
 1225 FORMAT(1H ,'   CRATLST'/)
      DO 1240 I=1,NCRAT
      WRITE(LU,1230)CRATLST(I)
 1230 FORMAT(1H ,I10)
 1240 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1245)
 1245 FORMAT(1H ,'COMMON/PACH/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1250)
 1250 FORMAT(1H ,'    KILATI    KILMSK    KILTYP'/)
      DO 1260 I=1,NKIL
      WRITE(LU,1255)KILATI(I),KILMSK(I),KILTYP(I)
 1255 FORMAT(1H ,I10,Z10,I10)
 1260 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1265)
 1265 FORMAT(1H ,'COMMON/PACJ/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1270)
 1270 FORMAT(1H ,'GNAM          GNDX  GTYP  PATN     GMSK   GLO',
     &           '   GHI  LPTR  RPTR  MPTR  NENT'/)
C
      DO 1280 J=1,NGAT
      WRITE(LU,1275)(GNAM(I,J),I=1,4),GTYP(J),PATN(J),GMSK(J),GLO(J),
     &               GHI(J),LPTR(J),RPTR(J),MPTR(J),NENT(J)
 1275 FORMAT(1H ,3A4,I6,3X,A3,I6,Z9,6I6)
 1280 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1285)
 1285 FORMAT(1H ,'  CRAT  SLOT  SUBA  FRED  MTYP'/)
      DO 1295 J=1,NGRED
      WRITE(LU,1290)(GCNAF(I,J),I=1,5)
 1290 FORMAT(1H ,5I6)
 1295 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1300)NGAT,NGRED
 1300 FORMAT(1H ,'NGAT,NGRED =',2I6)
C
      WRITE(LU,90)
      WRITE(LU,1305)
 1305 FORMAT(1H ,'COMMON/PACK/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      J=0
 1310 J=J+1
      IF(J.GT.NBOO) GO TO 1400
      WRITE(LU,1315)BOOLIST(J)
 1315 FORMAT(1H ,'PATNDX =',I8)
      J=J+1
      WRITE(LU,1320)BOOLIST(J)
 1320 FORMAT(1H ,'MASK   =',Z8)
      J=J+1
      WRITE(LU,1325)BOOLIST(J)
 1325 FORMAT(1H ,'NOTCOD =',I8)
      J=J+1
      WRITE(LU,1330)BOOLIST(J)
 1330 FORMAT(1H ,'GATNDX =',I8)
C
 1350 J=J+1
      IF(BOOLIST(J).EQ.-1) THEN
      WRITE(LU,1355)BOOLIST(J)
 1355 FORMAT(1H ,'ENDCODE=',I8)
      GO TO 1310
      ENDIF
C
      WRITE(LU,1360)BOOLIST(J)
 1360 FORMAT(1H ,'OPCODE =',I8)
      J=J+1
      WRITE(LU,1365)BOOLIST(J)
 1365 FORMAT(1H ,'NOTCOD =',I8)
      J=J+1
      WRITE(LU,1370)BOOLIST(J)
 1370 FORMAT(1H ,'GATNDX =',I8)
      GO TO 1350
C
 1400 WRITE(LU,90)
      WRITE(LU,1405)
 1405 FORMAT(1H ,'COMMON/PACL/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1410)
 1410 FORMAT(1H ,'   CDPAT   CDMSK   CDCNT'/)
      DO 1420 J=1,NCDN
      WRITE(LU,1415)CDPAT(J),CDMSK(J),CDCNT(J)
 1415 FORMAT(1H ,I8,Z8,I8)
 1420 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1425)
 1425 FORMAT(1H ,'COMMON/PACM/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1430)
 1430 FORMAT(1H ,'SOURCE FILE NAME IN ALL ITS GLORY:'/)
      WRITE(LU,1435)NAMPAC
 1435 FORMAT(1H ,20A4)
C
      WRITE(LU,90)
      WRITE(LU,1440)
 1440 FORMAT(1H ,'COMMON/PACN/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1445)
 1445 FORMAT(1H ,'      CAMC      CAMN      CAMT'/)
      DO 1460 I=1,NCAM
      WRITE(LU,1450)CAMC(I),CAMN(I),CAMT(I)
 1450 FORMAT(1H ,3I10)
 1460 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1465)
 1465 FORMAT(1H ,'CAMID TABLE (A VS N VS C) (32,32,8)'/)
C
      WRITE(LU,1470)CAMID
 1470 FORMAT(1H ,16Z6)
C
      WRITE(LU,90)
      WRITE(LU,1475)
 1475 FORMAT(1H ,'COMMON/PACP/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1480)
 1480 FORMAT(1H ,' DIR-ENTRY     DELAY'/)
      DO 1490 I=1,NDELAZ
      IF(DELAZ(I).EQ.0) GO TO 1490
      WRITE(LU,1485)I,DELAZ(I)
 1485 FORMAT(1H ,2I10)
 1490 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1505)
 1505 FORMAT(1H ,'COMMON/PACX/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1510)
 1510 FORMAT(1H ,'      XIAC      XIAN    XIAVSN    XIAGRP'/)
      DO 1520 I=1,NXIA
      WRITE(LU,1515)XIAC(I),XIAN(I),XIAVSN(I),XIAGRP(I)
 1515 FORMAT(1H ,4I10)
 1520 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1605)
 1605 FORMAT(1H ,'COMMON/PACV/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1610)
 1610 FORMAT(1H ,'    NUMBER   VADCMAP   VTDCMAP'/)
      DO 1620 I=1,10
      WRITE(LU,1615)I,VADCMAP(I),VTDCMAP(I)
 1615 FORMAT(1H ,3I10)
 1620 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1630)
      WRITE(LU,1635)
 1630 FORMAT(1H ,'ADC#=      1      2      3      4      5      6',
     &                '      7      8      9     10')
 1635 FORMAT(1H ,'CHAN#')
      DO 1650 I=1,34
      WRITE(LU,1640)I,(ADCID(I,J),J=1,10)
 1640 FORMAT(1H ,I5,10Z7)
 1650 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1660)
      WRITE(LU,1665)
 1660 FORMAT(1H ,'TDC#=      1      2      3      4      5      6',
     &                '      7      8      9     10')
 1665 FORMAT(1H ,'CHAN#')
      DO 1680 I=1,34
      WRITE(LU,1670)I,(TDCID(I,J),J=1,10)
 1670 FORMAT(1H ,I5,10Z7)
 1680 CONTINUE
C
      RETURN
      END
C$PROG VMEASS
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ******************************************************************
C
      SUBROUTINE VMEASS(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PACV/ VADCMAP(10),VTDCMAP(10),VADCID(340),VTDCID(340)
      INTEGER*4    VADCMAP,    VTDCMAP,    VADCID,     VTDCID
C     ------------------------------------------------------------------
      INTEGER*4    ADCID(34,10),TDCID(34,10)
C
      EQUIVALENCE (ADCID,VADCID),(TDCID,VTDCID)
C     ------------------------------------------------------------------
C 
      INTEGER*4 IWD(20),ILO(20),IHI(20),IT(5),NAME(3)
C
      INTEGER*4 CODES(5)
C
      DATA CODES/'ADC ','TDC ','A   ','ID  ','MT  '/
C
      DATA NCODE/5/
C
      DATA MAXADC,MAXTDC/10,10/
C
      CHARACTER*12 ADCNAM,TDCNAM
C
      DATA         ADCNAM/'CAEN-785    '/
      DATA         TDCNAM/'CAEN-775    '/
C
      CHARACTER*4  ICOD,JCODE,MODNAM
C
      INTEGER*4    X8000,BLANK
      DATA         X8000,BLANK/'8000'X,'    '/
C
      SAVE
C
C     ******************************************************************
C     PROCESSES ONE HARDWARE ASSIGNMENT LINE OF THE FORM:
C
C     $vme  adc0N  a0A-0B  NAME:0N,0I  id0N,0I  mt=MOTYP
C
C     $vme  tdc0N  a0A-0B  NAME:0N,0I  id0N,0I  mt=MOTYP
C     ******************************************************************
C
      CALL CASEUP(IWD)
C
      NF=0
      IB=5
  100 IA=NXNB(IWD,IB,80)
      IF(IA.LE.0) GO TO 200
      IB=NXBL(IWD,IA,80)
      IF(IB.LE.0) IB=81
      IB=IB-1
      NF=NF+1
      ILO(NF)=IA
      IHI(NF)=IB
      IB=IB+1
      IF(IB.GE.80) GO TO 200
      GO TO 100
C
  200 MODNAM='    '
      MODNUM=0
      MOCODE=0
      SUBAD1=0
      SUBAD2=0
      IDNUM=0
      IDINC=0
C
      DO 300 N=1,NF
C
      DO 205 I=1,5
      IT(I)=BLANK
  205 CONTINUE
C
      CALL LODUP(IWD,ILO(N),IHI(N),IT,1)
C
      CALL HARDECO(IT,ICOD,NAME,I1,I2)
      JCODE=ICOD
C
      MODKOD=0
      IF(JCODE.EQ.'MT  ')   THEN
      CALL MODCOD(NAME,MODKOD,IERR)
                            ENDIF
C
      IF(JCODE.EQ.'ADC ') THEN
      IF(I1.LT.1)      GO TO 1000
      IF(I1.GT.MAXADC) GO TO 1000
      MODNAM='ADC '
      MODNUM=I1
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'TDC ') THEN
      IF(I1.LT.1)      GO TO 1010
      IF(I1.GT.MAXTDC) GO TO 1010
      MODNAM='TDC '
      MODNUM=I1
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'A   ') THEN
      IF(I1.LT.1)      GO TO 1020
      IF(I2.GT.34)     GO TO 1020
      IF(I1.GT.I2)     GO TO 1020
      SUBAD1=I1
      SUBAD2=I2
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'ID  ') THEN
      IF(I1.LE.0)      GO TO 1030
      IDNUM=I1
      IDINC=I2
      IF(IDINC.LE.0) IDINC=1
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'MT  ') THEN
      MOCODE=MODKOD
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'    ') THEN
      GO TO 300
      ENDIF
C
  300 CONTINUE
C
      IF(MODNAM.EQ.'    ') GO TO 1040
      IF(MOCODE.LE.0)      GO TO 1050
      IF(SUBAD1.LE.0)      GO TO 1060
      IF(IDNUM.LE.0)       GO TO 1070
C
      IF(MODNAM.EQ.'ADC '.AND.MOCODE.NE.42) GO TO 1080
      IF(MODNAM.EQ.'TDC '.AND.MOCODE.NE.41) GO TO 1080
C
      IF(MODNAM.EQ.'ADC ') GO TO 400
      IF(MODNAM.EQ.'TDC ') GO TO 500
      GO TO 1040
C
  400 ID=IDNUM+X8000
      DO 410 I=SUBAD1,SUBAD2
      CALL CKVMEID(ID)
      ADCID(I,MODNUM)=ID
      ID=ID+IDINC
  410 CONTINUE
      VADCMAP(MODNUM)=1
      CALL VMESAV(ADCNAM,MOCODE)
      RETURN
C
  500 ID=IDNUM+X8000
      DO 510 I=SUBAD1,SUBAD2
      CALL CKVMEID(ID)
      TDCID(I,MODNUM)=ID
      ID=ID+IDINC
  510 CONTINUE
      VTDCMAP(MODNUM)=1
      CALL VMESAV(TDCNAM,MOCODE)
      RETURN
C
C
      RETURN
C
 1000 WRITE(CMSSG,1005)MAXADC
 1005 FORMAT('Illegal ADC number - allowed range = 1 to ',I2)
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)MAXTDC
 1015 FORMAT('Illegal TDC number - allowed range = 1 to ',I2)
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Illegal subaddress specified - allowed range = 1,34')
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('Illegal parameter ID number specified')
      GO TO 2000
C
 1040 WRITE(CMSSG,1045)
 1045 FORMAT('Module Name not specified')
      GO TO 2000
C
 1050 WRITE(CMSSG,1055)
 1055 FORMAT('Module Type not specified')
      GO TO 2000
C
 1060 WRITE(CMSSG,1065)
 1065 FORMAT('Module subaddress not properly specified')
      GO TO 2000
C
 1070 WRITE(CMSSG,1075)
 1075 FORMAT('Parameter ID number/s not properly specified')
      GO TO 2000
C
 1080 WRITE(CMSSG,1085)
 1085 FORMAT('Module Name and Module Type do not match')
      GO TO 2000
C
C
 2000 CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG VMESAV
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/22/2000
C     ************************************************************
C
      SUBROUTINE VMESAV(MODNAM,MODKOD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C     ------------------------------------------------------------------
      INTEGER*4    MODNAM(3),MODKOD
C
      INTEGER*4    MODLIS(100),NMOD
C
      DATA                   NMOD/0/
C
      CHARACTER*4  KIMO,USED
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 I=1,NMOD
      IF(MODLIS(I).EQ.MODKOD) RETURN
   10 CONTINUE
C
      NMOD=NMOD+1
C
      MODLIS(NMOD)=MODKOD
C
      N=NUMT+1
C
      IF(N.GT.TMX) THEN
      WRITE(CMSSG,20)N
   20 FORMAT('/PAC2/ ARRAY OVERFLOW AT N =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      N=TMX
      ENDIF
C
      NAMO(1,N)=MODNAM(1)
      NAMO(2,N)=MODNAM(2)
      NAMO(3,N)=MODNAM(3)
C
      KIMO(N)='$VME'
C
      MOTY(1,N)=MODKOD
C
      USED(N)='NO  '
C
      NUMT=N
C
      RETURN
C
      END
C$PROG XIAASS
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/24/2000
C     ******************************************************************
C
      SUBROUTINE XIAASS(IWD)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
      DATA         NXIA,MXXIA/0,64/
C     ------------------------------------------------------------------
C
      INTEGER*4    IWD(20)
C
      INTEGER*4    JWD(20),LWD(2,40),ITYP(40),NF,NTER,XIANDX
C
      INTEGER*4    NDX(4),IVAL(4),CRAT,SLOT,VSN,GRP
C
      CHARACTER*1  CLWD(8,4),CODE(4)
C
      EQUIVALENCE (CLWD,LWD)
C
      EQUIVALENCE (CRAT,IVAL(1)),
     &            (SLOT,IVAL(2)),
     &            (VSN, IVAL(3)),
     &            (GRP, IVAL(4))
C
      INTEGER*4    BLANK
      DATA         BLANK/'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
C
      IF(NTER.NE.0) GO TO 500
C
      IF(NF.LT.3)   GO TO 500
C
      IF(NF.GT.4)   GO TO 500
C
      DO 20 I=1,NF
      CODE(I)=CLWD(1,I)
      NDX(I)=XIANDX(CODE(I))
      IF(NDX(I).EQ.0) GO TO 500
   20 CONTINUE
C
      DO 30 I=1,20
      JWD(I)=BLANK
   30 CONTINUE
C
      DO 40 I=1,NF
      CLWD(1,I)=' '
   40 CONTINUE
C
      N=0
      DO 60 J=1,NF
      DO 50 I=1,2
      N=N+1
      JWD(N)=LWD(I,J)
   50 CONTINUE
   60 CONTINUE
C
      CALL GREAD(JWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(NTER.NE.0) GO TO 500
C
      DO 70 I=1,4
      IVAL(I)=0
   70 CONTINUE
C
      DO 80 I=1,NF
      CALL MILV(LWD(1,I),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 500
      IVAL(NDX(I))=IV
   80 CONTINUE
C
      IF(CRAT.LT.0.OR.CRAT.GT.7)   GO TO 510
      IF(SLOT.LT.1.OR.SLOT.GT.31)  GO TO 520
      IF(VSN .LT.0.OR.VSN .GT.255) GO TO 530
      IF(GRP .LT.0.OR.GRP .GT.255) GO TO 540
C
      CALL XIACHEK(IVAL,IERR)
C
      NXIA=NXIA+1
C
      IF(NXIA.GT.MXXIA) GO TO 550
C
      XIAC(NXIA)  =IVAL(1)
      XIAN(NXIA)  =IVAL(2)
      XIAVSN(NXIA)=IVAL(3)
      XIAGRP(NXIA)=IVAL(4)
C
C
      RETURN
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error or illegal command')
C
      GO TO 1000
C
  510 WRITE(CMSSG,515)CRAT
  515 FORMAT('Illegal CRATE number =',I8)
      GO TO 1000
C
  520 WRITE(CMSSG,525)SLOT
  525 FORMAT('Illegal SLOT number =',I8)
      GO TO 1000
C
  530 WRITE(CMSSG,535)VSN
  535 FORMAT('Illegal Virtual Station# =',I8,' - legal# is 0-255')
      GO TO 1000
C
  540 WRITE(CMSSG,545)GRP
  545 FORMAT('Illegal GROUP number =',I8,' - legal# is 0-255')
      GO TO 1000
C
  550 WRITE(CMSSG,555)NXIA
  555 FORMAT('/PACX/ overflow at NXIA =',I8)
      NXIA=MXXIA
      GO TO 1000
C
 1000 CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG XIACHEK
C
      SUBROUTINE XIACHEK(IVAL,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
C     ------------------------------------------------------------------
C
      INTEGER*4    IVAL(4)
C
      SAVE
C
      IERR=0
C
      CTST=IVAL(1)
      NTST=IVAL(2)
      VTST=IVAL(3)
C
      DO 20 N=1,NUMT
      IF(MOTY(1,N).EQ.28) GO TO 20
      IF(CTST.NE.CRAT(N)) GO TO 20
      IF(NTST.NE.SLOT(N)) GO TO 20
      WRITE(CMSSG,10)CTST,NTST
   10 FORMAT('MULTIPLE DEFINITION FOR: TYPE, C,N = ',2I8)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
   20 CONTINUE
C
      DO 50 N=1,NXIA
      IF(CTST.NE.XIAC(N)) GO TO 50
      IF(NTST.NE.XIAN(N)) GO TO 50
      WRITE(CMSSG,40)CTST,NTST
   40 FORMAT('MULTIPLE DEFINITION FOR: TYPE, C,N = ',2I8)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
   50 CONTINUE
C
      DO 80 N=1,NXIA
      IF(VTST.NE.XIAVSN(N))GO TO 80
      WRITE(CMSSG,70)VTST
   70 FORMAT('MULTIPLE DEFINITION FOR: TYPE, VSN = ',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
   80 CONTINUE
C
      RETURN
C
      END
C$PROG XIANDX
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/24/2000
C     ******************************************************************
C
      INTEGER*4 FUNCTION XIANDX(CODE)
C
      CHARACTER*1  CODE
C
      INTEGER*4    NDX
C
      NDX=0
C
      IF(CODE.EQ.'C') NDX=1
      IF(CODE.EQ.'N') NDX=2
      IF(CODE.EQ.'V') NDX=3
      IF(CODE.EQ.'G') NDX=4
C
      XIANDX=NDX
C
      RETURN
      END
