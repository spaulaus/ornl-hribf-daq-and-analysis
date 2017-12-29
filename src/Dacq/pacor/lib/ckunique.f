C$PROG CKUNIQUE
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     03/03/08  CAEN 792 support
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
      COMMON/PACV/ VADCMAP(24),VTDCMAP(12),VQDCMAP(12),
     .             VSISMAP(2),VMYRMAP(1),
     .             VADCID(816),VTDCID(408),VQDCID(408),
     .             VSISID(192),VMYRID(3)
      INTEGER*4    VADCMAP,    VTDCMAP,    VQDCMAP,
     .             VSISMAP,    VMYRMAP,
     .             VADCID,     VTDCID,     VQDCID,
     .             VSISID,     VMYRID
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
      DO 130 I=1,816
      IF(VADCID(I).LE.0)    GO TO 130
      IF(ITST.NE.VADCID(I)) GO TO 130
      NDEF=NDEF+1
  130 CONTINUE
C
      DO 140 I=1,408
      IF(VTDCID(I).LE.0)    GO TO 140
      IF(ITST.NE.VTDCID(I)) GO TO 140
      NDEF=NDEF+1
  140 CONTINUE
C
      DO 150 I=1,408
      IF(VQDCID(I).LE.0)    GO TO 150
      IF(ITST.NE.VQDCID(I)) GO TO 150
      NDEF=NDEF+1
  150 CONTINUE
C
      DO 160 I=1,192
      IF(VSISID(I).LE.0)    GO TO 160
      IF(ITST.NE.VSISID(I)) GO TO 160
      NDEF=NDEF+1
  160 CONTINUE
C
      DO 170 I=1,3
      IF(VMYRID(I).LE.0)    GO TO 170
      IF(ITST.NE.VMYRID(I)) GO TO 170
      NDEF=NDEF+1
  170 CONTINUE
C
      IF(NDEF.EQ.0) RETURN
C
      JTST=ITST-X8000
C
      WRITE(CMSSG,180)JTST,NDEF
  180 FORMAT('ID-NUMBER',I8,'    MULTIPLY DEFINED (',I5,' TIMES)')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
