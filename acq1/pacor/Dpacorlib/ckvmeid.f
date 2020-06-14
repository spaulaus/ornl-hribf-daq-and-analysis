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
      COMMON/PACV/ VADCMAP(12),VTDCMAP(12),VQDCMAP(12),
     .             VADCID(408),VTDCID(408),VQDCID(408)
      INTEGER*4    VADCMAP,    VTDCMAP,    VQDCMAP,
     .             VADCID,     VTDCID,     VQDCID
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
      DO 130 I=1,408
      IF(VADCID(I).LE.0)  GO TO 130
      IF(ID.NE.VADCID(I)) GO TO 130
      NDEF=NDEF+1
  130 CONTINUE
C
      DO 140 I=1,408
      IF(VTDCID(I).LE.0)  GO TO 140
      IF(ID.NE.VTDCID(I)) GO TO 140
      NDEF=NDEF+1
  140 CONTINUE
C
      DO 150 I=1,408
      IF(VQDCID(I).LE.0)  GO TO 150
      IF(ID.NE.VQDCID(I)) GO TO 150
      NDEF=NDEF+1
  150 CONTINUE
C
      IF(NDEF.EQ.0) RETURN
C
      JTST=ID-X8000
C
      WRITE(CMSSG,160)JTST,NDEF
  160 FORMAT('ID-NUMBER',I8,'    MULTIPLY DEFINED (',I5,' TIMES)')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
