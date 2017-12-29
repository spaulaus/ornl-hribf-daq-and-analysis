C$PROG NORMAN    - Saves & recalls normalization values for process SCAD
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C

      SUBROUTINE NORMAN
C
      IMPLICIT INTEGER*4 (A-Z)
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
C
      CHARACTER*4  LCMD
C
      REAL*4 NORFSAV
C
      DATA NORISAV,NORFSAV,LCMD/-1,1.0,'NONE'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'NORS') GO TO 100
      IF(KMD.EQ.'NORT') GO TO 200
                        RETURN
C
  100 IF(LCMD.NE.'NORT') RETURN
      NORI=NORISAV
      NORF=NORFSAV
      LCMD='NORS'
      RETURN
C
  200 IF(LCMD.EQ.'NORT') RETURN
      NORISAV=NORI
      NORFSAV=NORF
      NORI=-1
      NORF=1.0
      LCMD='NORT'
      RETURN
      END
