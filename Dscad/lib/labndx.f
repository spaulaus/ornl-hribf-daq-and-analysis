C$PROG LABNDX    - Searches for & returns LABEL index in current LIST
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      FUNCTION LABNDX(LABL)
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
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      INTEGER*4 LABL(3)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 20 N=1,NT
      DO 10 I=1,3
      IF(LABL(I).NE.LA(I,N)) GO TO 20
   10 CONTINUE
      GO TO 50
   20 CONTINUE
C
      LABNDX=0
      RETURN
C
   50 LABNDX=N
      RETURN
      END
