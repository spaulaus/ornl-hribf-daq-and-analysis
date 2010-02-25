C$PROG LABNDX
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      FUNCTION LABNDX(LABL)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
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
