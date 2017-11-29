C$PROG NORMAN
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE NORMAN(KMD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      real*8 vn, vo, vd
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
      DATA          NORI,NORF/-1,1.0/
C     ------------------------------------------------------------------
      COMMON/SCATD/ NORISAV,NORFSAV
      INTEGER*4     NORISAV
      REAL*4        NORFSAV
      DATA          NORISAV,NORFSAV/-1,1.0/
C     ------------------------------------------------------------------
C
      CHARACTER*4   KMD
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'NORS') GO TO 10
      IF(KMD.EQ.'NORT') GO TO 20
      IF(KMD.EQ.'UNOR') GO TO 30
                        RETURN
C
   10 NORI=NORISAV
      NORF=NORFSAV
      RETURN
C
   20 NORI=-1
      NORF=1.0
      RETURN
C
   30 NORI=0
      NORF=1.0
      RETURN
      END
