C$PROG SETZECL
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE SETZECL
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
      CHARACTER*4   TY,KI,LA
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SCATC/ CNZ(100),SNZ(100),AZ(100),NZOT
      INTEGER*4     CNZ,     SNZ,     AZ,     NZOT
C     ------------------------------------------------------------------
C
      SAVE
C
C     ******************************************************************
C     SET UP ZOT-LIST for NON-ECL SCALERS
C     ******************************************************************
C
      NZOT=0
      DO 50 I=1,NR
      IF(TY(I).EQ.'ECL ') GO TO 50
      DO 20 J=1,NZOT
      IF(CNZ(J).EQ.CN(I).AND.SNZ(J).EQ.SN(I)) GO TO 50
   20 CONTINUE
      NZOT=NZOT+1
      CNZ(NZOT)=CN(I)
      SNZ(NZOT)=SN(I)
      AZ(NZOT) =0
   50 CONTINUE
C
C     ******************************************************************
C     DETERMINE WHICH SCALERS (IF ANY) ARE ECL-TYPE AND
C     SETS UP AUXILIARY ARRAYS TO DIRECT THE READING OF SUCH SCALERS
C     ******************************************************************
C
      NONEC=0
      DO 100 N=1,NR
      IF(TY(N).EQ.'ECL ') GO TO 100
      NONEC=NONEC+1
  100 CONTINUE
C
      NEC=0
      DO 200 N=1,NR
      PV(N)=0
C
      IF(TY(N).NE.'ECL ') GO TO 200
C
      NEI=0
      DO 120 I=1,NEC
      NEI=NEI+1
      IF(ECN(I).EQ.CN(N).AND.ESN(I).EQ.SN(N)) GO TO 140
  120 CONTINUE
C
      NEC=NEC+1
      ECN(NEC)=CN(N)
      ESN(NEC)=SN(N)
      PV(N)=32*(NEC-1)+A(N)+1+NONEC+NEC
      GO TO 200
C
  140 PV(N)=32*(NEI-1)+A(N)+1+NONEC+NEC
C
  200 CONTINUE
      RETURN
      END
