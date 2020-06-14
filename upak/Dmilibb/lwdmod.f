C$PROG LWDMOD    - Modifies inp line (replaces symbol names with values)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE LWDMOD
C   
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C   
      COMMON/ML03/ ISYN(500),ISYV(500),NSYM
C
      CHARACTER*8  ISYN
C
      CHARACTER*8  CLWD(40)
      EQUIVALENCE (CLWD,LWD)
C
      SAVE
C   
      DO 50 J=2,NF
      DO 20 I=1,NSYM
      IF(CLWD(J).NE.ISYN(I)) GO TO 20
      WRITE(CLWD(J),10)ISYV(I)
   10 FORMAT(I8)
      ITYP(J)=2
      GO TO 50
   20 CONTINUE
   50 CONTINUE
      RETURN
      END
