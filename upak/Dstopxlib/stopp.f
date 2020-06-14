C$PROG STOPP     - S(ELE+NUC) for specified energy (used by RANGE)
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      REAL*4 FUNCTION STOPP(EA)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/CONTROL/ ISW1,ISW2,ISW3,ISW4
      INTEGER*4       ISW1,ISW2,ISW3
      LOGICAL                        ISW4
C     ------------------------------------------------------------------
      COMMON/PARTICLE/IZT,IZP,ZT,ZP,AT,AP,SPWRH
      INTEGER*4       IZT,IZP
      REAL*4                  ZT,ZP,AT,AP,SPWRH
C     ------------------------------------------------------------------
      REAL*4          NUSPWR,ELSPWR,SPWRHI,EA
C
      INTEGER*4       IE
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IE=EA
C
      IF(ISW1.EQ.IZT.AND.ISW2.EQ.IE) GO TO 50
C
      ISW2=IE
C
      SPWRH=ELSPWR(EA)
C
      STOPP=SPWRH
C
   50 IF(IZP.GT.1) STOPP=SPWRHI(EA)
C
      IF(ISW4) RETURN
C
      IF(EA.GT.2000.) RETURN
C
      STOPP=STOPP+NUSPWR(EA)
C
      RETURN
      END
