C$PROG USEND     - Processes UCOM (deletes UCOM & calls USERCMP)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE USEND(IWD,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    IWD(20),IERR
C
      INTEGER*4    NXBL,NXNB,IA,N,ITMP,I
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
C     PROCESS "UCOM" - DELETE "UCOM" & LEADING BLANKS - CALL USERCMP
C     ------------------------------------------------------------------
C
      IERR=0
C
      IA=NXBL(IWD,1,80)
      IF(IA.EQ.0) GO TO 200
      IA=NXNB(IWD,IA,80)
      IF(IA.EQ.0) GO TO 200
      N=0
      DO 110 I=IA,80
      CALL ILBYTE(ITMP,IWD,I-1)
      CALL ISBYTE(ITMP,IWD,N)
      N=N+1
  110 CONTINUE
      ITMP=Z'20'
      IA=N+1
      IF(IA.GT.80) GO TO 130
      DO 120 I=IA,80
      CALL ISBYTE(ITMP,IWD,N)
      N=N+1
  120 CONTINUE
C
  130 CALL USERCMP(IWD)
      RETURN
C
  200 IERR=1
      RETURN
      END
