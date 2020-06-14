C$PROG GLABNDX   - Searches for & returns LABEL index in current LIST
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      FUNCTION GLABNDX(LABL)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      INTEGER*4 LABL(3)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      DO 20 N=1,NSCA
      DO 10 I=1,3
      IF(LABL(I).NE.LAG(I,N)) GO TO 20
   10 CONTINUE
      GO TO 50
   20 CONTINUE
C
      GLABNDX=0
      RETURN
C
   50 GLABNDX=N
      RETURN
      END
