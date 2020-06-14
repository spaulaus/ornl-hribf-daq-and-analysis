C$PROG HISSUB    - Dummy/example HISSUB
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/17/99
C     ******************************************************************
C
      SUBROUTINE HISSUB(IBUF,NB)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*2 IBUF(*),MASK,XFFFF
C
      INTEGER*4 NB,ID,IX,I
C   
      DATA ID/0/
C
      DATA MASK/'0FFF'X/
C
      DATA XFFFF/'FFFF'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     EXAMPLE HISSUB - WHICH HISTOGRAMS ALL RAW PARMS AT 1/4 RES
C                    - WITH HIS-ID = PARM-ID
C     ------------------------------------------------------------------
C
C   
      ID=IAND(IBUF(I),MASK)
C
      DO 100 I=1,NB
C
      IF(IBUF(I).GE.0) GO TO 50
C
      IF(IBUF(I).EQ.XFFFF) THEN
                           ID=0
                           GO TO 100
                           ENDIF
C
      ID=IAND(IBUF(I),MASK)-1
      GO TO 100
C
   50 ID=ID+1
      IX=IBUF(I)/4
      CALL COUNT1(ID,IX,0)
C
  100 CONTINUE
      RETURN
      END
