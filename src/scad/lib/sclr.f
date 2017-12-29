C$PROG SCLR      - Clears display screen for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE SCLR
C
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      CHARACTER*4  KTERM
C     ------------------------------------------------------------------
C
      INTEGER*4 ICLR(2),NBY(2)
C
      DATA ICLR/'00002B1B'X,'4A325B1B'X/
C
      DATA NBY/2,4/
C
      SAVE
C
C     ------------------------------------------------------------------
C     CLEARS TERMINAL SCREEN
C     ------------------------------------------------------------------
C
      IDX=1
      IF(KTERM.EQ.'ANSI') IDX=2
C
      CALL SENDBUF(6,ICLR(IDX),NBY(IDX))
C
      RETURN
      END
