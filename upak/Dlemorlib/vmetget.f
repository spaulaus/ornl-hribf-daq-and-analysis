C$PROG VMETGET   - Extracts VME clock from first 4 16-bit words of LIST
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 09/20/2004
C     ******************************************************************
C
      SUBROUTINE VMETGET(LIST,TIM)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM32/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
C     ------------------------------------------------------------------
      INTEGER*2    LIST(*)
C
      INTEGER*4    ID,DATA,DATAHI,DATALO,THI,TLO,TIM,I,N
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      THI=0
      TLO=0
      TIM=0
      VMETIM=0
C
      IF(CLIDHI.LE.0) RETURN
C
      DO 100 I=1,16384,2
C
      N=I
C
      ID=LIST(I)
C
      DATA=LIST(I+1)
C
      IF(ID.EQ.-1.AND.DATA.EQ.-1) RETURN
C
      ID=IAND(ID,Z'7FFF')
C
      DATA=IAND(DATA,Z'FFFF')
C
      IF(ID.EQ.CLIDHI) GO TO 200
C
  100 CONTINUE
C
      RETURN
C
  200 DATAHI=DATA
C
      N=N+2
C
      ID=LIST(N)
C
      ID=IAND(ID,Z'7FFF')
C
      IF(ID.NE.CLIDLO) RETURN
C
      DATA=LIST(N+1)
C
      DATALO=IAND(DATA,Z'FFFF')
C
      THI=DATAHI
      TLO=DATALO
C
      TIM=65536*THI+TLO
C
      VMETIM=TIM
C
      RETURN
C
      END
