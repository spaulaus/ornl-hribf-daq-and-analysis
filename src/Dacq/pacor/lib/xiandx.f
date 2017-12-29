C$PROG XIANDX
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/24/2000
C     ******************************************************************
C
      INTEGER*4 FUNCTION XIANDX(CODE)
C
      CHARACTER*1  CODE
C
      INTEGER*4    NDX
C
      NDX=0
C
      IF(CODE.EQ.'C') NDX=1
      IF(CODE.EQ.'N') NDX=2
      IF(CODE.EQ.'V') NDX=3
      IF(CODE.EQ.'G') NDX=4
C
      XIANDX=NDX
C
      RETURN
      END
