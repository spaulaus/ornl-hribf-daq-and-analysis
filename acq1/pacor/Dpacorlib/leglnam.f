C$PROG LEGLNAM
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      FUNCTION LEGLNAM(NAME)
C
      CHARACTER*4  NAME(*)
C
      INTEGER*4    YES,NO
      character*4  cYES, cNO
      equivalence (cYES, YES), (cNO, NO)
C
      DATA         cYES,cNO/'YES ','NO  '/
C
      SAVE
C
      IF(NAME(1).EQ.'AND ') GO TO 10
      IF(NAME(1).EQ.'OR  ') GO TO 10
      IF(NAME(1).EQ.'NOT ') GO TO 10
C
      LEGLNAM=YES
      RETURN
C
   10 LEGLNAM=NO
      RETURN
      END
