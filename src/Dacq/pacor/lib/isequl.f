C$PROG ISEQUL
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      FUNCTION ISEQUL(IWD,JWD,N)
C
      INTEGER*4 IWD(*),JWD(*)
C
      INTEGER*4  YES,NO
      character*4  cYES,cNO
      equivalence (cYES, YES), (cNO,NO)
      DATA       cYES,cNO/'YES ','NO  '/
C
C
      DO 10 I=1,N
      IF(IWD(I).NE.JWD(I)) GO TO 100
   10 CONTINUE
      ISEQUL=YES
      RETURN
C
  100 ISEQUL=NO
      RETURN
      END
