C$PROG LEGLASS
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 01/27/96
C     ************************************************************
C
      SUBROUTINE LEGLASS(LINE,N)
C
      BYTE LINE(*)
C
      BYTE X20,X7E
C
      DATA X20,X7E/'20'X,'7E'X/
C
      SAVE
C
      DO 10 I=1,80
      IF(LINE(I).LT.X20) GO TO 100
      IF(LINE(I).GT.X7E) GO TO 100
   10 CONTINUE
      RETURN
C
  100 WRITE(6,105)N
  105 FORMAT(1H ,'Illegal character on line no.',I6)
      WRITE(6,110)
  110 FORMAT(1H ,'Compilation aborted!')
      STOP
      END
