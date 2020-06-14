C$PROG NUMASS    - Returns an ASCII representation of a number
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE NUMASS(NUM,ANUM,NDIG)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      CHARACTER*12 ANUM,ITMP,JTMP
C
      CHARACTER*1  CEXP
C
      INTEGER*4    NUM,NDIG,IEXP,I
C
      REAL*4       XNUM
C
c     BYTE         IBYT(12),JBYT(12),X20,CBYT
      integer(kind=1) IBYT(12),JBYT(12),X20,CBYT
C
      EQUIVALENCE (IBYT,ITMP),(JBYT,JTMP),(CEXP,CBYT)
C
      SAVE
C
C     ------------------------------------------------------------------
C     PRODUCES AN ASCII REPRESENTATION OF THE NUMBER NUM
C     AND RETURNS IT IN THE ARRAY ANUM.
C     NDIG IS THE ACTUAL NUMBER OF DIGITS GENERATED.
C     ANUM MAY BE A FIXED-POINT OR EXPODENTIAL REPRESENTATION,
C     DEPENDING ON THE SIZE OF NUM.
C     ------------------------------------------------------------------
C
      X20=Z'20'
C
      ANUM=' '
      JTMP=' '
      NDIG=0
C
      IF(NUM.GE.-99999.AND.NUM.LE.999999) GO TO 20
      IF(NUM.GT.0)                        GO TO 50
      IF(NUM.LT.0)                        GO TO 100
      RETURN
C
C     ------------------------------------------------------------------
C     Positive or negative number can be displayed as simple integer
C     ------------------------------------------------------------------
C
   20 WRITE(JTMP,25)NUM
   25 FORMAT(I6)
C
      NDIG=6
      DO 30 I=1,6
      IF(JBYT(I).NE.X20) GO TO 40
      NDIG=NDIG-1
   30 CONTINUE
C
   40 ANUM=JTMP
      RETURN
C
C     ------------------------------------------------------------------
C     Display positive number as 4 digits plus 1-digit exponent
C     ------------------------------------------------------------------
C
   50 XNUM=NUM
      WRITE(ITMP,55)XNUM
   55 FORMAT(E12.4)
C
      JBYT(1)=IBYT(5)
      JBYT(2)=IBYT(6)
      JBYT(3)=IBYT(7)
      JBYT(4)=IBYT(8)
      JBYT(5)=IBYT(10)
      READ(ITMP,60)IEXP
   60 FORMAT(10X,I2)
      IEXP=IEXP-4
      WRITE(CEXP,65)IEXP
   65 FORMAT(I1)
      JBYT(6)=CBYT
      GO TO 200
C
C     ------------------------------------------------------------------
C     Display negative number as 3 digits plus 1-digit exponent
C     ------------------------------------------------------------------
C
  100 XNUM=NUM
      WRITE(ITMP,105)XNUM
  105 FORMAT(E12.3)
C
      JBYT(1)=IBYT(3)
      JBYT(2)=IBYT(6)
      JBYT(3)=IBYT(7)
      JBYT(4)=IBYT(8)
      JBYT(5)=IBYT(10)
      READ(ITMP,60)IEXP
      WRITE(CEXP,65)IEXP
      IEXP=IEXP-3
      JBYT(6)=CBYT
C
  200 ANUM=JTMP
      NDIG=6
      RETURN
      END
