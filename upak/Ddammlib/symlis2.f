C$PROG SYMLIS2   - Selects symbol sequence for dot-mode 2-D displays
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SYMLIS2(DOTSIZ,NZLEV,NZVAL,SYMPO)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      INTEGER*4 SYMS(10,10),SYML(10,10),SYMPO(*)
C
      DATA SYMS/ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
     &           0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
     &           3,  6,  9,  0,  0,  0,  0,  0,  0,  0,  
     &           2,  4,  6,  8,  0,  0,  0,  0,  0,  0, 
     &           1,  3,  5,  7,  9,  0,  0,  0,  0,  0,  
     &           0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
     &           0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
     &           0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
     &           1,  2,  3,  4,  5,  6,  7,  8,  9,  0, 
     &           0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
C
      DATA SYML/ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
     &           0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
     &           8, 16, 24,  0,  0,  0,  0,  0,  0,  0,  
     &           6, 12, 18, 24,  0,  0,  0,  0,  0,  0, 
     &           5, 10, 15, 20, 25,  0,  0,  0,  0,  0,  
     &           4,  8, 12, 16, 20, 24,  0,  0,  0,  0, 
     &           4,  7, 10, 13, 16, 19, 22,  0,  0,  0,  
     &           3,  6,  9, 12, 14, 16, 18, 24,  0,  0,  
     &           4,  6,  8, 10, 12, 14, 16, 18, 20,  0, 
     &           4,  6,  8, 10, 12, 14, 16, 18, 20, 22/
C
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO SELECT SYMBOL SEQUENCE FOR DOT-MODE 2-D DISPLAYS
C     ------------------------------------------------------------------
C
      IF(DOTSIZ.GE.5) GO TO 100
C
      NZVAL=NZLEV
      IF(NZVAL.LT.3) NZVAL=3
      IF(NZVAL.GT.5) NZVAL=9
C
      DO 10 I=1,10
      SYMPO(I)=SYMS(I,NZVAL)
   10 CONTINUE
      RETURN
C
  100 NZVAL=NZLEV
      IF(NZVAL.LT.3)  NZVAL=3
      IF(NZVAL.GT.10) NZVAL=10
C
      DO 110 I=1,10
      SYMPO(I)=SYML(I,NZVAL)
  110 CONTINUE
      RETURN
      END
