C$PROG BUFLOD    - Loads one buffer into another 
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      SUBROUTINE BUFLOD(IBUF,NBYT,ISEC)
C
      COMMON/AAA/ KBUF(3120)
C
      INTEGER*4 IBUF(*)
C
      NW=NBYT/4
C
      IA=64*ISEC
C
      DO 10 I=1,NW
      IA=IA+1
      IBUF(I)=KBUF(IA)
   10 CONTINUE
      RETURN
      END
