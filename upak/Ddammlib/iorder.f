C$PROG IORDER    - Sets array IX in the order specified by array IORD
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE IORDER(IX,IORD,IT,N)
C   
      INTEGER*4 IX(*),IORD(*),IT(*)
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IF(N.LE.1) RETURN
C   
      DO 10 I=1,N
      IT(I)=IX(I)
   10 CONTINUE
C   
      DO 20 I=1,N
      J=IORD(I)
      IX(I)=IT(J)
   20 CONTINUE
C   
      RETURN
      END
