C$PROG STRIPATH
C
      SUBROUTINE STRIPATH(NAME,N)
C
      BYTE    NAME(*)
C
      BYTE    X20,X2F
C
      DATA    X20,X2F/'20'X,'2F'X/
C
      SAVE
C
      J=N+1
      DO 10 I=1,N
      J=J-1
      IF(NAME(J).EQ.X2F) GO TO 20
   10 CONTINUE
      RETURN
C
   20 JA=J+1
      II=0
      DO 30 J=JA,N
      II=II+1
      NAME(II)=NAME(J)
   30 CONTINUE
C
      IA=II+1
      DO 40 I=IA,N
      II=II+1
      NAME(II)=X20
   40 CONTINUE
      RETURN
      END
