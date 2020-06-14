C$PROG UNPACK    _ Unpacks bytes into full words
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE UNPACK(IBY,JWD,N)
C
      BYTE IBY(*)
C
      INTEGER*4 JWD(*)
C
      SAVE
C
      DO 10 I=1,N
      JWD(I)=IBY(I)
   10 CONTINUE
      RETURN
      END
