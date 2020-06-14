C$PROG PACK      - Packs lo-order bytes from ICH into IWD
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/25/2002
C     ******************************************************************
C
      SUBROUTINE PACK(ICH,IWD,N)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4 ICH(*),IWD(*),N
C
      INTEGER*4 JTEMP,I
C     ------------------------------------------------------------------
C
      DO 10 I=1,N
      JTEMP=ICH(I)
      CALL ISBYTE(JTEMP,IWD,I-1)
   10 CONTINUE
      RETURN
      END
