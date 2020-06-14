C$PROG PACK      - Packs lo-order bytes from ICH into IWD
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      SUBROUTINE PACK(ICH,IWD,N)
C
      INTEGER*4 ICH(1),IWD(1),JTEMP
C
      DO 10 I=1,N
      JTEMP=ICH(I)
      CALL ISBYTE(JTEMP,IWD,I-1)
   10 CONTINUE
      RETURN
      END
