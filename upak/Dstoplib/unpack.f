C$PROG UNPACK    - Unpacks bytes from IWD to lo-order bytes in ICH
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      SUBROUTINE UNPACK(IWD,ICH,N)
C
      INTEGER*4 ICH(1),IWD(1),JTEMP
C
      DO 10 I=1,N
      CALL ILBYTE(JTEMP,IWD,I-1)
      ICH(I)=JTEMP
   10 CONTINUE
      RETURN
      END
