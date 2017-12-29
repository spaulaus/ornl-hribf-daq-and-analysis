C$PROG SENDBUF   - Sends buffer for SCAD display
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE SENDBUF(LU,IBUF,NBY)
C
      CHARACTER*1 IBUF(*)
C
      SAVE
C
      WRITE(LU,10)(IBUF(I),I=1,NBY)
CX 10 FORMAT('+',$,120A1)
   10 FORMAT($,120A)
      RETURN
      END
