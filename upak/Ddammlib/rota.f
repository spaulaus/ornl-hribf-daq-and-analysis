C$PROG ROTA      - Rotates a banana into standard position (order)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE ROTA(IX,IY,NP)
C   
      INTEGER*4 IX(64),IY(64),IXX(64),IYY(64),MIN
C
      SAVE
C   
      IF(NP.LE.1) RETURN
      MIN=IX(1)
      IP=1
C   
      DO 10 I=2,NP
      IF(IX(I).GE.MIN) GO TO 10
      MIN=IX(I)
      IP=I
   10 CONTINUE
C   
      IF(IP.EQ.1) RETURN
      IPM=IP-1
      N=0
C   
      DO 20 I=IP,NP
      N=N+1
      IXX(N)=IX(I)
      IYY(N)=IY(I)
   20 CONTINUE
C   
      DO 30 I=1,IPM
      N=N+1
      IXX(N)=IX(I)
      IYY(N)=IY(I)
   30 CONTINUE
C   
      DO 40 I=1,NP
      IX(I)=IXX(I)
      IY(I)=IYY(I)
   40 CONTINUE
      RETURN
      END
