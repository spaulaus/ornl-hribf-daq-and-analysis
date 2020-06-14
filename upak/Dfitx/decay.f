C$PROG DECAY
C
      REAL*4 LA,LB,T,N0
C
      N0=10000.0
C
      LA=0.2
C
      LB=0.05
C
      WRITE(7,10)
   10 FORMAT('DATA')
      DO 100 I=1,50
C
      T=2*I
C
      Y=N0*(LA/(LB-LA))*(EXP(-LA*T)-EXP(-LB*T))
C
      WRITE(6,20)T,Y
   20 FORMAT(1H ,2F12.4)
      WRITE(7,30)T,Y
   30 FORMAT(2F10.2)
  100 CONTINUE
      WRITE(7,110)
  110 FORMAT('ENDA')
      CALL EXIT(0)
      END
