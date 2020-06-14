C$PROG REDSTOP
      SUBROUTINE REDSTOP(LIN)
C   
      COMMON/BBB/ KBUF(3120)
C   
      DIMENSION   XBUF(3120),TARG(10)
C   
      EQUIVALENCE (XBUF,KBUF)
C   
      N=0
      DO 60 II=1,30
      DO 40 KK=1,4
      READ(LIN,20)TARG,NAMT
   20 FORMAT(10F7.0,A2)
      DO 30 I=1,10
      N=N+1
      XBUF(N)=TARG(I)
   30 CONTINUE
   40 CONTINUE
      N=N+1
      KBUF(N)=NAMT
   60 CONTINUE
C   
C     READ IN LIST OF CONSTANTS ASSOCIATED WITH EACH ELEMENT
C   
      DO 100 II=1,210
      READ(LIN,65)A,B,C,D,E,ICODE,AMU,NAME,CHGI
   65 FORMAT(5F12.0,I2,F6.0,A2,4X,F5.0)
      N=N+1
      XBUF(N)=A
      N=N+1
      XBUF(N)=B
      N=N+1
      XBUF(N)=C
      N=N+1
      XBUF(N)=D
      N=N+1
      XBUF(N)=E
      N=N+1
      KBUF(N)=ICODE
      N=N+1
      XBUF(N)=AMU
      N=N+1
      KBUF(N)=NAME
      N=N+1
      XBUF(N)=CHGI
  100 CONTINUE
      RETURN
      END
