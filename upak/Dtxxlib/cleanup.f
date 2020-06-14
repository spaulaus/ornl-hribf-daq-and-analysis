C$PROG CLEANUP
C
      SUBROUTINE CLEANUP(IBY)
C
      BYTE  IBY(80),JBY(80)
C
      BYTE  X20,X5C
C
      DATA  X20,X5C/'20'X,'5C'X/
C
      DO 10 I=1,80
      JBY(I)=IBY(I)
      IBY(I)=X20
   10 CONTINUE
C
      I=0
      J=0
  100 J=J+1
      IF(J.GT.80) RETURN
C
      IF(JBY(J).NE.X5C) THEN
                        I=I+1
                        IBY(I)=JBY(J)
                        GO TO 100
                        ENDIF
C
  110 J=J+1
      IF(J.GT.80) RETURN
      IF(JBY(J).NE.X5C) GO TO 110
      GO TO 100
C
      END 
