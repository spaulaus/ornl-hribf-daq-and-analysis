C$PROG CLIMFIX   - Removes non-numeric bytes from data fields 
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 11/13/2004
C     ******************************************************************
C
      SUBROUTINE CLIMFIX(IWD)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      BYTE       IWD(80),JWD(80),X20,X30,X39,X21,X3B
C
      DATA       X20,X30,X39,X21,X3B/Z'20',Z'30',Z'39',Z'21',Z'3B'/
C
      INTEGER*4  I,N
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C     Removes non-numeric characters from data field/s and squeezes left
C     ------------------------------------------------------------------
C
      DO 10 I=1,5                                  !Save first 5 bytes
      JWD(I)=IWD(I)
   10 CONTINUE
C
      DO 20 I=6,80                                 !Blank rest of output
      JWD(I)=X20    
   20 CONTINUE
C
      N=5                                          !init output pointer
C
      DO 40 I=6,80                                 !Loop on input 
      IF(IWD(I).EQ.X21) GO TO 50                   !quitif ;
      IF(IWD(I).EQ.X3B) GO TO 50                   !quitif !
      IF(IWD(I).EQ.X20) GO TO 30                   !saveif blank
      IF(IWD(I).GE.X30.AND.IWD(I).LE.X39) GO TO 30 !saveif numeric
      GO TO 40                                     !skipif otherwise
   30 N=N+1                                        !inc output pointer
      JWD(N)=IWD(I)                                !save in output
   40 CONTINUE
C
   50 DO 100 I=1,80                                !load back into IWD
      IWD(I)=JWD(I)
  100 CONTINUE
C
      RETURN
      END
