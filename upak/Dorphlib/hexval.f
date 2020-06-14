C$PROG HEXVAL    - Processes ACSII hex number & returns value
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE HEXVAL(IWD,IV,IERR)
C
      INTEGER*4    IWD(2),JWD(2)
C
      CHARACTER*8  CJWD
C
      EQUIVALENCE (CJWD,JWD)
C
C     ------------------------------------------------------------------
      INTEGER*4 X20,X30,X39,X41,X46
C
      DATA      X20,X30,X39,X41,X46/Z'20',Z'30',Z'39',Z'41',Z'46'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     "IWD" CONTAINS RIGHT-JUSTIFIED ASCII STRING REPRESENTING A
C     HEXADECIMAL NUMBER
C     FIRST CHARACTER MUST BE A DECIMAL DIGIT
C     LAST  CHARACTER MUST BE CHARACTER "H"
C     THE NUMERICAL VALUE IS RETURNED IN "IV"
C     ------------------------------------------------------------------
C
      IV=0
      IERR=0
      CJWD=' '
C
      I=7
      J=7
      DO 40 N=1,8
      CALL ILBYTE(IT,IWD,I)
      IF(IT.EQ.X20) GO TO 30
      IF(IT.GE.X30.AND.IT.LE.X39) GO TO 20
      IF(IT.GE.X41.AND.IT.LE.X46) GO TO 20
      GO TO 50
C
   20 CALL ISBYTE(IT,JWD,J)
      J=J-1
C
   30 I=I-1
   40 CONTINUE
C
      READ(CJWD,45)IV
   45 FORMAT(Z8)
      RETURN
C
   50 IERR=1
      RETURN
      END
