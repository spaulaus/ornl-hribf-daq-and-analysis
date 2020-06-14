C$PROG IVALU     - Decodes unsigned ASCII integers
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE IVALU(IWD,IV,IERR)
C
      INTEGER*4 IWD(2)
C
      INTEGER*4 X20,X30,X39
C
      DATA      X20,X30,X39/Z'20',Z'30',Z'39'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO DECODE ASCII INTEGER RIGHT JUSTIFIED IN FIELD OF 8
C     ACCEPTS ONLY UNSIGNED INTEGERS
C
C     IF(IERR.NE.0) IV IS UNCHANGED
C     ------------------------------------------------------------------
C
      IERR=0
      KV=0
      MUL=1
      N=7
      DO 20 I=1,8
      CALL ILBYTE(IT,IWD,N)
      IF(IT.EQ.X20) GO TO 15
      IF(IT.LT.X30.OR.IT.GT.X39) GO TO 50
      KV=KV+MUL*(IT-X30)
      MUL=10*MUL
   15 N=N-1
   20 CONTINUE
      IV=KV
      RETURN
   50 IERR=1
      RETURN
      END
