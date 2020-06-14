C$PROG CASEUP1   - Converts ASCII string (cmd only) to upper case
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE CASEUP1(IBY)
C
      INTEGER*1 IBY(80)    !Was type BYTE on Alpha
C
      INTEGER*1 X61,X7A,X20
C
      DATA      X61,X7A,X20/Z'61',Z'7A',Z'20'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     CONVERT COMMAND-PART ONLY TO UPPER-CASE (I.E. STOP AT 1ST BLANK)
C     ------------------------------------------------------------------
C
      DO 20 I=1,80
      IT=IBY(I)
      IF(IT.EQ.X20) RETURN
      IF(IT.LT.X61.OR.IT.GT.X7A) GO TO 20
      IBY(I)=IT-X20
   20 CONTINUE
      RETURN
      END
