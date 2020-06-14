C$PROG BLANKWD   - Sets words in IWD (IA thru IB) to blank
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BLANKWD(IWD,IA,IB)
C
      CHARACTER*4 IWD(*)
C
      DO 20 I=IA,IB
      IWD(I)='    '
   20 CONTINUE
      RETURN
      END
