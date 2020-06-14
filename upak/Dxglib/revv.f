C$PROG REVV      - Does a reverse-video
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE REVV
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/XLJJ/ IRED(40),IGRE(40),IBLU(40),KOLRSET
      INTEGER*4    IRED,    IGRE,    IBLU
      CHARACTER*4                             KOLRSET
C     ------------------------------------------------------------------
C
      INTEGER*4    I
C
      SAVE
C
      DO 10 I=1,40
      IRED(I)=65535-IRED(I)
      IGRE(I)=65535-IGRE(I)
      IBLU(I)=65535-IBLU(I)
   10 CONTINUE
      RETURN
      END
