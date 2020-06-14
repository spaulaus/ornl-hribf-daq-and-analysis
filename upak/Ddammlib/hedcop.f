C$PROG HEDCOP    - Copies spk header (IHED to JHED)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE HEDCOP(IHED,JHED)
C   
      INTEGER*4 IHED(128),JHED(128)
C
      INTEGER*4 STARS
      character*4 cSTARS
      equivalence (cSTARS,STARS)
C
      DATA      cSTARS/'****'/
C
      SAVE
C   
      JHED(2)=IHED(2)
      JHED(3)=IHED(3)
      JHED(4)=IHED(4)
      CALL ISBYTE(Z'2A',JHED,15)
      JHED(13)=IHED(13)
      JHED(14)=IHED(14)
      DO 10 I=18,32
      JHED(I)=IHED(I)
   10 CONTINUE
      JHED(32)=STARS
      RETURN
      END
