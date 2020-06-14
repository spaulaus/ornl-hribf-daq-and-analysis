C$PROG HEDZOT    - Initialized spk header
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE HEDZOT(IHED)
C   
      INTEGER*4 IHED(128),BLANK
      character*4 cBLANK
      equivalence (cBLANK, BLANK)
C
      DATA     cBLANK/'    '/
C
      SAVE
C   
      DO 10 I=1,22
      IHED(I)=0
   10 CONTINUE
      DO 20 I=2,4
      IHED(I)= BLANK  
   20 CONTINUE
      DO 30 I=23,128
      IHED(I)=BLANK
   30 CONTINUE
      RETURN
      END
