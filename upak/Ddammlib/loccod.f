C$PROG LOCCOD    - Locates file-codes in list
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE LOCCOD(LIST,NF)
C
      COMMON/MD01/ KODFIL
      CHARACTER*4 CKODFIL
      EQUIVALENCE (CKODFIL,KODFIL)
C     ------------------------------------------------------------------
C
      INTEGER*4    FCODES(6),LIST(*)
      character*4 cfcodes(6)
      equivalence (cfcodes, fcodes)
C
      DATA cFCODES/'N   ','O   ','P   ','Q   ','R   ','S   '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CKODFIL='N   '
      DO 10 I=1,6
      IF(LIST(1).EQ.FCODES(I)) GO TO 20
   10 CONTINUE
      RETURN
C
   20 KODFIL=FCODES(I)
      NF=NF-1
      NDO=2*NF
      DO 30 I=1,NDO
      LIST(I)=LIST(I+2)
   30 CONTINUE
      RETURN
      END
