C$PROG KODVAL    - Returns input source "code values" M N O P Q R S
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      INTEGER*4 FUNCTION KODVAL(KWD)
C
C     ------------------------------------------------------------------
      INTEGER*4  KWD,BLANK
C
      INTEGER*4  KSOT(7),KMD,KODE,I
      character*4 cBLANK, cKSOT(7)
      equivalence (cblank,blank), (cksot, ksot)
C
      DATA      cKSOT/'M   ','N   ','O   ','P   ','Q   ','R   ','S   '/
C
      DATA      cBLANK/'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      KODE=BLANK
C
      DO 10 I=1,7
      IF(KWD.EQ.KSOT(I)) GO TO 20
   10 CONTINUE
      GO TO 50
C
   20 KODE=KWD
C
   50 KODVAL=KODE
C
      RETURN
      END
