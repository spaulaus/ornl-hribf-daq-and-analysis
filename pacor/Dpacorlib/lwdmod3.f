C$PROG LWDMOD3
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE LWDMOD3(LWD3)
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C
      INTEGER*4 LWD3(3,40)
C
      CHARACTER*8  CTMP
C
      INTEGER*4    ITMP(2)
C
      EQUIVALENCE (CTMP,ITMP)
C
      INTEGER*4    BLANK
      character*4  cBLANK
      equivalence  (cBLANK, BLANK)
      DATA         cBLANK/'    '/
C
      SAVE
C
C     ************************************************************
C     REPLACE SYMBOLS IN 12-BYTE FIELDS WITH NUMERIC VALUES
C     ************************************************************
C
      DO 50 J=3,4
      IF(LWD3(2,J).NE.BLANK) GO TO 50
      DO 20 I=1,NSYM
      IF(LWD3(1,J).NE.ISYN(I)) GO TO 20
      WRITE(CTMP,10)ISYV(I)
   10 FORMAT(I8)
      LWD3(1,J)=ITMP(1)
      LWD3(2,J)=ITMP(2)
      GO TO 50
   20 CONTINUE
   50 CONTINUE
      RETURN
      END
