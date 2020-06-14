C$PROG PUSPUL    - Routine to save & restore data buffers 1 or 2
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PUSPUL(IPP,JB)
C
C     ------------------------------------------------------------------
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
C     ------------------------------------------------------------------
      CHARACTER*4  IPP
C
      INTEGER*4 KBUF(33024),TMP(16512)
C
      EQUIVALENCE (KBUF(1),XBUF(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO SAVE AND RESTORE DATA BUFFER 1 OR 2 (ADAT OR BDAT)
C     ------------------------------------------------------------------
C
      IOF=IOFF(1,JB)
C
      IF(IPP.EQ.'PULL') GO TO 20
C
      DO 10 I=1,16512
      TMP(I)=KBUF(IOF+I)
   10 CONTINUE
      RETURN
C
   20 DO 30 I=1,16512
      KBUF(IOF+I)=TMP(I)
   30 CONTINUE
      RETURN
      END
