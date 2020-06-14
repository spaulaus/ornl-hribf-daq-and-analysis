C$PROG DATMM     - Checks data against min & max (for display routines) 
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/12/02
C     ******************************************************************
C
      SUBROUTINE DATMM(DAT,ILO,IHI,YMINI,YMAXI,YMIN,YMAX)
C   
C     ------------------------------------------------------------------
      COMMON/SM11/ MDYHMS(5),KINFD,MINSY,MAXSY
      INTEGER*4    MDYHMS,         MINSY,MAXSY
      CHARACTER*4  KINFD,CMINSY,CMAXSY
      EQUIVALENCE (CMINSY,MINSY),(CMAXSY,MAXSY)
C     ------------------------------------------------------------------
C   
      DIMENSION    DAT(*)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(CMINSY.NE.'VAR ') THEN
                           YMIN=MINSY
                           GO TO 20
                           ENDIF
C
      YMIN=YMINI
      DO 10 I=ILO,IHI
      IF(DAT(I).LT.YMIN) YMIN=DAT(I)
   10 CONTINUE
C
   20 IF(CMAXSY.NE.'VAR ') THEN
                           YMAX=MAXSY
                           RETURN
                           ENDIF
C
      YMAX=YMAXI
      DO 30 I=ILO,IHI
      IF(DAT(I).GT.YMAX) YMAX=DAT(I)
   30 CONTINUE
      RETURN
      END
