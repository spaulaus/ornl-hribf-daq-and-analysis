C$PROG DISTY     - Returns display type (lin or log)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/12/02
C     ******************************************************************
C
      SUBROUTINE DISTY(YMIN,JDSP,KDSP,KSCAL)
C
      CHARACTER*4           JDSP,KDSP
C
C     ------------------------------------------------------------------
C   
      IF(JDSP.EQ.'LIN ') THEN
                         KDSP='LIN '
                         KSCAL=1
                         ENDIF
C   
      IF(JDSP.EQ.'LOG ') THEN
                         KDSP='LOG '
                         KSCAL=2
                         IF(YMIN.LT.1.0) YMIN=1.0
                         ENDIF
C   
      RETURN
      END
