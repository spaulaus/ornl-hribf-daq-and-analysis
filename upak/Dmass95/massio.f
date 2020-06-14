C$PROG MASSIO
C
      INTEGER*4 ERRI,ERRJ
C
      OPEN(UNIT       = 1,
     &     FILE       = 'mass_rmd.mas95',
     &     STATUS     = 'OLD')
C
      OPEN(UNIT       = 7,
     &     FILE       = 'massio.log',
     &     STATUS     = 'UNKNOWN',
     &     ACCESS     = 'APPEND')
C
      DO 20 I=1,39
      READ(1,10)IDUM
   10 FORMAT(A4)
   20 CONTINUE
C
  100 READ(1,105,END=500)IZ,IA,MASI,ITA,MASJ,ERRI,ITB,ERRJ
  105 FORMAT(9X,2I5,9X,I7,A1,I3,I5,A1,I3)
C
      IF(ITA.EQ.'#   ') GO TO 120
C
      WRITE(7,110)IZ,IA,MASI,ITA,MASJ,ERRI,ITB,ERRJ
  110 FORMAT(1H ,2I8,I7,A1,I3.3,'D0',I7,A1,I3.3'D0')
      GO TO 100
C
  120 WRITE(7,125)IZ,IA,MASI,ERRI
  125 FORMAT(1H ,2I8,I7,'.000D0',I7,'.000D0')
      GO TO 100
C
  500 CALL EXIT
      END
