C$PROG HELPLIS   - Displays help for program SWAPO
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE HELPLIS
C
C     ------------------------------------------------------------------
      INTEGER*4    HELP(15,17)
      CHARACTER*60 CHELP(17)
      EQUIVALENCE (CHELP,HELP)
      DATA         CHELP/
     &'lon            - Turns logging to swapo.log ON              ',
     &'lof            - Turns logging to swapo.log OFF             ',
     &'                                                            ',
     &'test  name.spk - Tests name.spk for proper byte-order       ',
     &'swap  name.spk - Byte-swap name.spk                         ',
     &'                                                            ',
     &'test  name.his - Tests name.drr for proper byte-order       ',
     &'swap  name.his - Byte-swap name.drr & name.his              ',
     &'                                                            ',
     &'test  name.drr - Tests name.drr for proper byte-order       ',
     &'swap  name.drr - Byte-swap name.drr                         ',
     &'                                                            ',
     &'test  name.ldf - Tests name.ldf for proper byte-order       ',
     &'swap  name.ldf - Byte-swap name.ldf                         ',
     &'                                                            ',
     &'end            - Ends program                               ',
     &'h              - Displays this list again                   '/
C     ------------------------------------------------------------------
C
      WRITE(6,10)HELP
   10 FORMAT(1H ,15A4)
C
      RETURN
      END
