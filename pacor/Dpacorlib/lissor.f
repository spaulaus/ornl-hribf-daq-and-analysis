C$PROG LISSOR
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE LISSOR(KIND,IWD)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      INTEGER*4    IWD(20)
C
      CHARACTER*4  KIND,LISTYP
C
      SAVE
C
C     ************************************************************
C     LISTS SOURCE (OR GENERATED) LINE CONTAINED IN "IWD"
C
C     KIND   = 'SOR ' SAYS    SOURCE-LINE
C     KIND   = 'GEN ' SAYS GENERATED-LINE
C
C     ISORL  =   SOURCE-LINE COUNTER
C     IEXPL  = EXPANDED-LINE COUNTER
C  
C     LISTYP = 'NONE' SAYS LIST NONE
C     LISTYP = 'SOR ' SAYS LIST SOURCE-LINES ONLY
C     LISTYP = 'ALL ' SAYS LIST SOURCE & GENERATED LINES
C     ************************************************************
C
      IF(KIND.EQ.'SOR ') THEN
                         ISORL=ISORL+1
                         IEXPL=IEXPL+1
                         ENDIF
C
      IF(KIND.EQ.'GEN ') IEXPL=IEXPL+1
C
      IF(LISTYP.EQ.'NONE') RETURN
C
      IF(LISTYP.EQ.'ALL ') GO TO 100
C
      IF(KIND.EQ.'SOR ')   GO TO 100
C
      RETURN
C
  100 WRITE(LOGUP,110)ISORL,IEXPL,IWD
  110 FORMAT(1H ,2I6,2X,20A4)
      RETURN
      END
