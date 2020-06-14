C$PROG PACOR     - Preprocessor for data acquisition code PAC
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C
C     Modified for CAEN ADC & TDC support - 06/02/2001
C     ************************************************************
C
      COMMON/III/  LIN,LCM,LCI
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      CHARACTER*4  KLIS
C
      SAVE
C
C     *************************************************************
C     PRE-PROCESSOR FOR DATA ACQUISITION CODE - PAC
C     *************************************************************
C
C
      CALL PACNIT(KLIS)
C
      CALL PASS1
C
      CALL PASS2
C
      CALL CKCLOCID
C
      CALL MOTABLE
C
      IF(KLIS.EQ.'X   ') CALL TABLOG(9)
C
      IF(NERR.EQ.0)      CALL POBGEN(10,KLIS)
C
      IF(KLIS.EQ.'L   ') CALL POBLOD
C
      IF(NERR.LE.0) WRITE(6,95)
   95 FORMAT(1H ,'NO ERRORS')
C
      IF(NERR.GT.0) WRITE(6,100)NERR
  100 FORMAT(1H ,'NUMBER OF COMPILATION ERRORS =',I8)
C
      IF(NERR.EQ.0) STOP
C
                    STOP 1
C
      END
