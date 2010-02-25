C$PROG TIMEKEEP  - Gets initial time & returns elapled time in seconds
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE TIMEKEEP(KMD,NSEC)
C
      CHARACTER*4  KMD
C
      INTEGER*4    INITIME,NCALL
C
      DATA         INITIME,NCALL/0,0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      NSEC=0
C
      IF(NCALL.EQ.0)    THEN
                        CALL SECSENS70(INITIME)
                        NCALL=1
                        IF(KMD.EQ.'INIT') RETURN
                        ENDIF
C
      IF(KMD.EQ.'INIT') THEN
                        CALL SECSENS70(INITIME)
                        RETURN
                        ENDIF
C
      IF(KMD.EQ.'GET ') THEN
                        CALL SECSENS70(NEWTIME)
                        NSEC=NEWTIME-INITIME
                        RETURN
                        ENDIF
C
      RETURN
      END
