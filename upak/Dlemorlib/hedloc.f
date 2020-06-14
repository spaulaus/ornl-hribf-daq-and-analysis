C$PROG HEDLOC    - Locates header-IDs, lists titles, etc
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE HEDLOC(C,ID,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/LM05/ IBUF(16384)
      INTEGER*4    IBUF
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      INTEGER*4    C,ID,IERR
C
      INTEGER*4    NEOF,NFF,NFFM,NFFT,IDNUM,JERR,NUMBY,I
C
      INTEGER*4    LISTF(16384)
C
      EQUIVALENCE (LISTF(1),IBUF(1))
C
      CHARACTER*4  STAT,IFLAG
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     LOCATE HEADER # ID - LIST "TITLES" ALONG THE WAY
C     ------------------------------------------------------------------
C
      IERR=0                            !RESET ERROR FLAG
      STAT='GOOD'                       !RESET STATUS
      NEOF=0                            !RESET EOF-COUNTER
      NFF=0                             !RESET FORWARD-FILE COUNTER
C
   10 IF(MSGF.NE.'    ') GO TO 100      !TST FOR STOP MESSAGE
      IF(NEOF.LT.2) GO TO 30            !TST FOR DOUBLE-EOF
C
      WRITE(CMSSG,20)NFF                !IF YES, SAY SO
      CALL MESSLOG(LOGUT,LOGUP)
   20 FORMAT('DOUBLE END-OF-FILE ENCOUNTERED AT FILE-MARK OFFSET',I6)
      CALL TAPHAN(C,'BF  ',1,STAT)      !BACK-UP ONE FILE-MARK
      IF(STAT.NE.'GOOD') GO TO 110
C
      NFFM=NFF-1
      WRITE(CMSSG,25)NFFM,NFF
   25 FORMAT('WITH LUCK - I AM BETWEEN FILE-MARK OFFSETS',I5,'  &',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 200                         !TAKE ERROR RETURN
C
   30 CALL READUM(C,LISTF,65536,NUMBY,STAT) !READ ONE RECORD
C
      IF(STAT.EQ.'GOOD') GO TO 40       !TST FOR GOOD STATUS
      IF(STAT.EQ.'EOF ') THEN           !TST FOR END-OF-FILE
                         NEOF=NEOF+1    !INC N-FILE COUNTER
                         NFF=NFF+1      !INC FORWARD-FILE COUNTER
                         CALL MT_CSE(C,JERR) !CLR SERIOUS EXCEPTION
C                        IF(JERR.NE.0)GOTO 80
                         GO TO 10
                         ENDIF
      GO TO 120                         !OTHERWISE, TAKE ERROR RETN
C
   40 NEOF=0                            !RESET EOF-COUNTER
C
      IF(NUMBY.NE.256) THEN             !TST FOR PRIMARY HEADER
      NFFT=NFF+1
      WRITE(CMSSG,45)NFFT
   45 FORMAT('HEADER RECORD NOT FOUND - EXECUTING FORWARD-FILE#',I7)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 55
                       ENDIF
C
      IFLAG='NO  '                      !RESET FOUND-FLAG
C
      IF(ISWAH.EQ.'YES ') CALL SWAPHED(LISTF,256)
C
      IDNUM=LISTF(33)
      IF(ISWAB.EQ.'YES ') CALL SWAPF(IDNUM,1,1)
C
      IF(IDNUM.EQ.ID) IFLAG='YES '      !TST FOR REQUESTED ID
C
      WRITE(CMSSG,50)(LISTF(I),I=9,23),IDNUM,IFLAG
      CALL MESSLOG(LOGUT,LOGUP)
   50 FORMAT(15A4,I12,2X,A4)
C
      IF(IFLAG.EQ.'YES ') GO TO 60      !RETURN IF DESIRED ID FOUND
      IF(ID.EQ.0)         GO TO 60      !TST FOR DTIT REQUEST
C
   55 CALL TAPHAN(C,'FF  ',1,STAT)      !OTHERWISE, SFIP-FILE
      IF(STAT.NE.'GOOD') GO TO 130      !TST FOR GOOD STATUS
      NEOF=NEOF+1                       !INC EOF-COUNTER
      NFF=NFF+1                         !INC FORWARD-FILE COUNTER
      GO TO 10                          !GO TRY AGAIN
C
   60 CALL TAPHAN(C,'BR  ',1,STAT)      !DTIT REQUEST - DO BACKSPACE
      IF(STAT.NE.'GOOD') GO TO 140
      RETURN
C
C     ------------------------------------------------------------------
C     ERROR RETURN - REQUESTED HEADER-ID NOT FOUND
C     ------------------------------------------------------------------
C
  100 WRITE(CMSSG,105)
  105 FORMAT('PROCESS INTERRUPTED VIA CTRL-C')
      GO TO 190
C
  110 WRITE(CMSSG,115)STAT
  115 FORMAT('HEDLOC ERROR DOING BACKWARD-FILE - STAT =',A4)
      GO TO 190
C
  120 WRITE(CMSSG,125)STAT
  125 FORMAT('HEDLOC ERROR READING - STAT =',A4)
      GO TO 190
C
  130 WRITE(CMSSG,135)STAT
  135 FORMAT('HEDLOC ERROR DOING FORWARD-FILE - STAT =',A4)
      GO TO 190
C
  140 WRITE(CMSSG,145)STAT
  145 FORMAT('HEDLOC ERROR DOING BACKWARD-RECORD - STAT =',A4)
      GO TO 190

  190 CALL MESSLOG(LOGUT,LOGUP)
C
  200 IERR=1
      RETURN
C
      END
