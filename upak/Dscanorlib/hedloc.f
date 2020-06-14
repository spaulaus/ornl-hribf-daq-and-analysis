C$PROG HEDLOC    - Locates given header# & lists titles along the way
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/17/99
C     ******************************************************************
C
      SUBROUTINE HEDLOC(ID,IERR)
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
      COMMON/SC03/ LUC(10)                                                     
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM                      
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC06/ LIST(16384,2)                                               
      INTEGER*2    LIST
C     ------------------------------------------------------------------
      INTEGER*4   LISTF(16384)
C
      INTEGER*4   ID,IERR
C
      INTEGER*4   NEOF,IERRR,NUMBY,C,NBY,IDNUM,ISWAF,I
C
      CHARACTER*4 IFLAG
C
      EQUIVALENCE (LISTF(1),LIST(1,1))
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0                             !RESET ERROR FLAG
      NEOF=0                             !RESET EOF-COUNTER
      C=LUC(1)                           !MAG TAPE CHANNEL
      NBY=32768                          !# OF BYTES TO READ
      CALL MT_IOCLR(C,IERR)              !TERMINATE N-BUF'ed READ
C
   10 IF(MSGF.NE.'    ') GO TO 100       !TST FOR STOP MESSAGE
      IF(NEOF.LT.2)      GO TO 30        !TST FOR DOUBLE-EOF
C
      WRITE(CMSSG,20)                    !IF YES, SAY SO
      CALL MESSLOG(6,7)
   20 FORMAT(' DOUBLE END-OF-FILE ENCOUNTERED')
C
      GO TO 100                          !TAKE ERROR RETURN
C
   30 CALL MT_READ(C,LISTF,NBY,NUMBY,IERR)
C
      IF(IERR.EQ.0) GO TO 40             !TST FOR GOOD STATUS
      IF(IERR.EQ.999) THEN
              NEOF=NEOF+1 	         !IF EOF, INC EOF-CNTR
              GO TO 10  	         !AND CONTINUE
      ELSE
              CALL IOSERR(IERR)          !TST STATUS OF IO CALL
      ENDIF
      CALL TAPCLEAR(IERRR)               !CLEAR ERROR
      GO TO 100                          !OTHERWISE, TAKE ERROR RETN
C
   40 NEOF=0                             !RESET EOF-COUNTER
      IF(NUMBY.NE.256) GOTO 10           !TST FOR PRIMARY HEADER
      IFLAG='NO  '                       !RESET FOUND-FLAG
      IDNUM=LISTF(33)                    !GET HEADER ID
      IF(ISWAB.EQ.'YES ') IDNUM=ISWAF(IDNUM) !SWAP BYTES?
      IF(IDNUM.EQ.ID) IFLAG='YES '       !TST FOR REQUESTED ID
C
      WRITE(CMSSG,50)(LISTF(I),I=9,23),IDNUM,IFLAG
      CALL MESSLOG(6,7)
   50 FORMAT(15A4,I6,2X,A4)
C
      IF(IFLAG.EQ.'YES ') THEN           !RETURN IF DESIRED ID FOUND
          CALL TAPCLEAR(IERRR)
          RETURN
      ENDIF
C
      CALL MT_FF(C,1,IERR)
C
      IF(IERR.EQ.0.OR.IERR.EQ.999) THEN
                                   NEOF=NEOF+1     !INC EOF-COUNTER
                                   GO TO 10
                                   ENDIF

      CALL IOSERR(IERR)  			 !ERROR RETURN
C
C     ------------------------------------------------------------------
C     ERROR RETURN - REQUESTED HEADER-ID NOT FOUND
C     ------------------------------------------------------------------
C
  100 IERR=1
      RETURN
C
      END
