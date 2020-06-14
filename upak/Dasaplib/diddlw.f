C$PROG DIDDLW
      SUBROUTINE DIDDLW(WIDINC)
C
      COMMON/FFF/ HWID(16),QFN,QFLO,IFBGD,NTRY,MSN,NCH,KPK,I1,I2
      COMMON/HHH/ IGAUS,KNBAK,KNVAR,KVW
C
      DIMENSION WSAV(16),WLAST(16)
C
      SAVE
C
C     **************************************************************
C     ADJUSTS THE WIDTHS OF ALL PEAKS IN A SECTION BY MULTIPLYING
C     THE ORIGINAL VALUES BY (1+-N*WIDINC) UNTIL BEST QFN IS FOUND
C     **************************************************************
C
      IF(WIDINC.EQ.0.0) RETURN          !IGNORE CALL IF WIDINC=0
C
C     **************************************************************
C     FIND CHANGE DIRECTION - TRY INCREASE FIRST
C     **************************************************************
C
      CALL PKFIT                        !GET INITIAL QFN
C
      QFLO=QFN                          !SET BEST QFN (QFLO)
      WFAC=1.0+WIDINC                   !SET MULT FACTOR
C
      DO 10 I=1,KPK                     !LOOP ON ALL PEAKS
      WLAST(I)=HWID(I)                  !SAVE LAST WIDTHS
      WSAV(I)=HWID(I)                   !SAVE ORIGINAL WIDTHS
      HWID(I)=HWID(I)*WFAC              !MULT BY WFAC
   10 CONTINUE
C
      CALL PKFIT                        !FIT WITH INCREASED WIDTHS
C
      IF(QFN.LT.QFLO) GO TO 100         !TST FOR BETTER FIT
      IF(QFN.GT.QFLO) GO TO 200         !TST FOR WORSE  FIT
                      GO TO 300         !IF NO CHANGE, BACKUP & QUIT
C
C     **************************************************************
C     INCREASE WIDTH UNTIL FIT GETS WORSE OR LIMITS HIT
C     **************************************************************
C
  100 QFLO=QFN                          !RE-DEFINE QFLO
      DO 105 I=1,KPK                    !SAVE LAST WIDTHS
      WLAST(I)=HWID(I)
  105 CONTINUE
      WFAC=WFAC+WIDINC                  !INCREASE WIDTH
      IF(WFAC.GT.3.0) RETURN            !TEST AGAINST HI-LIMIT
C
      DO 110 I=1,KPK                    !LOOP ON ALL PEAKS
      HWID(I)=WFAC*WSAV(I)              !INCREASE WIDTH
  110 CONTINUE
C
      CALL PKFIT                        !DO THE FIT
C
      IF(QFN.LT.QFLO) GO TO 100         !IF BETTER, CONTINUE
      IF(QFN.GT.QFLO) GO TO 300         !IF WORSE,  BACKUP & QUIT
      RETURN                            !IF SAME,   RETURN
C
C     **************************************************************
C     DECREASE WIDTH UNTIL FIT GETS WORSE OR LIMITS HIT
C     **************************************************************
C
  200 DO 210 I=1,KPK                    !LOOP ON ALL PEAKS
      HWID(I)=WSAV(I)                   !RESTORE ORIGINAL VALUES
  210 CONTINUE
      QFN=QFLO                          !RESTORE ORIGINAL QFLO
      WFAC=WFAC-WIDINC                  !RESTORE ORIGINAL WFAC
C
  220 QFLO=QFN                          !RE-DEFINE QFLO
      DO 225 I=1,KPK                    !SAVE LAST WIDTHS
      WLAST(I)=HWID(I)
  225 CONTINUE
      WFAC=WFAC-WIDINC                  !DECREASE WIDTH
      IF(WFAC.LT.0.3) RETURN            !TEST AGAINST LO-LIMIT
C
      DO 230 I=1,KPK                    !LOOP ON ALL PEAKS
      HWID(I)=WFAC*WSAV(I)              !DECREASE
  230 CONTINUE
C
      CALL PKFIT                        !DO THE FIT
C
      IF(QFN.LT.QFLO) GO TO 220         !IF BETTER, CONTINUE
      IF(QFN.GT.QFLO) GO TO 300         !IF WORSE,  BACKUP & QUIT
      RETURN                            !IF SAME,   RETURN
C
C     **************************************************************
C     RESTORE PREVIOUS WIDTHS & RE-FIT
C     **************************************************************
C
  300 DO 310 I=1,KPK                    !LOOP ON ALL PEAKS
      HWID(I)=WLAST(I)                  !BACKUP ONE STEP
  310 CONTINUE
C
      CALL PKFIT                        !GET QFN
      QFLO=QFN                          !SET QFLO
C
      RETURN
      END
