C$PROG CLOSUM    - Closes his-, spk-, and ban-files
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 01/31/2005
C     ******************************************************************
C
      SUBROUTINE CLOSUM
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      INTEGER*4    NAM(20)
C
      CHARACTER*4  KSOR,KINF
C   
      EQUIVALENCE (KSOR,LWD(1,2))
C
      SAVE
C     ------------------------------------------------------------------
C
      IF(KSOR.EQ.'P   ') THEN
                         CLOSE(UNIT=18)
                         KFIL(18)='    '
                         RETURN
                         ENDIF
C
      IF(KSOR.EQ.'BAN ') THEN
                         CLOSE(UNIT=20)
                         KFIL(20)='    '
                         RETURN
                         ENDIF
C   
      CALL LUGET(KSOR,LUH,LUD,KINF,IERR)
      IF(IERR.NE.0) GO TO 100
C
      LU=LUD-1
      CLOSE(UNIT=LU)                         !CLOSE DATA-FILE
      CLOSE(UNIT=LUD)                        !CLOSE DIR-FILE
C
      IF(KINF.EQ.'HIS ') THEN                !TST FOR HIS-FILE OPEN
      CALL HISMAND('CLOS',NAM,LUD,LUC(LU),IDUM,IERR) !IF YES, DEASSIGN
                         ENDIF
C
      KFIL(LU)='    '                        !SET DATA-FILE CLOSED
      KFIL(LU+1)='    '                      !SET DIR- FILE CLOSED
C
      RETURN
C
  100 WRITE(CMSSG,105)
  105 FORMAT('ILLEGAL CLOSE-FILE SYNTAX - IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
