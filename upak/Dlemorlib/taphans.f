C$PROG TAPHANS   - Sets ou tape control functions & calls TAPHAN
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE TAPHANS(KMI,NV,STAT)
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
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      CHARACTER*4  STAT
C
      INTEGER*4    KMI,NV
C
      INTEGER*4    KMD,IT3,IT4,C,IXO
C
      INTEGER*4    X20,X30,X49
      DATA         X20,X30,X49/Z'20',Z'30',Z'49'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      STAT='GOOD'                       !RESET ERROR FLAG
C
      KMD=KMI                           !SAVE REQUEST COMMAND
      CALL ILBYTE(IT3,KMD,2)            !GET 3RD BYTE
      CALL ILBYTE(IT4,KMD,3)            !GET 4TH BYTE
C
      CALL ISBYTE(Z'20',KMD,2)          !ZOT 3RD BYTE
      CALL ISBYTE(Z'20',KMD,3)          !ZOT 4TH BYTE
C
      C=0                               !SET CHAN# TO 0
      IF(IT3.NE.X49) GO TO 20           !TST FOR OUTPUT TAPE
C
      C=LUCI                            !OTHERWISE, INPUT TAPE
      IF(LINO.NE.'YES ') GO TO 230      !TST FOR OPEN
      GO TO 50
C
   20 IXO=1                             !OUTPUT STREAM # (DFLT=1)
      IF(IT4.EQ.X20) GO TO 30           !ASSUME 1 IF NOT SPECIFIED
      IXO=IT4-X30                       !GET STREAM#
      IF(IXO.LT.1) GO TO 220            !TST FOR LEGAL
      IF(IXO.GT.3) GO TO 220            !TST FOR LEGAL
C
   30 IF(LOUO(IXO).NE.'YES ') GO TO 230 !TST FOR OPEN
      C=LUCO(IXO)                       !LOGICAL UNIT#
C
   50 CALL TAPHAN(C,KMD,NV,STAT)        !GO OFF AND DO IT
      RETURN
C
C     ------------------------------------------------------------------
C     Error returns
C     ------------------------------------------------------------------
C
  220 WRITE(CMSSG,225)
  225 FORMAT('ILLEGAL REQUEST OR SYNTAX ERROR')
      GO TO 300
C
  230 WRITE(CMSSG,235)
  235 FORMAT('SPECIFIED FILE/DEVICE NOT ASSIGNED')
C
  300 CALL MESSLOG(LOGUT,LOGUP)
      STAT='BAD '
      RETURN
      END
