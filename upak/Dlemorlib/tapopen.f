C$PROG TAPOPEN   - Opens Input & Output tapes for LEMOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE TAPOPEN(KMD,KMM,IERR)
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
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
      COMMON/LM13/ NUMINT,NUMOUT(3),NOUT(3),MTIN(6),MTOUT(6,3),NOSTR
      INTEGER*4    NUMINT,NUMOUT,   NOUT,   MTIN,   MTOUT,     NOSTR
C     ------------------------------------------------------------------
      CHARACTER*4  KMD,KMM,KFD
C
      INTEGER*4    IERR,KL,I
C
      INTEGER*4    KOUTN
C
      CHARACTER*80 CNAMFD, CTNAME
C
      INTEGER*4    NAMFD(20), TNAME(20) 
C
      EQUIVALENCE (CNAMFD,NAMFD)
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     LIN    = LOGICAL UNIT FOR INPUT (MAY NOT BE USED)
C     LUCI   = CHANNEL #    FOR INPUT (IF ANY)
C     KFI    = 'TAPE' / 'FILE' IF INPUT IS FROM TAPE/FILE
C     LINO   = 'YES' IF INPUT IS OPEN - 'NO' OTHERWISE
C
C     LOU(J) = LOGICAL UNIT FOR OUTPUT STREAM-J (MAY NOT BE USED)
C     LUCO(J)= CHANNEL #    FOR OUTPUT STERAM-J (IF ANY)
C     KFO(J) = 'TAPE' / 'FILE' IF OUTPUT IS TO TAPE/FILE
C     LINO(J)= 'YES' IF OUTPUT-J IS OPEN - 'NO' OTHERWISE
C     ------------------------------------------------------------------
C
      IERR=0
C
      CTNAME=' '
C
      CALL MKTAPNAM(LWD(1,2),NAMFD)
C
      KFD='TAPE'
C
      IF(KMM.EQ.'OU  ') GO TO 50            !TST FOR INPUT OR OUTPUT
C
C     ------------------------------------------------------------------
C     OPEN INPUT
C     ------------------------------------------------------------------
C
      CALL CKCLI('DMOU')                    !CHECK & CLOSE INPUT
C
      DO 20 I=1,6                           !SAVE DEVICE NAME
      MTIN(I)=NAMFD(I)
   20 CONTINUE
C
      CALL MT_OPENRO(NAMFD,LUCI)
      IF(LUCI.LT.0)GO TO 110                !TST FOR ERROR
      KFI=KFD                               !SAVE FD-TYPE
      LINO='YES '                           !SET OPEN-FLAG
      RETURN
C
C     ------------------------------------------------------------------
C     OPEN OUTPUT - 1 OF 3
C     ------------------------------------------------------------------
C
   50 KL=KOUTN(KMD,3,IERR)                  !GET OUTPUT STREAM #
      IF(IERR.NE.0) GO TO 100               !TST FOR ERROR
C
      CALL CKCLO('DMOU',KL)                 !CHECK & CLOSE OUTPUT-KL
C
      DO 60 I=1,6                           !SAVE DEVICE NAME
      MTOUT(I,KL)=NAMFD(I)
   60 CONTINUE

      CALL MT_OPENRW(NAMFD,LUCO(KL))
      IF(LUCO(KL).LT.0)GO TO 120           !TST FOR ERROR
C
      KFO(KL)=KFD                           !SAVE FD-TYPE
      LOUO(KL)='YES '                       !SET OPEN-FLAG FOR OUTPUT-KL
      RETURN
C
C     ------------------------------------------------------------------
C     SET UP AND SEND ERROR MESSAGES
C     ------------------------------------------------------------------
C
  100 WRITE(CMSSG,105)
  105 FORMAT('ILLEGAL COMMAND - IGNORED')
      GO TO 200
C
  110 WRITE(CMSSG,115)LUCI
  115 FORMAT('ERROR ASSIGNING INPUT TAPE TO CHANNEL - STAT =',I8)
      GO TO 200
C
  120 WRITE(CMSSG,125)LUCO(KL)
  125 FORMAT('ERROR ASSIGNING OUTPUT TAPE TO CHANNEL - STAT =',I8)
C
  200 IERR=1
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
