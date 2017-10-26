C$PROG LEMOR     - List-tape Examine, Modify, Output, Revised
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
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
      COMMON/LM21/ NAMCMD(20)
      INTEGER*4    NAMCMD
C     ------------------------------------------------------------------
      INTEGER*4    LII,ICON,LCMD
C
      INTEGER*4    IERR,I
C
      CHARACTER*4  KMD,KMX,IDONE,KMI
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(2,1)),(KMI,IWD(1)),(IBY,IWD)
C
      BYTE         IBY(80)
C
      DATA         LII,ICON,LCMD/5,5,10/
C
      integer*4  msgerr
      character*80 ciwd
      equivalence  (ciwd,iwd)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL LEMONIT
C
      CALL COMSET
      msgerr = 0
C
      GO TO 100
C
C     ------------------------------------------------------------------
C     TEST "CONTINUE FLAGS", SWITCH INPUT FILE/DEV, ETC
C     ------------------------------------------------------------------
C
30    continue
40    continue
50    continue
C
  100 IF(LII.EQ.ICON) WRITE(LOGUT,110)  !ISSUE PROMPT IF CON:-MODE
  110 FORMAT('FILEOU->',$)
C
      MSGF='    '                      !CLR CTRL/C INTERRUPT FLG
C
C     ------------------------------------------------------------------
C     READ IN AND PROCESS THE NEXT COMMAND FROM LU "LII"
C     ------------------------------------------------------------------
C
*
*   Get command input from a message queue
*
      call frcvmsg(IWD,80,msgerr)
      if (msgerr .ne. 0) call exit(99)
C
      MSGF='    '                      !CLR CTRL/C INTERRUPT FLG
C
      call wait(100,1,0)               !Wait 0.1 seconds
C
C
      WRITE(CMSSG,120)IWD
      CALL MESSLOG(0,LOGUP)
  120 FORMAT(20A4,' - CON:')
C
      MSGF='    '                      !CLR CTRL/C INTERRUPT FLG
C
*
*    Remove leading spaces from the command string
*
      do i=1,80
        if (ciwd(i:i) .ne. ' ') go to 114
      enddo
114   if (i .ne. 1) ciwd = ciwd(i:)
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
  115 CONTINUE
C
      CALL CASEUP(IWD)
C
      IF(KMI.EQ.'    ') GO TO 100
      IF(KMI.EQ.'COM ') GO TO 100
      IF(KMI.EQ.'MSG ') GO TO 100
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IDONE='    '
      IERR=0
      CALL CALLER(IDONE,IERR)
      IF(IERR.NE.0)       GO TO 50
      IF(IDONE.EQ.'YES ') GO TO 100
C
  190 WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
C
 1500 CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
C$PROG CALLER    - Routine caller
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/13/99
C     ******************************************************************
C
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      IF(IDONE.NE.'YES ') CALL EQUATE(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL MODFIN(IERR)
      IF(IERR.NE.0)       RETURN
C   
      IF(IDONE.NE.'YES ') CALL LWDMOD
C
      IF(IDONE.NE.'YES ') CALL CMPGEN(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPSETUP(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPDISP(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPIOMAN(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPCOPY(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPMOC(IDONE,IERR)
C
C
      RETURN
      END
