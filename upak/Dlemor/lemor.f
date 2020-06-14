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
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL LEMONIT
C
      CALL COMSET
C
CX    CALL LEMORMSG
C
      GO TO 100
C
C     ------------------------------------------------------------------
C     TEST "CONTINUE FLAGS", SWITCH INPUT FILE/DEV, ETC
C     ------------------------------------------------------------------
C
   30 LII=ICON
      WRITE(CMSSG,35)
      CALL MESSLOG(LOGUT,LOGUP)
   35 FORMAT('ERROR READING CMD-FILE OR CMD-FILE NOT ASSIGNED')
      GO TO 100
C
   40 WRITE(CMSSG,45)
      CALL MESSLOG(LOGUT,LOGUP)
   45 FORMAT('END OF COMMAND FILE')
C
C                                       !WE GET HERE VIA "ERROR"
   50 LII=ICON                          !SWITCH TO CON:-INPUT
      GO TO 100
C
   55 IF(LII.NE.ICON) GO TO 190         !TST FOR CON:-INPUT
      BACKSPACE(LCMD,ERR=100)           !BACKSPACE CMD-FILE
C                                       !IF ERROR, READ FROM CON:
C                                       !OTHERWISE,
   60 LII=LCMD                          !SWITCH TO CMD-FILE INPUT
      GO TO 100
C
   70 CALL NUINP(LCMD,IERR)             !DEFINE NEW COMMAND FILE
      IF(IERR.NE.0) GO TO 50            !SWITCH TO CON: IF ERROR
C
  100 IF(LII.EQ.ICON) WRITE(LOGUT,110)  !ISSUE PROMPT IF CON:-MODE
  110 FORMAT(' LEMOR->',$)
C
      MSGF='    '                      !CLR CTRL/C INTERRUPT FLG
C
C     ------------------------------------------------------------------
C     READ IN AND PROCESS THE NEXT COMMAND FROM LU "LII"
C     ------------------------------------------------------------------
C
      READ(LII,112,ERR=30,END=40)IWD    !READ NEXT COMMAND FROM LII
  112 FORMAT(20A4)
C
      IF(LII.NE.ICON)WRITE(LOGUT,114)(IWD(I),I=1,12),(NAMCMD(I),I=1,5)
  114 FORMAT(1H ,1X,12A4,' - FROM ',5A4)
C
      IF(LII.EQ.ICON) THEN
                      WRITE(CMSSG,120)IWD
                      CALL MESSLOG(0,LOGUP)
                      ENDIF
C
      IF(LII.NE.ICON) THEN
                      WRITE(CMSSG,122)IWD,(NAMCMD(I),I=1,6)
                      CALL MESSLOG(LOGUT,LOGUP)
                      ENDIF
C
  120 FORMAT(20A4,' - CON:')
  122 FORMAT(20A4,' - ',6A4)
C
      MSGF='    '                      !CLR CTRL/C INTERRUPT FLG
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
  115 CONTINUE
C
      CALL CASEUP(IWD)
C
      IF(KMI.EQ.'CCON') GO TO 50
      IF(KMI.EQ.'CLCM') GO TO 55
      IF(KMI.EQ.'CCMD') GO TO 60
      IF(KMI.EQ.'CMDF') GO TO 70
      IF(KMI.EQ.'CMD ') GO TO 70
C
      IF(KMI.EQ.'    ') GO TO 100
      IF(KMI.EQ.'COM ') GO TO 100
      IF(KMI.EQ.'MSG ') GO TO 100
C
CX    IF(IBY(1).EQ.'*')    GO TO 100
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMX.NE.'    ') GO TO 1000
C
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPER(LII,ICON)
                        GO TO 100
                        ENDIF
C
      IDONE='    '
      IERR=0
      CALL CALLER(IDONE,IERR)
      IF(IERR.NE.0)       GO TO 50
      IF(IDONE.EQ.'YES ') GO TO 100
C
  190 WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      GO TO 1500
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Syntax Error - Command Ignored')
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
