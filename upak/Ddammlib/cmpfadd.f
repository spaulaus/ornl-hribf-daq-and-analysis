C$PROG CMPFADD   - Command processor for hisadd feature of damm
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 08/20/05
C     ******************************************************************
C
      SUBROUTINE CMPFADD(IDONE,IERR)
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
      COMMON/HIS1/ NAMI,NAMO,LUHI,LUDI,LUHO,LUDO,INOP,OUOP
      CHARACTER*160 NAMI,NAMO
      INTEGER*4              LUHI,LUDI,LUHO,LUDO
      CHARACTER*4                                INOP,OUOP
C
      DATA         LUHI,LUDI,LUHO,LUDO/31,32,33,34/
C     ------------------------------------------------------------------
      COMMON/HIS2/ NRDI,NRHI,NRDO,NRHO
      INTEGER*4    NRDI,NRHI,NRDO,NRHO
C     ------------------------------------------------------------------
      CHARACTER*4  IDONE,KMD
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      INTEGER*4    STRLEN,IERR
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
      IERR=0
C
      IF(KMD.EQ.'FADF') GO TO 140
C
      IF(KMD.EQ.'FADI') GO TO 200
      IF(KMD.EQ.'FADO') GO TO 210
      IF(KMD.EQ.'FADN') GO TO 220
C
      IF(KMD.EQ.'CLIF') GO TO 250
      IF(KMD.EQ.'CLOF') GO TO 260
C
      IF(KMD.EQ.'FADD') GO TO 300
      IF(KMD.EQ.'FSUB') GO TO 400
      IF(KMD.EQ.'FADZ') GO TO 500 
C
      RETURN
C
C     ------------------------------------------------------------------
C     Display files which are open and 16-bit "data type"
C     ------------------------------------------------------------------
C
  140 IF(INOP.EQ.'YES ') THEN
      WRITE(LOGUT,145)NAMI(1:STRLEN(NAMI))
      WRITE(LOGUP,145)NAMI(1:STRLEN(NAMI))
      ENDIF
C
      IF(OUOP.EQ.'YES ') THEN
      WRITE(LOGUT,150)NAMO(1:STRLEN(NAMO))
      WRITE(LOGUP,150)NAMO(1:STRLEN(NAMO))
      ENDIF
C
  145 FORMAT('FAD Input  - ',A)
  150 FORMAT('FAD Output - ',A)
C
      GO TO 2000
C   
  200 CALL FADDOPEN('IN  ',IERR)
      GO TO 2000
C
  210 CALL FADDOPEN('OU  ',IERR)
      GO TO 2000
C
  220 CALL FADDOPEN('INIT',IERR)
      GO TO 2000
C
  250 CALL FADDCLOSE('IN  ')
      GO TO 2000
C
  260 CALL FADDCLOSE('OU  ')
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Add input to output
C     ------------------------------------------------------------------
C
  300 IF(INOP.NE.'YES ') GO TO 1000
      IF(OUOP.NE.'YES ') GO TO 1010
      WRITE(LOGUT,305)
      WRITE(LOGUP,305)
      WRITE(LOGUT,310)NAMI(1:STRLEN(NAMI))
      WRITE(LOGUP,310)NAMI(1:STRLEN(NAMI))
      WRITE(LOGUT,320)
      WRITE(LOGUP,320)
      WRITE(LOGUT,310)NAMO(1:STRLEN(NAMO))
      WRITE(LOGUP,310)NAMO(1:STRLEN(NAMO))
C
      CALL FADDUM(IERR)
C
      WRITE(LOGUT,325)
      WRITE(LOGUP,325)
C
  305 FORMAT('ADDING')
  310 FORMAT(A)
  320 FORMAT('to')
  325 FORMAT('DONE')
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Subtract input from output
C     ------------------------------------------------------------------
C
  400 IF(INOP.NE.'YES ') GO TO 1000
      IF(OUOP.NE.'YES ') GO TO 1010
      WRITE(LOGUT,405)
      WRITE(LOGUP,405)
      WRITE(LOGUT,410)NAMI(1:STRLEN(NAMI))
      WRITE(LOGUP,410)NAMI(1:STRLEN(NAMI))
      WRITE(LOGUT,420)
      WRITE(LOGUP,420)
      WRITE(LOGUT,410)NAMO(1:STRLEN(NAMO))
      WRITE(LOGUP,410)NAMO(1:STRLEN(NAMO))
C
      CALL FADSUB(IERR)
C
      WRITE(LOGUT,425)
      WRITE(LOGUP,425)
C
  405 FORMAT('SUBTRACTING')
  410 FORMAT(A)
  420 FORMAT('to')
  425 FORMAT('DONE')
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Zero the output file
C     ------------------------------------------------------------------
C
  500 IF(OUOP.NE.'YES ') GO TO 1010
      WRITE(LOGUT,505)
      WRITE(LOGUP,505)
  505 FORMAT('ZEROING')
C
      WRITE(LOGUT,310)NAMO(1:STRLEN(NAMO))
      WRITE(LOGUP,310)NAMO(1:STRLEN(NAMO))
C
      CALL FADDZOT(IERR)
C
      WRITE(LOGUT,510)
      WRITE(LOGUP,510)
  510 FORMAT('DONE')
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Return error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('FAD Input file is not open - command not executed')
      IERR=1
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('FAD Output file is not open - command not executed')
      IERR=1
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2000
C
 2000 IDONE='YES '
      RETURN
      END
