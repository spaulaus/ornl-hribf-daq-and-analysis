C$PROG FILCOP    - Copies 80-byte records tape-to-disk & disk-to-tape
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE FILCOP(KMD,IERR)
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
      CHARACTER*4  KMD,CSTAT
C
      INTEGER*4    NAMF(20),IBUF(20),JBUF(20)
C
      INTEGER*4    IERR,NCOP,KFD,IHI,NBRED,I,LSNB,STAT
C
      CHARACTER*80 CNAMF,CIBUF,CJBUF
      BYTE BBUF(80)
C
      EQUIVALENCE (CNAMF,NAMF),(BBUF,IBUF),(CIBUF,IBUF),(CJBUF,JBUF)
C
      SAVE
C
C     ------------------------------------------------------------------

C
      IERR=0
      NCOP=0
      CLOSE(UNIT=8)
      CNAMF=' '
C
      CALL GETNAM(IWDRAW,5,80,NAMF,KFD,IERR)
C
      IF(IERR.NE.0) GO TO 700
C
      IF(KMD.EQ.'FCOP') GO TO 200
C
      IF(LINO.NE.'YES ')   GO TO 500
C
C     ------------------------------------------------------------------
C     READ TAPE  -  WRITE FILE
C     ------------------------------------------------------------------
C
      OPEN(UNIT       = 8,                  !OPEN FILE FOR OUTPUT
     &     FILE       = CNAMF,
     &     STATUS     = 'NEW',
     &     ACCESS     = 'SEQUENTIAL',
     &     IOSTAT     = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 700
                    ENDIF
C
  100 CALL READUM(LUCI,IBUF,80,NBRED,CSTAT)
C
      IF(CSTAT.NE.'GOOD') GO TO 700
C
      IHI=LSNB(IBUF,1,80)
      IF(IHI.LT.4) IHI=4
C
      WRITE(8,110)(BBUF(I),I=1,IHI)
  110 FORMAT(80A1)
      NCOP=NCOP+1
      GO TO 100
C
C     ------------------------------------------------------------------
C     READ FILE  -  WRITE TAPE
C     ------------------------------------------------------------------
C
  200 IF(LOUO(1).NE.'YES ') GO TO 510
C
      OPEN(UNIT   = 8,                      !OPEN FILE FOR INPUT
     &     FILE   = CNAMF,                  
     &     STATUS = 'OLD',
     &     ACCESS = 'SEQUENTIAL',
     &     IOSTAT = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 700
                    ENDIF
C
  220 READ(8,225,END=250,ERR=240)CIBUF
  225 FORMAT(A)
C
      CALL WRITUM(LUCO(1),IBUF,80,CSTAT)
      IF(CSTAT.NE.'GOOD') GO TO 250
      NCOP=NCOP+1
      GO TO 220
C
  240 WRITE(CMSSG,245)
  245 FORMAT('ERROR READING INPUT FILE')
      CALL MESSLOG(LOGUT,LOGUP)
C
  250 CALL FILMAR(LUCO(1),2,1,IERR)
      GO TO 700
C
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('INPUT TAPE NOT OPEN')
      GO TO 600
C
  510 WRITE(CMSSG,515)
  515 FORMAT('OUTPUT TAPE NOT OPEN')
C
  600 CALL MESSLOG(LOGUT,LOGUP)
C
  700 WRITE(CMSSG,705)NCOP
  705 FORMAT('NUMBER OF RECORDS COPIED =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      CLOSE(UNIT=8)
      RETURN
      END
