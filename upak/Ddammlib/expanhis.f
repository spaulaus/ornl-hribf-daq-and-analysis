C$PROG EXPANHIS  - Checks his-file for proper length & expands
C
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/31/2005 - for gnu
C     ******************************************************************
C
      SUBROUTINE EXPANHIS(MODE,LUD,LUH,IERR)
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
      COMMON/DML8/  NAMH(20)
      INTEGER*4     NAMH
C
      CHARACTER*80  CNAMH
      EQUIVALENCE  (CNAMH,NAMH)
C     ------------------------------------------------------------------
      CHARACTER*4  MODE,ANS
C
      INTEGER*4    LUD,LUH,IERR
C
      INTEGER*4    FSTUFF(13),FSIZE,STATUS,NRECS,LTOT,RTOT,RADD,NR,IOS
C
      INTEGER*4    RECLVALU,N,STAT
C
      REAL*4       RECS
C
      INTEGER*4    JDIRF(32),NHW,LUN
C
      EQUIVALENCE (NHW,JDIRF(5))
C
      DATA         LUN/2/
C
      INTEGER*4    BUFF(16384)
      DATA         BUFF/16384*0/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     Expand the HIS-file
C     ------------------------------------------------------------------
C
      IERR=0
C
      READ(LUD,REC=1,IOSTAT=IOS)JDIRF       !Read in 1st DRR record to 
C                                           !# of I*2 words on HIS-file
      IF(IOS.NE.0) THEN                     !Test for I/O error
      CALL IOFERR(IOS)
      GO TO 1000
      ENDIF
C
      CALL HISMAND('CLOSE',
     &              CNAMH,
     &              LUD,
     &              LUH,
     &             'RW  ',
     &              IERR)
C
      IF(IERR.NE.0) GO TO 1070
C
      OPEN(UNIT   =  LUN,                   !Open HIS-file for normal
     &     FILE   =  CNAMH,                 !direct access
     &     STATUS = 'OLD',
     &     ACCESS = 'DIRECT',
     &     RECL   =  RECLVALU(65536),
     &     IOSTAT =  STAT)
C
      IF(STAT.NE.0) THEN                    !Tst for error
                    CALL IOFERR(STAT)
                    GO TO 1080
                    ENDIF
C
      CALL FSTAT(LUN,FSTUFF,STATUS)         !Get current His-file size
      FSIZE=FSTUFF(8)/2                     !Size in I*2 words
C
      IF(FSIZE.GE.NHW) GO TO 200            !Tst for large enough
C
      IF(MODE.EQ.'OUT ') GO TO 100          !Tst for opening for output
C
      GO TO 1110                            !Error/reject if for input
C
C     ------------------------------------------------------------------
C     Expand HIS-file to match DRR-file if opening for output
C     ------------------------------------------------------------------
C
  100 WRITE(6,105)
  105 FORMAT(' Output his file too short - shall I expand it?')
      CALL YESNO(ANS)
      IF(ANS.NE.'YES ') THEN
      WRITE(6,110)
  110 FORMAT(' HIS-file not expanded')
      GO TO 200
      ENDIF
C
      RECS=FLOAT(FSIZE)/32768.0             !# of 65K records
      NRECS=RECS+0.5                        !# of 65K records
C
      LTOT=NHW                              !Expanded size in I*2 words
      RTOT=(LTOT+32767)/32768               !Expanded size in 65K recs
      RADD=RTOT-NRECS                       !# 65K records to add
C
      NR=NRECS                              !Init record number
      DO 120 N=1,RADD                       !Loop to add records
      NR=NR+1                               !Inc record number
      WRITE(LUN,REC=NR,IOSTAT=IOS)BUFF      !Write zero-filled buffer
C
      IF(IOS.NE.0) THEN                     !Tst for error
      CALL IOFERR(IOS)
      GO TO 1090
      ENDIF
C
  120 CONTINUE
C
  200 CLOSE(UNIT=LUN)                       !Close the temp file
C
      CALL HISMAND('OPEN',                  !Re-open HIS & DRR files
     &              CNAMH,
     &              LUD,                    !in the normal way
     &              LUH,
     &             'RW  ',
     &              IERR)
C
      IF(IERR.NE.0) GO TO 1100
C
      RETURN
C
C     ------------------------------------------------------------------
C     Return error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('I/O error reading record-1 of DRR file')
      GO TO 2000
C
 1070 WRITE(CMSSG,1075)IERR
 1075 FORMAT('Error closing HIS-file IERR =',I10)
C
 1080 WRITE(CMSSG,1085),CNAMH
 1085 FORMAT('Error trying to open ',A,' for expand')
      GO TO 2000
C
 1090 WRITE(CMSSG,1095)
 1095 FORMAT('Error expanding HIS-file')
      GO TO 2000
C
 1100 WRITE(CMSSG,1105),IERR
 1105 FORMAT('Error re-opening HIS-file for BUFO oper IERR =',I10)
      GO TO 2000
C
 1110 WRITE(CMSSG,1115)
 1115 FORMAT('Input HIS-file is shorter than implied by DRR-file')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
C
      IERR=1
C
      RETURN
      END
