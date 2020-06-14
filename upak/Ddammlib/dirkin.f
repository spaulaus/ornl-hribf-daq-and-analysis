C$PROG DIRKIN    - Directory input routine (for his-files)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/12/02
C     ******************************************************************
C
      SUBROUTINE DIRKIN(LUD,NH,LBYTS,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      PARAMETER (MXNH=6144)
C
      COMMON/XAM3/ IDLST(MXNH),IHED(32),NUMHIS
C
      INTEGER*4 JDIRF(32)
C
      INTEGER*2 JDIRH(64),IDLSH(4096)
C
      EQUIVALENCE (IDLSH(1),IDLST(1)),(JDIRH(1),JDIRF(1))
      EQUIVALENCE (NHIS,NUMHIS)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
      READ(LUD,REC=1,IOSTAT=IOS)JDIRF       !READ 1ST BLK OF .DIR
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   GO TO 110
                   ENDIF
C
      NHIS=JDIRF(4)                         !# OF HIST ON FILE
      NH=NHIS                               !# OF HIST ON FILE
      LDF=JDIRF(5)                          !FILE LENTGTH (HALF-WDS)
      LBYTS=2*LDF                           !FILE LENGTH (BYTES)
C
      DO 30 I=1,32                          !LOAD 1ST RECORD INTO
      IHED(I)=JDIRF(I)                      !IHED FOR CALLER
   30 CONTINUE
C
      NIDREC=(NHIS+31)/32                   !# HIS-ID RECORDS
      IB=0                                  !INIT IDLSH 1/2WD CNTR
      IREC=NHIS+1                           !1ST ID-RECORD MINUS 1
C
      DO 40 I=1,NIDREC                      !LOOP ON HIS-ID RECORDS
      IREC=IREC+1                           !INC REC CNTR
      IA=IB+1                               !1ST  1/2WD ELE TO LOAD
      IB=IA+63                              !LAST 1/2WD ELE TO LOAD
      READ(LUD,REC=IREC,IOSTAT=IOS)(IDLSH(J),J=IA,IB)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   GO TO 110
                   ENDIF
C
   40 CONTINUE
      RETURN
C
  110 WRITE(LOGUT,115)
  115 FORMAT(1H ,'ERROR READING DRR-FILE')
      IERR=1
      RETURN
      END
