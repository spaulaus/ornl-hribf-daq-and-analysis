C$PROG EXPANMAN  - Expands DRR- and HIS-files
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 03/04/2004 - for gnu
C     ******************************************************************
C
      SUBROUTINE EXPANMAN(LUD,LUH,NUID,DIRF,IERR)
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
      INTEGER*4  DIRF(32),LUD,LUH,NUID,IERR
C
      INTEGER*4  FSTUFF(13),FSIZE,STATUS,NRECS,LTOT,RTOT,RADD,NR,IOS
C
      INTEGER*4  RECLVALU,N,STAT
C
      REAL*4     RECS
C     ------------------------------------------------------------------
      INTEGER*4  LUN
      DATA       LUN/2/
C
      INTEGER*4  BUFF(16384)
      DATA       BUFF/16384*0/
C
      INTEGER*4  MXNH
      PARAMETER (MXNH=6144)
      INTEGER*4  IDLIS(MXNH)
C     ------------------------------------------------------------------
      INTEGER*4  LDF,NHIS,NIDREC,NRN,JA,JB,I,J
C
      INTEGER*4  LENH,LASNHW,NXDAD,NBF
C     ------------------------------------------------------------------
C     HEADER QUANTITIES - DIMENSIONS, TYPES & EQUIVALENCES
C     ------------------------------------------------------------------
C
      INTEGER*4 IDIRF(32),JDIRF(32)
      INTEGER*2 IDIRH(64),JDIRH(64)
      REAL*4    XDIRF(32)
C
      EQUIVALENCE (IDIRH,IDIRF),(JDIRH,JDIRF),(XDIRF,IDIRF)
C
      INTEGER*2 ND,NHW,JHSP(4),LRAW(4),LENG(4),MINC(4),MAXC(4)
C
      INTEGER*4 IOF,LABX(3),LABY(3),CONS(4),ITIT(10)
C
      EQUIVALENCE (ND     ,IDIRH( 1)),  !# OF DIMENSIONS
     &            (NHW    ,IDIRH( 2)),  !# OF HALF-WDS/CHANNEL
     &            (JHSP(1),IDIRH( 3)),  !HIST PARM LIST
     &            (LRAW(1),IDIRH( 7)),  !LENGTH OF RAW    PARMS
     &            (MINC(1),IDIRH(15)),  !MIN CHANNEL# LIST
     &            (MAXC(1),IDIRH(19)),  !MAX CHANNEL# LIST
     &            (IOF    ,IDIRF(12)),  !DISK OFFSET (# HALF-WDS)
     &            (LABX(1),IDIRF(13)),  !X-PARM LABEL
     &            (LABY(1),IDIRF(16)),  !Y-PARM LABEL
     &            (CONS(1),XDIRF(19)),  !CALIBRATION CONSTANTS
     &            (ITIT(1),IDIRF(23))   !TITLE (40 BYTES)
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     STRUCTURE OF .DRR-FILE - FIRST RECORD (128 BYTES)
C     ------------------------------------------------------------------
C     JDIRF(1-3)   - 'HHIRFDIR0001'
C     JDIRF(4)     - # OF HISTOGRAMS ON .HIS-FILE
C     JDIRF(5)     - # OF HALF-WORDS ON .HIS-FILE
C     JDIRF(7-12)  - YR,MO,DA,HR,MN,SC (DATE, TIME OF CHIL RUN)
C     JDIRF(13-32) - TEXT (ENTERED IN CHIL VIA $TEX COMMAND)
C     ------------------------------------------------------------------
C     STRUCTURE OF .DRR-FILE - DIRECTORY ENTRY (128 BYTES)
C     ------------------------------------------------------------------
C     IDIRH(1)     - HISTOGRAM DIMENSIONALITY (MAX = 4)
C     IDIRH(2)     - NUMBER OF HALF-WORDS PER CHANNEL (1 OR 2)
C     IDIRH(3-6)   - HISTOGRAM PARM#'S (UP TO 4 PARAMETERS)
C     IDIRH(7-10)  - LENGTH OF RAW    PARAMETERS (PWR OF 2)
C     IDIRH(11-14) - LENGTH OF SCALED PARAMETERS (PWR OF 2)
C     IDIRH(15-18) - MIN CHANNEL# LIST
C     IDIRH(19-22) - MAX CHANNEL# LIST
C     IDIRF(12)    - DISK OFFSET IF HALF-WORDS (1ST WORD# MINUS 1)
C     IDIRF(13-15) - X-PARM LABEL
C     IDIRF(16-18) - Y-PARM LABEL
C     XDIRF(19-22) - CALIBRATION CONSTANTS (UP TO 4 FP NUMBERS)
C     IDIRF(23-32) - SUB-TITLE (40 BYTES) (ENTERED VIA $TIT CMD)
C     ------------------------------------------------------------------
C     STRUCTURE OF .DRR-FILE - HISTOGRAM ID-LIST (32 ID'S/RECORD)
C     ------------------------------------------------------------------
C     IDLST(1)     - ID NUMBER OF 1ST HISTOGRAM DEFINED
C     IDLST(2)     - ID NUMBER OF 2ND HISTOGRAM DEFINED
C                  -
C     ------------------------------------------------------------------
C     Read in first DRR record
C     ------------------------------------------------------------------
C
      IERR=0
C
      NRN=1
C
      READ(LUD,REC=NRN,IOSTAT=IOS)JDIRF
C
      IF(IOS.NE.0) THEN                     !Test for I/O error
      CALL IOFERR(IOS)
      GO TO 1000
      ENDIF
C
      NHIS=JDIRF(4)                         !# OF HIST ON FILE
C
      LDF=JDIRF(5)                          !FILE LENTGTH (HALF-WDS)
C
      NXDAD=LDF+1                           !Next disk addr to use
C
C     ------------------------------------------------------------------
C     Read in last entry to get last # half-words/channel - NHW
C     ------------------------------------------------------------------
C
      NRN=JDIRF(4)+1                        !REC# of last dir entry
C
      READ(LUD,REC=NRN,IOSTAT=IOS)IDIRF
C
      IF(IOS.NE.0) THEN                     !Test for I/O error
      CALL IOFERR(IOS)
      GO TO 1000
      ENDIF
C
      LASNHW=NHW                            !Last NHW
C
C     ------------------------------------------------------------------
C     Read in complete ID-list
C     ------------------------------------------------------------------
C
      NIDREC=(NHIS+31)/32                   !# HIS-ID RECORDS
      JB=0                                  !INIT IDLIS CNTR
      NRN=JDIRF(4)+1                        !1ST ID-RECORD MINUS 1
      DO 40 I=1,NIDREC                      !LOOP ON HIS-ID RECORDS
      NRN=NRN+1                             !INC REC CNTR
      JA=JB+1                               !1ST  WD TO LOAD
      JB=JA+31                              !LAST WD TO LOAD
C
      READ(LUD,REC=NRN,IOSTAT=IOS)(IDLIS(J),J=JA,JB) !READ 32 WDS
C
      IF(IOS.NE.0) THEN                     !Test for I/O error
                   CALL IOFERR(IOS)
                   GO TO 1000
                   ENDIF
   40 CONTINUE
C
C     ------------------------------------------------------------------
C     Check to see if requested NUID already exists
C     ------------------------------------------------------------------
C
      DO 50 I=1,NHIS
      IF(NUID.EQ.IDLIS(I)) GO TO 1020       !Test for already exists
   50 CONTINUE
C
C     ------------------------------------------------------------------
C     Output updated ID-list
C     ------------------------------------------------------------------
C
      NHIS=NHIS+1                           !Inc # of histograms
C
      IDLIS(NHIS)=NUID                      !Add NUID to ID-list
C
      IF(NHIS.GT.MXNH) GO TO 1030           !Test for DRR overflow
C
      NIDREC=(NHIS+31)/32                   !# HIS-ID RECORDS
      JB=0                                  !INIT IDLIS CNTR
      NRN=NHIS+1                            !1ST ID-RECORD MINUS 1
      DO 60 I=1,NIDREC                      !LOOP ON HIS-ID RECORDS
      NRN=NRN+1                             !INC REC CNTR
      JA=JB+1                               !1ST  WD TO LOAD
      JB=JA+31                              !LAST WD TO LOAD
C
      WRITE(LUD,REC=NRN,IOSTAT=IOS)(IDLIS(J),J=JA,JB) !Write 32 WDS
C
      IF(IOS.NE.0) THEN                     !Test for I/O error
                   CALL IOFERR(IOS)
                   GO TO 1050
                   ENDIF
   60 CONTINUE
C
C     ------------------------------------------------------------------
C     Update & output first DRR record
C     ------------------------------------------------------------------
C
      DO 70 I=1,32                          !Copy DIRF into IDIRF
      IDIRF(I)=DIRF(I)                      !to pick up "equivalences"
   70 CONTINUE
C
      LENH=1                                !INIT HIST LENGTH
      DO 80 I=1,ND                          !LOOP ON # DIMENSIONS
      LENH=LENH*(MAXC(I)-MINC(I)+1)         !PROD OF HIST DIMENSIONS
   80 CONTINUE
      LENH=NHW*LENH                         !HIST LENGTH (HALF-WDS)
C
      JDIRF(4)=NHIS                         !Update # histograms
C
      IF(NHIS.GT.1)     THEN                !Tsfr first one
      IF(NHW.NE.LASNHW) THEN                !Tsfr change in bits/chan
      NBF=(NXDAD+32767)/32768               !Round up NXDAD to multiple
      NXDAD=32768*NBF                       !of 32768 half-wds
      ENDIF
      ENDIF
C
      JDIRF(5)=NXDAD+LENH-1                 !His-file length in Hwds
      IF(2*(JDIRF(5)/2).NE.JDIRF(5)) THEN   !FORCE full-word length
      JDIRF(5)=JDIRF(5)+1
      ENDIF
C
      WRITE(LUD,REC=1,IOSTAT=IOS)JDIRF
C
      IF(IOS.NE.0) THEN                     !Test for I/O error
      CALL IOFERR(IOS)
      GO TO 1060
      ENDIF
C
C
C     ------------------------------------------------------------------
C     Output requested directory entry
C     ------------------------------------------------------------------
C
      DIRF(12)=NXDAD-1                      !Offset in half-wds
      IF(2*(DIRF(12)/2).NE.DIRF(12)) THEN   !Force full-word offset
      DIRF(12)=DIRF(12)+1
      ENDIF
C
      NRN=NHIS+1                            !Rec# for directory entry
C
      WRITE(LUD,REC=NRN,IOSTAT=IOS)DIRF     !Output requested DRR rec
C
      IF(IOS.NE.0) THEN                     !Test for I/O error
      CALL IOFERR(IOS)
      GO TO 1040
      ENDIF
C
C     ------------------------------------------------------------------
C     Expand the HIS-file
C     ------------------------------------------------------------------
C
      CALL EXPANHIS('OUT ',LUD,LUH,IERR)
C
      RETURN
C
C     ------------------------------------------------------------------
C     Return error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)NRN
 1005 FORMAT('I/O error reading record',I5,' of DRR file')
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)NUID
 1025 FORMAT('Requested ID',I8,' already exists in DRR file')
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)MXNH
 1035 FORMAT('Request for new histogram ID exceeds max of',I5)
      GO TO 2000
C
 1040 WRITE(CMSSG,1045)NRN
 1045 FORMAT('I/O error writing directory entry at rec# =',I4)
      GO TO 2000
C
 1050 WRITE(CMSSG,1055),NRN
 1055 FORMAT('I/O error writing ID-list at rec# =',I4)
      GO TO 2000
C
 1060 WRITE(CMSSG,1065)
 1065 FORMAT('I/O error writing  record-1 of DRR file')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
C
      IERR=1
C
      RETURN
      END
