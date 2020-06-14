C$PROG HISIO     - General his-file I/O routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE HISIO(MODE,LUD,LUH,IHN,NDX,NCH,IHED,IDAT,IERR,MSER)
C
C     ------------------------------------------------------------------
C     Shared memory control information
C
      LOGICAL SHMFLG
      LOGICAL SHMUSE
      INTEGER SHMID
C
      COMMON /SharedMem/ SHMID(20), SHMFLG(20), SHMUSE
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      CHARACTER*4 MODE
C
      INTEGER*4 STAT,S
C
      INTEGER*4 MESS(10,6),MSER(10),LABLF(3)
C
      PARAMETER (MXNH=6144,MXHH=12288)
C
      INTEGER*4 IDLST(MXNH),IHED(32),NDX(4)
C
      INTEGER*2 IDLSH(MXHH),NBUF(256),IDAT(2)
C
      INTEGER*4 FWA,FWB,SWA,SWB,SECA,SECB,HWPB,BYPB
C
      CHARACTER CMESS(6)*40,CLABLF*12
C
      EQUIVALENCE (CMESS(1),MESS(1,1)),(CLABLF,LABLF(1))
C
C     ------------------------------------------------------------------
C     HEADER QUANTITIES - DIMENSIONS, TYPES & EQUIVALENCES
C
      INTEGER*4 IDIRF(32),JDIRF(32)
      INTEGER*2 IDIRH(64),JDIRH(64)
      DIMENSION XDIRF(32)
C
      EQUIVALENCE (IDIRH(1),IDIRF(1)),(XDIRF(1),IDIRF(1))
      EQUIVALENCE (JDIRH(1),JDIRF(1)),(IDLSH(1),IDLST(1))
C
      INTEGER*2 ND,NHW,JHSP(4),LRAW(4),LENG(4),MINC(4),MAXC(4)
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
C
      DATA CLABLF/'HHIRFDIR0001'/
C
      DATA CMESS/'REQUESTED ID DOES NOT EXIST            ',
     2           'INVALID CHANNEL#,S REQUESTED           ',
     3           'I/O ERROR READING IN DIRECTORY         ',
     4           'I/O ERROR READING IN DATA              ',
     5           'I/O ERROR WRITING OUT DATA             ',
     6           'ILLEGAL HISIO REQUEST MODE             '/
C
      DATA NCALL,LASNH,IOF,NHIS,ND,NHW/0,0,0,0,1,1/
      DATA HWPB,BYPB/256,512/
C
      SAVE
C
C     ------------------------------------------------------------------
C     HISIO RETURNS A CONTIGIOUS BLOCK OF THE HIST FILE IN IDAT
C         & RETURNS THE ASSOCIATED HEADER IN IHED
C     ------------------------------------------------------------------
C     DEFINITION OF ARGUMENT LIST
C
C     MODE = 'INIT' SAYS NEW FILE OPENED (READ 1ST BLK & IDLST)
C     MODE = 'DIR ' RETURNS 1ST "NCH" ENTRIES IN IDLST IN IDAT
C     MODE = 'READ' SAYS READ
C     MODE = 'RITE' SAYS WRITE
C
C     LUD  = LOGICAL UNIT # FROM WHICH DIRECTORY IS READ
C     LUH  = LOGICAL UNIT # FROM WHICH DATA IS READ
C     IHN  = THE "HISTOGRAM ID # TO BE PARTIALLY OR WHOLLY XFERED
C
C     NDX  IS AN ARRAY WHICH CONTAINS THE INDICES IF THE FIRST WORD
C          TO BE XFERRED (INDICES ARE REFERED TO HISTOGRAM # IHN)
C
C     NCH  = THE # OF CHANNELS (MAY BE INT*2 OR INT*4) TO BE XFERED
C     NCH  = 0 SAYS RETURN DIRECTORY BLOCK (IHED) ONLY
C     ------------------------------------------------------------------
C     IF HIST DIMENSION = N, NDX(N+1)  THRU NDX(4)  ARE IGNORED
C     IF USED NDX(I)  MUST BE .GT. MINC(I) AND .LE. MAXC(I)+1
C     ------------------------------------------------------------------
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
C     OTHER DEFINITIONS
C     HWPB = 1/2WDS/BLK  FOR CONTIG  FILE (128/256 FOR P.E./VAX)
C     BYPB =  BYTES/BLK  FOR CONTIG  FILE (256/512 FOR P.E./VAX)
C
C     ND   = DIMENSIONALITY OF HISTOGRAM (# PARMS AND # LENGTHS)
C     NHW  = # HALF-WORDS/CHANNEL
C     LENH = # OF HALF-WORDS IN THIS HISTOGRAM
C     IOF  = DISK "OFFSET" OF 1ST WORD OF THIS HIST (1ST WORD # -1)
C            (IN HALF-WORDS)
C     LDF  = LENGTH OF DISK FILE "USER.HIS" IN HALF-WORDS
C     NHIS = TOTAL # OF HISTOGRAMS ON FILE "USER.HIS"
C
C     LENG(I),I=1,4 = HISTOGRAM "LENGTHS" IN CHANNELS
C     ------------------------------------------------------------------
C     RECORD NUMBERS - SECTOR NUMBERS - BLOCK NUMBERS
C
C     RECORD NUMBERS FOR THE DRR-FILE ARE ON BASIS OF 1ST RECORD# = 1
C     SECTOR NUMBERS FOR THE HIS-FILE ARE ON BASIS OF 1ST SECTOR# = 0
C     BLOCK  NUMBERS FOR THE HIS-FILE ARE ON BASIS OF 1ST BLOCK#  = 1
C
C     ROUTINES BUFI & BUFO ARE CALLED WITH RECORD#'S OR BLOCK#'S
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(NCALL.EQ.0)     GO TO 10
      IF(MODE.EQ.'INIT') GO TO 10
      IF(MODE.EQ.'DIR ') GO TO 100
      IF(MODE.EQ.'READ') GO TO 200
      IF(MODE.EQ.'RITE') GO TO 200
      GO TO 560
C
C     ------------------------------------------------------------------
C     READ IN 1ST BLOCK OF .DRR-FILE ON 1ST CALL (OR IF MODE='INIT')
C     ALSO READ IN HIST ID-LIST
C     ------------------------------------------------------------------
C
   10 NCALL=1                               !RESET "FIRST CALL" FLAG
      LASNH=0                               !SET LAST HIS-ID TO 0
      READ(LUD,REC=1,IOSTAT=S)JDIRH         !READ 1ST DRR-RECORD
      IF(S.NE.0) THEN                       !TST STATUS
                 CALL IOFERR(S)
                 GO TO 530
                 ENDIF
C
      DO 20 I=1,3                           !TST FILE-IDENTIFICATION
      IF(JDIRF(I).NE.LABLF(I)) GO TO 530
   20 CONTINUE
C
      NHIS=JDIRF(4)                         !# OF HIST ON FILE
C
      IF(NHIS.GT.MXNH) THEN                 !TST FOR GT MAX ACCOMODATED
      WRITE(CMSSG,25)NHIS,MXNH
   25 FORMAT('# HIS OF FILE =',I6,'  HISIO LIMIT =',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      NHIS=MXNH
                       ENDIF
C
      LDF=JDIRF(5)                          !FILE LENTGTH (HALF-WDS)
C
      DO 30 I=1,32                          !LOAD 1ST RECORD INTO
      IHED(I)=JDIRF(I)                      !IHED FOR CALLER
   30 CONTINUE
C
      NIDREC=(NHIS+31)/32                   !# HIS-ID RECORDS
      JB=0                                  !INIT IDLSH 1/2WD CNTR
      NRN=JDIRF(4)+1                        !1ST ID-RECORD MINUS 1
      DO 40 I=1,NIDREC                      !LOOP ON HIS-ID RECORDS
      NRN=NRN+1                             !INC REC CNTR
      JA=JB+1                               !1ST  1/2WD ELE TO LOAD
      JB=JA+63                              !LAST 1/2WD ELE TO LOAD
C
      READ(LUD,REC=NRN,IOSTAT=S)(IDLSH(J),J=JA,JB) !READ 64 1/2-WDS
      IF(S.NE.0) THEN                       !TST STATUS
                 CALL IOFERR(S)
                 GO TO 530
                 ENDIF
C
   40 CONTINUE
      IF(MODE.EQ.'DIR ') GO TO 100          !CONT IF MODE.NE.'INIT'
      IF(MODE.EQ.'READ') GO TO 200
      RETURN                                !OTHERWISE, RETURN
C
C     ------------------------------------------------------------------
C     RETURN FIRST "NCH ENTRIES" OF IDLST IN IDAT
C     ------------------------------------------------------------------
C
  100 NDO=NCH                               !# HIS-IDS TO RETURN
      IF(NDO.GT.NHIS) NDO=NHIS              !TST FOR .GT. NHIS
      IDAT(1)=NDO                           !NID LO-ORDER (ACTUAL #)
      IDAT(2)=0                             !NID HI-ORDER
      NDO=2*NDO                             !SET TO LOOP ON 1/2WDS
      DO 110 I=1,NDO                        !LOAD INTO IDAT
      IDAT(I+2)=IDLSH(I)
  110 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     RETURN THE SPECIFIED PORTION OF A HISTOGRAM
C     ------------------------------------------------------------------
C
  200 DO 220 I=1,NHIS                       !LOOK FOR REQUESTED ID
      IF(IHN.EQ.IDLST(I)) GO TO 240         !IN IDLST
  220 CONTINUE
      GO TO 510                             !ERROR IF NOT FOUND
  240 NH=I                                  !DIRECTORY ORDINAL #
      NR=NH+1
      IF(LASNH.EQ.NH) GO TO 300             !TST FOR SAME AS LAST
      READ(LUD,REC=NR,IOSTAT=S)IDIRH        !OTHERWISE, READ ENTRY
      IF(S.NE.0) THEN
                 CALL IOFERR(S)
                 GO TO 530
                 ENDIF
C
      LASNH=NH                              !RESET LAST DRR-ENTRY #
C
      LENH=1                                !INIT HIST LENGTH
      DO 260 I=1,ND                         !LOOP ON # DIMENSIONS
      LENG(I)=MAXC(I)-MINC(I)+1             !SAVE LENGTH
      LENH=LENH*LENG(I)                     !PROD OF HIST DIMENSIONS
  260 CONTINUE
      LENH=NHW*LENH                         !HIST LENGTH (HALF-WDS)
C
  300 DO 320 I=1,32                         !LOAD DRR-ENTRY
      IHED(I)=IDIRF(I)                      !INTO IHED EVERY TIME
  320 CONTINUE
C
      IF(NCH.LE.0) RETURN
C
C     ------------------------------------------------------------------
C     COMPUTE FIRST HALF-WORD # ON DISK TO BE XFERRED - NWN
C     ------------------------------------------------------------------
C
      ISUM=IOF+1                            !1ST HALF-WD# FOR THIS HIS
      DO 370 I=1,ND                         !LOOP ON # DIMENSIONS
      IF(NDX(I).LE.0)       GO TO 520       !TST FOR LEGAL CHAN#
      IF(NDX(I).GT.LENG(I)) GO TO 520       !TST FOR LEGAL CHAN#
      IAD=NHW*(NDX(I)-1)                    !AMOUNT TO SKIP IN UNITS
C                                           !OF HALF-WORDS  IF I=1
C                                           !OF 1-D REGIONS IF I=2
C                                           !OF 2-D REGIONS IF I=3
C                                           !OF 3-D REGIONS IF I=4
C
      JDO=I-1                               !# OF LENGTH FACTORS
      IF(JDO.LT.1) GO TO 360
      DO 350 J=1,JDO                        !MULT BY PROD OF DIMS
      IAD=IAD*LENG(J)
  350 CONTINUE
  360 ISUM=ISUM+IAD                         !ACCUMULATE TOTAL OFFSET
  370 CONTINUE
C
      IF(MODE.EQ.'RITE') GO TO 410          !TST FOR WRITE REQUEST
C
C     ------------------------------------------------------------------
C     PROCESS - READ REQUEST
C     ------------------------------------------------------------------
C
      NWN=ISUM                              !1ST HALF-WD# TO XFER
C
C     ------------------------------------------------------------------
C     Reading from shared memory instead of File
C     ------------------------------------------------------------------
C
      IF(SHMFLG(LUD)) THEN                  !Flag set by HISMAN
      NHWD = NHW*NCH                        !# of 2 byte words to read
      CALL MEM_READ_HW(IDAT(1),NWN, NHWD)   !Routine to fetch data
      RETURN
      ENDIF
C
C     ------------------------------------------------------------------
C     Reading from File
C     ------------------------------------------------------------------
C
      ISEC=(NWN-1)/HWPB                     !SEC# WHERE IT IS
C
      IF(MOD((NWN-1),HWPB).EQ.0) GO TO 390  !TST FOR NWN AT BEGINNING
C                                           !OF A SECTOR
C
      NLO=NWN-HWPB*ISEC                     !OTHERWISE, NLO IS WORD#
C                                           !IN NBUF WHICH CORRESPONDS
C                                           !TO NWN
C
C     ------------------------------------------------------------------
C     READ SECTOR ISEC INTO NBUF AND LOAD APPROPRIATE PART INTO IDAT
C     ------------------------------------------------------------------
C
      CALL BUFI(LUH,NBUF,ISEC+1,BYPB,STAT)  !READ FRAGGED SECT
      IF(STAT.NE.0) GO TO 540
      NHWD=NHW*NCH                          !TOTAL # HALF-WDS TO XFER
      N=0                                   !INIT IDAT INDEX
      DO 380 I=NLO,HWPB                     !LOAD PARTIAL SECTOR
      N=N+1                                 !INC IDAT INDEX
      IDAT(N)=NBUF(I)                       !LOAD IDAT FROM NBUF
      IF(N.GE.NHWD) RETURN                  !TST FOR DONE
  380 CONTINUE
      NBYTES=2*(NHWD-N)                     !REMAINING BYTES TO READ
      N=N+1                                 !NEXT HALF-WD TO LOAD
      ISEC=ISEC+1                           !STARTING SECTOR #
      GO TO 400
C
  390 N=1                                   !1ST HALF-WD# TO LOAD
      NBYTES=2*NCH*NHW                      !# BYTES TO READ
C
C     ------------------------------------------------------------------
C     READ ALL (OR REMAINDER) OF DATA DIRECTLY INTO CALLERS BUFFER
C     ------------------------------------------------------------------
C
  400 CALL BUFI(LUH,IDAT(N),ISEC+1,NBYTES,STAT)
      IF(STAT.NE.0) GO TO 540
      RETURN
C
C     ------------------------------------------------------------------
C     PROCESS - WRITE REQUEST
C     ------------------------------------------------------------------
C
  410 FWA=ISUM                              !FIRST FILE-WHD TO XFER
      NHWD=NHW*NCH                          !# HWDS TO XFER
      FWB=FWA+NHWD-1                        !LAST  FILE-HWD TO XFER
      SECA=(FWA-1)/HWPB                     !FIRST SECT INVOLVED
      SECB=(FWB-1)/HWPB                     !LAST  SECT INVOLVED
      SWA=FWA-HWPB*SECA                     !FIRST WD IN FIRST SECT
      SWB=FWB-HWPB*SECB                     !LAST  WD IN LAST  SECT
      IDX=0                                 !INIT IDAT-INDEX
C
      IF(SWA.EQ.1) GO TO 430                !FIRST SECT FRAGGED??
C
      CALL BUFI(LUH,NBUF,SECA+1,BYPB,STAT)  !READ IN FIRST SECT
      IF(STAT.NE.0) GO TO 540
C
      IA=SWA                                !FIRST HWD TO REPLACE
      IB=IA+NHWD-1                          !LAST  HWD TO REPLACE
      IF(IB.GT.HWPB) IB=HWPB                !LIMIT IT TO HWPB
C
      DO 420 I=IA,IB                        !LOOP TO REPLACE
      IDX=IDX+1                             !INC IDAT INDEX
      NBUF(I)=IDAT(IDX)
  420 CONTINUE
C
      CALL BUFO(LUH,NBUF,SECA+1,BYPB,STAT)  !WRITE IT BACK OUT
      IF(STAT.NE.0) GO TO 550
C
      SECA=SECA+1                           !INC FIRST SECT#
      IF(SECA.GT.SECB) RETURN               !TST FOR DONE
C
  430 NFS=(NHWD-IDX)/HWPB                   !# FULL SECTS TO WRITE
      IF(NFS.LE.0) GO TO 440                !TST FOR .LT.1
      NBYT=2*HWPB*NFS                       !# BYTES TO WRITE
      IDX=IDX+1                             !INC IDAT INDEX
C
      CALL BUFO(LUH,IDAT(IDX),SECA+1,NBYT,STAT) !WRITE IT OUT
      IF(STAT.NE.0) GO TO 550
C
      IDX=IDX+HWPB*NFS-1                    !ADJUST IDAT-INDEX
      IF(IDX.GE.NHWD) RETURN                !TST FOR DONE
C
  440 CALL BUFI(LUH,NBUF,SECB+1,BYPB,STAT)  !READ IN LAST SECT
      IF(STAT.NE.0) GO TO 540
C
      IA=1                                  !FIRST HWD TO REPLACE
      IB=SWB                                !LAST  HWD TO REPLACE
C
      DO 450 I=IA,IB                        !LOOP TO REPLACE
      IDX=IDX+1                             !INC IDAT-INDEX
      NBUF(I)=IDAT(IDX)
  450 CONTINUE
C
      CALL BUFO(LUH,NBUF,SECB+1,BYPB,STAT)  !WRITE IT OUT
      IF(STAT.NE.0) GO TO 550
      RETURN
C
C     ------------------------------------------------------------------
C     SET ERROR CODE AND LOAD UP MESSAGE
C     ------------------------------------------------------------------
C
  510 IERR=1
      GO TO 600
  520 IERR=2
      GO TO 600
  530 IERR=3
      GO TO 600
  540 IERR=4
      GO TO 600
  550 IERR=5
      GO TO 600
  560 IERR=6
C
  600 DO 610 I=1,10
      MSER(I)=MESS(I,IERR)
  610 CONTINUE
      RETURN
      END
