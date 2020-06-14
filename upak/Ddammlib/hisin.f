C$PROG HISIN     - his-file input routine
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE HISIN(LUD,LUH,NH,NDX,NCH,IDAT,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DIR/  KLOC(6),JHSP(4),LENG(4),ND,NHW,           !/DIR
     &             LENH,LENT,IOF,LDF,NHIS,LEND(4),           !/DIR
     &             LENS(4),MINC(4),MAXC(4),CONS(4),ITEX(20), !/DIR
     &             ITIT(10),LABX(3),LABY(3),MSER(10),KFILT   !/DIR 
C     ------------------------------------------------------------------
      INTEGER*4 NDX(4),MDX(4)
C
      DIMENSION IDIRF(12),JDIRF(32),XDIRF(32)
C
      INTEGER*2 IDIRH(24),JDIRH(64),IDAT(1)
C
      EQUIVALENCE (IDIRH(1),JDIRF(1)),(IDIRF(1),JDIRF(1)),
     &            (JDIRH(1),JDIRF(1)),(XDIRF(1),JDIRF(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C     THIS ROUTINE RETURNS A CONTIGIOUS BLOCK OF THE HIST FILE IN IDAT
C     AND ASSOCIATED INFORMATION IN COMMON/DIR/
C     1-D DATA IS ALWAYS RETURNED "PROPERLY POSITIONED"
C     ------------------------------------------------------------------
C     DEFINITIONS - ARGUMENT LIST
C
C     LUD = LOGICAL UNIT # FROM WHICH DIRECTORY IS READ
C     LUH = LOGICAL UNIT # FROM WHICH DATA IS READ
C     NH  = THE "HISTOGRAM NUMBER" TO BE PARTIALLY OR WHOLLY XFERRED
C
C     NDX  IS AN ARRAY WHICH CONTAINS THE INDICES OF THE FIRST WORD
C          TO BE XFERRED (INDICES ARE REFERRED TO HISTOGRAM # NH)
C     IF HIST DIMENSION = N, NDX(N+1)  THRU NDX(4)  ARE IGNORED
C     IF USED, NDX(I) MUST BE .GT.0 & .LE. ASSOCIATED HIST LENGTH
C
C     NCH = THE # OF CHANNELS (MAY BE INT*2 OR INT*4) TO BE XFERRED
C     ------------------------------------------------------------------
C     DEFINITIONS - ERROR CODES
C
C     IERR = 0 SAYS EVERYTHING IS OK
C     IERR = 1 SAYS INVALID VALUE OF NH (DOESN'T EXIST IN DIRECTORY)
C     IERR = 2 SAYS INVALID VALUE OF AN INDEX (ELEMENT OF NDX)
C     IERR = 3 SAYS I/O ERROR READING IN DIRECTORY
C     IERR = 4 SAYS I/O ERROR READING IN DATA
C     ------------------------------------------------------------------
C     DEFINITIONS - COMMOM/DIR/
C
C     KLOC(I),I=1,6  = YR,MO,DAY,HR,MIN,SEC
C     JHSP(I),I=1,4  = HISTOGRAM PARAMETERS
C     LENG(I),I=1,4  = HIST "LENGTHS" - (MIXC(I)-MINC(I)+1) FOR 2-D
C     LENG(I),I=1,4  - LENS(1),0,0,0                        FOR 1-D
C     LEND(I),I=1,4  = RAW   -DATA "LENGTHS" IN CHANNELS
C     LENS(I),I=1,4  = SCALED-DATA "LENGTHS" IN CHANNELS
C     MINC(I),I=1,4  = MIN CHANNEL# LIST
C     MAXC(I),I=1,4  = MAX CHANNEL# LIST
C     CONS(I),I=1,4  = CAL CONSTANTS
C     ITEX(I),I=1,20 = TEXT  FROM $TEX CHIL-ENTRY
C     ITIT(I),I=1,10 = TITLE FROM $TIT CHIL-ENTRY
C     LABX(I),I=1,3  = X-PARM LABEL IF ANY
C     LABY(I),I=1,3  = Y-PARM LABEL IF ANY
C     ND   = DIMENSIONALITY OF HISTOGRAM (# PARMS AND # LENGTHS)
C     NHW  = # HALF-WORDS/CHANNEL
C     LENH = # OF HALF-WORDS IN THIS HISTOGRAM
C     LENT = (NO LONGER SUPPLIED - I CAN'T SEE ANY USE FOR IT)
C     IOF  = DISK "OFFSET" OF 1ST WD OF THIS HISTOGRAM (1ST WD # -1)
C            (IN HALF-WORDS)
C     LDF  = LENGTH OF DISK FILE "USER.HIS" IN HALF-WORDS
C     NHIS = TOTAL # OF HISTOGRAMS ON FILE "USER.HIS"
C     ------------------------------------------------------------------
C
      IERR=0                            !RESET ERROR CODE
C
C     ------------------------------------------------------------------
C     USE ROUTINE HISIO TO DO ACTUAL READS, ETC
C
C     THIS FORM OF HISIN DOES AN "INIT" FOR EVERY CALL
C     ------------------------------------------------------------------
C
      CALL HISIO('INIT',LUD,LUH,NH,NDX,NCH,JDIRF,IDAT,IERR,MSER)
      IF(IERR.NE.0) RETURN
C
      NHIS=JDIRF(4)
      LDF =JDIRF(5)
      DO 305 I=1,6
      KLOC(I)=JDIRF(I+6)
  305 CONTINUE
      DO 310 I=1,20
      ITEX(I)=JDIRF(I+12)
  310 CONTINUE
      IF(NH.LE.0) RETURN                !RETURN IF INIT REQUEST
C
      NC=0
      CALL HISIO('READ',LUD,LUH,NH,NDX,NC,JDIRF,IDAT,IERR,MSER)
      IF(IERR.NE.0) RETURN
C
      ND =JDIRH(1)
      NHW=JDIRH(2)
C
      LENH=1
      DO 330 I=1,4
      JHSP(I)=JDIRH(I+2)
      LEND(I)=JDIRH(I+6)
      LENS(I)=JDIRH(I+10)
      MINC(I)=JDIRH(I+14)
      MAXC(I)=JDIRH(I+18)
      LENG(I)=0
      CONS(I)=XDIRF(I+18)
      IF(I.GT.ND) GO TO 330
      LENG(I)=MAXC(I)-MINC(I)+1
      LENH=LENH*LENG(I)
  330 CONTINUE
      LENH=NHW*LENH
C
      DO 340 I=1,10
      ITIT(I)=JDIRF(I+22)
  340 CONTINUE
C
      DO 350 I=1,3
      LABX(I)=JDIRF(I+12)
      LABY(I)=JDIRF(I+15)
  350 CONTINUE
      IOF=JDIRF(12)
C
C     ------------------------------------------------------------------
C     TEST FOR 1-D OR 2-D DATA & TAKE APPROPRIATE ACTION
C     ------------------------------------------------------------------
C
      IF(ND.EQ.1)  GO TO 400
      IF(NCH.EQ.0) RETURN
      CALL HISIO('READ',LUD,LUH,NH,NDX,NCH,JDIRF,IDAT,IERR,MSER)
      RETURN
C
C     ------------------------------------------------------------------
C     RETURN 1-D DATA PROPERLY POSITIONED
C     ------------------------------------------------------------------
C
  400 LENG(1)=LENS(1)                   !MODIFY LENGTH IN CHANS
      IF(NCH.LE.0) RETURN               !RETURN IF NO-DATA REQUEST
C
      MIND=MINC(1)+1                    !LOWEST  AVAILABLE CH-INDEX
      MAXD=MAXC(1)+1                    !HIGHEST AVAILABLE CH-INDEX
      MINR=NDX(1)                       !LOWEST  REQUESTED CH-INDEX
      MAXR=MINR+NCH-1                   !HIGHEST REQUESTED CH-INDEX
C
C     ================================= !ZERO NCH CHANNELS
C
      NDO=NHW*NCH                       !# 1/2-WDS TO ZERO
      DO 420 I=1,NDO                    !LOOP TO ZERO BUFFER
      IDAT(I)=0
  420 CONTINUE
      IF(MINR.GT.MAXD) RETURN           !TST FOR DATA IN RANGE
      IF(MAXR.LT.MIND) RETURN           !TST FOR DATA IN RANGE
C
C     ==================================================================
C
      IF(MINR.GE.MIND) GO TO 440        !TST FOR 1ST CHAN AVAILABLE
      MDX(1)=1                          !IF NOT, SET 1ST ELE TO 1
      NC=MAXR-MIND+1                    !# CHANNELS TO READ
      NA=MAXD-MIND+1                    !# CHANNELS AVAILABLE
      IF(NC.GT.NA) NC=NA                !# CHANNELS TO READ
      ISK=NHW*(MIND-MINR)               !# HALF-WDS TO SKIP
      GO TO 450                         !GO DO READ
C
  440 MDX(1)=MINR-MIND+1                !1ST ELE (1ST CHAN AVAILABLE
      NC=MAXR-MINR+1                    !# CHANNELS TO READ
      NA=MAXD-MINR+1                    !# CHANNELS AVAILABLE
      IF(NC.GT.NA) NC=NA                !# CHANNELS TO READ
      ISK=0                             !# HALF-WDS TO SKIP
C
  450 CALL HISIO('READ',LUD,LUH,NH,MDX,NC,JDIRF,IDAT,IERR,MSER)
C
      IF(IERR.NE.0) RETURN              !RETURN IF ERROR
C
C     ================================= !SHIFT DATA REQUIRED AMOUNT
C
      IF(ISK.LE.0) RETURN               !RETURN IF NO SHIFT
      IMV=NHW*NC                        !# HALF-WDS TO MOVE
      N=ISK+IMV                         !HIGHEST HALF-WD TO SET
      M=IMV                             !INIT INPUT INDEX
C
      DO 460 J=1,IMV                    !LOOP TO SHIFT DATA
      IDAT(N)=IDAT(M)                   !RUNS IN REVERSE
      N=N-1                             !DEC OUTPUT CNTR
      M=M-1                             !DEC INPUT  CNTR
  460 CONTINUE
C
      DO 470 I=1,ISK                    !LOOP TO ZERO FIRST PART
      IDAT(I)=0                         !IF ANY
  470 CONTINUE
      RETURN
      END
