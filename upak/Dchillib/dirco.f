C$PROG DIRCO
      SUBROUTINE DIRCO(KDAD,LABX,LABY)
C
      INTEGER*4 IDLST(2048),IDIRF(32),LABX(3),LABY(3),TIMBF(2)
C
      INTEGER*2 IDIRH(64)
C
      DIMENSION XDIRF(32)
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      COMMON/HHH/ KHP(4,20),LHP(4),KRA(2,4),KGS(2,4),KHT,NHP,NHS,
     &            ICTY(100),NORS(100),LOCC(100),LOCG(100),ITPR(2,100),
     &            LIMS(2,100),IFBID(100),NCON
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      COMMON/QQQ/ ITEX(20),ITIT(10),NUID
C
      INTEGER*4   DATIM(6)
C
      EQUIVALENCE (IDIRH(1),IDIRF(1)),(XDIRF(1),IDIRF(1))
C
      DATA NCALL,NHID,LDR/0,0,10/
C
      INTEGER*4    HHIR,FDIR,I0001
      CHARACTER*4  CHHIR, CFDIR, CI0001
      EQUIVALENCE  (CHHIR,HHIR), (CFDIR,FDIR), (CI0001,I0001)
      DATA         cHHIR,cFDIR,cI0001/'HHIR','FDIR','0001'/
C
      SAVE
C
C     **************************************************************
C     STORES DIRECTORY ON DISK
C     **************************************************************
C     STRUCTURE OF .DIR-FILE - FIRST RECORD (128 BYTES)
C     **************************************************************
C     IDIRF(1-3)   - 'HHIRFDIR0001'
C     IDIRF(4)     - # OF HISTOGRAMS ON .HIS-FILE
C     IDIRF(5)     - # OF HALF-WORDS ON .HIS-FILE
C     IDIRF(7-12)  - YR,MO,DA,HR,MN,SC (DATE, TIME OF CHIL RUN)
C     IDIRF(13-32) - TEXT (ENTERED IN CHIL VIA $TEX COMMAND)
C     **************************************************************
C     STRUCTURE OF .DIR-FILE - DIRECTORY ENTRY (128 BYTES)
C     **************************************************************
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
C     **************************************************************
C     STRUCTURE OF .DIR-FILE - HISTOGRAM ID-LIST (64 ID'S/RECORD)
C     **************************************************************
C     IDLST(1)     - ID NUMBER OF 1ST HISTOGRAM DEFINED
C     IDLST(2)     - ID NUMBER OF 2ND HISTOGRAM DEFINED
C                  -
C     **************************************************************
C
      IF(NCALL.GT.0) GO TO 50
C
C     **************************************************************
C     WRITE A DUMMY RECORD-1 FOR LATER REPLACEMENT
C     **************************************************************
C
      DO 10 I=1,32
      IDIRF(I)=0
   10 CONTINUE
      IREC=1
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF
      CALL IOERR(IOS)
      IF(IOS.NE.0) GO TO 300
      NCALL=1
C
C     **************************************************************
C     STORE ONE DIRECTORY ENTRY PER CALL
C     **************************************************************
C
   50 IDIRH(1)=NHP                          !# HIST-PARMS
      IDIRH(2)=NHWPC                        !HALF-WDS/CHANNEL
      DO 60 I=1,4                           !LOOP ON "4 HIST-PARMS"
      IP=KHP(I,1)                           !HIST-PARM#
      IDIRH(I+2)=IP                         !HIST-PARM#
      IDIRH(I+6)=0                          !PARM-LENGTH
      IF(IP.GT.0) IDIRH(I+6)=LPAR(IP)       !PARM-LENGTH
      IDIRH(I+10)=LHP(I)                    !HIST-LENGTH
      IDIRH(I+14)=KRA(1,I)                  !RANGE (LO-CHANNEL#)
      IDIRH(I+18)=KRA(2,I)                  !RANGE (HI-CHANNEL#)
   60 CONTINUE
C
      IDIRF(12)=KDAD                        !DISK OFFSET IN HALF-WDS
C
      DO 80 I=1,3
      IDIRF(I+12)=LABX(I)                   !X-PARM OR X-PARM NAME
      IDIRF(I+15)=LABY(I)                   !Y-PARM OR Y-PARM NAME
   80 CONTINUE
C
      DO 90 I=19,22                         !LOOP ON 4 FP CONSTANTS
      XDIRF(I)=0.0
   90 CONTINUE
      DO 100 I=1,10                         !STORE 10 WDS OF TITLE
      IDIRF(I+22)=ITIT(I)
  100 CONTINUE
C
      DO 110 I=1,NHID                       !TST FOR EXISTANCE OF ID
      IF(NUID.EQ.IDLST(I)) GO TO 340        !ERROR IF ALREADY EXISTS
  110 CONTINUE
C
      NHID=NHID+1                           !INC # ID'S STORED
      IF(NHID.GT.2048) GO TO 320            !TST FOR TOO MANY ID'S
      IDLST(NHID)=NUID                      !SAVE HIST-ID
C
      IREC=IREC+1
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF   !OUTPUT DIRECTORY RECORD
      CALL IOERR(IOS)
      IF(IOS.NE.0) GO TO 300                !TST FOR ERROR
      RETURN
C
C     **************************************************************
C     ENTRY ENDIR - WINDUP CALL - OUTPUTS DIRECTORY AND 1ST BLOCK
C     **************************************************************
C
      ENTRY ENDIR
C
C     **************************************************************
C     OUTPUT "DIRECTORY" TO .DIR-FILE
C     **************************************************************
C
      ILO=1                                 !INIT ID-CNTR
  210 IF(ILO.GT.NHID) GO TO 250             !TST FOR DONE
      IHI=ILO+31
      IREC=IREC+1
      WRITE(LDR,REC=IREC,IOSTAT=IOS)(IDLST(I),I=ILO,IHI)
      CALL IOERR(IOS)
      IF(IOS.NE.0) GO TO 300                !TST FOR ERROR
      ILO=ILO+32                            !INC ID-CNTR
      GO TO 210                             !GO BACK FOR MORE
C
C     **************************************************************
C     SET-UP AND OUTPUT FIRST BLOCK OF .DIR-FILE
C     **************************************************************
C
  250 IDIRF(1)=HHIR
      IDIRF(2)=FDIR
      IDIRF(3)=I0001
      IDIRF(4)=NHID                         !# OF ID'S ON FILE
      IDIRF(5)=NXDAD                        !# OF HALF-WDS ON FILE
      IDIRF(6)=0                            !NOT USED
C
      CALL MILYMDHMS(DATIM)                 !Get Y,M,D,H,M,S
C
      DO 255 I=1,6
      IDIRF(I+6)=DATIM(I)                   !Save in header
  255 CONTINUE
C
      DO 270 I=1,20                         !20 WDS OF "TEXT"
      IDIRF(I+12)=ITEX(I)
  270 CONTINUE
C
      IREC=1                                !LOCATE FIRST RECORD
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF   !OUTPUT FIRST RECORD
      CALL IOERR(IOS)
      IF(IOS.NE.0) GO TO 300                !TST FOR ERROR
      RETURN
C
  300 NERR=NERR+1
      WRITE(LU6,310)
  310 FORMAT(1H ,'TRYING TO WRITE DIRECTORY FILE')
      RETURN
  320 NERR=NERR+1
      WRITE(LU6,330)
  330 FORMAT(1H ,'.DIR-FILE DIRECTORY OVERFLOW - MAX# = 2048')
      RETURN
  340 NERR=NERR+1
      WRITE(LU6,350)NUID
  350 FORMAT(1H ,'HISTOGRAM ID# ',I8,' ALREADY DEFINED')
      RETURN
      END
