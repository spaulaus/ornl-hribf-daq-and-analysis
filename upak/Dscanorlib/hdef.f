C$PROG HD1D      - Defines histogram space in shared memory 
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 02/17/99
C     ******************************************************************
C
C     ------------------------------------------------------------------
C
      SUBROUTINE HD1D(
     &      HID0,     ! [INT] HIST. ID 
     &      NHWPC0,   ! [INT] NO. OF HALF WORDS PER CHANNEL  
     &      RAWL0,    ! [INT] RAW PARAMETER LENGTH 
     &      HSTL0,    ! [INT] HISTOGRAMMED PARAM LENGTH 
     &      MN0,      ! [INT] MINIMUM PARAM "RANGE" VALUE
     &      MX0,      ! [INT] MAXIMUM PARAM "RANGE" VALUE 
     &      TIT)      ! [CHAR*N] (N<40) TITLE FOR HISTOGRAM
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC17/ IOFF(8000),IOFH(8000),NDIM(8000),NHPC(8000),
     &             LENX(8000),LENH(8000)
C
      INTEGER*2    LENX,                 NDIM,      NHPC
      INTEGER*4    IOFF,      IOFH
      INTEGER*4               LENH
C     ------------------------------------------------------------------
      COMMON/SC18/ ICMP(4,8000),IMIN(4,8000),IMAX(4,8000),MAXOFF
C
      INTEGER*2    ICMP,        IMIN,        IMAX
      INTEGER*4                                           MAXOFF
C     ==================================================================
C
      CHARACTER*(*) TIT
C
      CHARACTER*12 CLABX,CLABY
C
      CHARACTER*40 CTIT
C
      INTEGER*2    IDIRH(64)
C
      INTEGER*4    MXNH
C
      PARAMETER   (MXNH=6144)
C
      INTEGER*4    IDIRF(32),IDLST(MXNH)
C
      CHARACTER*4  CDIRF(32)
C
      INTEGER*4    IDATIM(6),LDR
C
      REAL*4       XDIRF(32),XCMP
C
      INTEGER*4    RAWL_N(4),HSTL_N(4),MAX_N(4),MIN_N(4)
C
      EQUIVALENCE (IDIRH(1),IDIRF(1)), (XDIRF(1),IDIRF(1))
      EQUIVALENCE (CDIRF(1),IDIRF(1))
      EQUIVALENCE (IDIRF(13),CLABX),   (IDIRF(16),CLABY)
      EQUIVALENCE (IDIRF(23),CTIT),    (LUC(9),LDR)
C
      DATA         IDIRF/32*0/
C
      INTEGER*4    NCALL,NHID,MXOFF,LSOFF,LUERR,NXOFF
C
      DATA         NCALL,NHID,NXOFF,LSOFF/4*0/
C
      DATA         LUERR/0/
C     ==================================================================
      INTEGER*4    HID,HID0,NHWPC,NHWPC0,HDIM,HDIM0
C
      INTEGER*4    IREC,IOS,LHIS,HCMP,LENHIS
C
      INTEGER*4    NEAR2
C
      INTEGER*4    RAWL,RAWL0,HSTL,HSTL0,MN,MN0,MX,MX0,JDIM
C
      INTEGER*4    RAWLX,RAWLX0,HSTLX,HSTLX0,MNX,MNX0,MXX,MXX0
C
      INTEGER*4    RAWLY,RAWLY0,HSTLY,HSTLY0,MNY,MNY0,MXY,MXY0
C
      INTEGER*4    ILO,IHI,NERR,I,J
C
      SAVE
C
C     ------------------------------------------------------------------
C     STORES DIRECTORY ON DISK
C     ------------------------------------------------------------------
C     STRUCTURE OF .DRR-FILE - FIRST RECORD (128 BYTES)
C     ------------------------------------------------------------------
C     IDIRF(1-3)   - 'HHIRFDIR0001'
C     IDIRF(4)     - # OF HISTOGRAMS ON .HIS-FILE
C     IDIRF(5)     - # OF HALF-WORDS ON .HIS-FILE
C     IDIRF(7-12)  - YR,MO,DA,HR,MN,SC (DATE, TIME OF CHIL RUN)
C     IDIRF(13-32) - TEXT (ENTERED IN CHIL VIA $TEX COMMAND)
C     ------------------------------------------------------------------
C     STRUCTURE OF .DIR-FILE - DIRECTORY ENTRY (128 BYTES)
C     ------------------------------------------------------------------
C     IDIRH(1)     - HISTOGRAM DIMENSIONALITY (MAX = 4)
C     IDIRH(2)     - NUMBER OF HALF-WORDS PER CHANNEL (1 OR 2)
C     IDIRH(3-6)   - HISTOGRAM PARM#'S (UP TO 4 PARAMETERS)
C     IDIRH(7-10)  - LENGTH OF RAW PARAMETERS (PWR OF 2)
C     IDIRH(11-14) - LENGTH OF SCALED PARAMETERS (PWR OF 2)
C     IDIRH(15-18) - MIN CHANNEL# LIST
C     IDIRH(19-22) - MAX CHANNEL# LIST
C     IDIRF(12)    - DISK OFFSET IF HALF-WORDS (1ST WORD# MINUS 1)
C     IDIRF(13-15) - X-PARM LABEL
C     IDIRF(16-18) - Y-PARM LABEL
C     XDIRF(19-22) - CALIBRATION CONSTANTS (UP TO 4 FP NUMBERS)
C     IDIRF(23-32) - SUB-TITLE (40 BYTES) (ENTERED VIA $TIT CMD)
C     ------------------------------------------------------------------
C     STRUCTURE OF .DIR-FILE - HISTOGRAM ID-LIST (64 ID'S/RECORD)
C     ------------------------------------------------------------------
C     IDLST(1)     - ID NUMBER OF 1ST HISTOGRAM DEFINED
C     IDLST(2)     - ID NUMBER OF 2ND HISTOGRAM DEFINED
C                  -
C     ------------------------------------------------------------------
C
C
CX    WRITE(6,888)HID0,NHWPC0,RAWL0,HSTL0,MN0,MX0
CX888 FORMAT(1H ,'ID,HWPC,RAWL,HSTL,MN,MX =',6I8)
C
   30 FORMAT(1H ,'***HDEF: HID',I5,' MAX# OF HISTOGRAMS EXCEEDED')
   35 FORMAT(1H ,'***HDEF: HID',I5,' ALREADY DEFINED')
   40 FORMAT(1H ,'***HDEF: HID',I5,' HDIM =',I4,' ILLEGAL - SET TO 1')
   45 FORMAT(1H ,'***HDEF: HID',I5,' NHWPC=',I4,' ILLEGAL - SET TO 2')
C
      HID=HID0
      NHWPC=NHWPC0
      RAWL=RAWL0
      HSTL=HSTL0
      MN=MN0
      MX=MX0
C
C     ------------------------------------------------------------------
C     WRITE A DUMMY RECORD-1 FOR LATER REPLACEMENT
C     ------------------------------------------------------------------
C
      IF(NCALL.NE.0) GO TO 70
C
      MAXOFF=0 
      NXOFF=0
      LSOFF=0
      CTIT=' '
      CLABX=' '
      CLABY=' '
      DO 60 I=1,32
      IDIRF(I)=0
   60 CONTINUE
      IREC=1
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 777
      NCALL=1
C
C     ------------------------------------------------------------------
C     STORE ONE DIRECTORY ENTRY PER CALL
C     ------------------------------------------------------------------
C
   70 NHID=NHID+1
      LHIS=1
C
      IF(NHID.GT.MXNH)THEN                !CHECK FOR TOO MANY IDs
      WRITE(LUERR,30)HID
      GO TO 777
      ENDIF
C
      DO 75 I=1,NHID                      !CHECK FOR DUPLICATE ID
      IF(HID.EQ.IDLST(I))THEN
      WRITE(LUERR,35)HID
      GO TO 777
      ENDIF
   75 CONTINUE
C
      JDIM=1
      IF(NHWPC.LT.1.OR.NHWPC.GT.2)THEN    !CHECK FOR ILLEGAL HWPC
      WRITE(LUERR,45)HID,NHWPC
      NHWPC=2
      ENDIF
C
      IDIRH(1)=JDIM
      IDIRH(2)=NHWPC
      I=1
      IDIRH(2+I)=0
      RAWL=NEAR2(RAWL)
      HSTL=NEAR2(HSTL)
      HSTL=MIN(HSTL,16384)
      RAWL=MIN(RAWL,16384)
C
      IF(RAWL.LE.0)RAWL=2048
      IF(HSTL.LE.0)HSTL=RAWL
      IF(MN.LT.0)MN=0
      IF(MX.LE.0.OR.MX.GT.HSTL-1)MX=HSTL-1
      XCMP=RAWL/HSTL
C
      IF(XCMP.LE.1.)THEN
      HCMP=0
      ELSE
      XCMP=LOG(XCMP)/LOG(2.)
      HCMP=ABS(NINT(XCMP))
      ENDIF
C
      IDIRH(6+I)=RAWL
      IDIRH(10+I)=HSTL
      IDIRH(14+I)=MN
      IDIRH(18+I)=MX
      J=HID
      ICMP(I,HID)=HCMP
      IMIN(I,HID)=MN
      IMAX(I,HID)=MX
      LHIS=LHIS*(MX-MN+1)
      LENHIS=LHIS*NHWPC
      LENHIS=LENHIS+MOD(LENHIS,2)	!Make it even
      LSOFF=NXOFF
      NXOFF=NXOFF+LENHIS
      IDIRF(12)=LSOFF
      CTIT=' '
      CLABX=' '
      CLABY=' '
      CTIT=TIT
C
      DO 80 I=19,22
      XDIRF(I)=0.0
   80 CONTINUE
C
      IOFH(HID)=LSOFF+1
      IOFF(HID)=LSOFF/2 + 1
      NDIM(HID)=JDIM
      NHPC(HID)=NHWPC
      LENX(HID)= MX-MN+1
      IREC=IREC+1
      IDLST(NHID)=HID
C
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF         !OUTPUT DRR RECORD
      CALL IOFERR(IOS)
      IF(IOS.NE.0)GOTO 777
      RETURN
C
C$ENTR HD2D      - Sets up space for 2-D histograms
C
C     ------------------------------------------------------------------
C     ENTRY  HD2D - ALTERATE ENTRY FOR 2D HISTOGRAM
C     ------------------------------------------------------------------
C
      ENTRY HD2D(
     &      HID0,    ! [INT] HIST. ID 
     &      NHWPC0,  ! [INT] NO. OF HALF WORDS PER CHANNEL  
     &      RAWLX0,  ! [INT] RAW X PARAM LENGTH  
     &      HSTLX0,  ! [INT] HISTOGRAMMED X PARAM LENGTH 
     &      MNX0,    ! [INT] MINIMUM X PARAM "RANGE" VALUE
     &      MXX0,    ! [INT] MAXIMUM X PARAM "RANGE" VALUE 
     &      RAWLY0,  ! [INT] RAW Y PARAM LENGTH 
     &      HSTLY0,  ! [INT] HISTOGRAMMED Y PARAM LENGTH 
     &      MNY0,    ! [INT] MINIMUM Y PARAM "RANGE" VALUE
     &      MXY0,    ! [INT] MAXIMUM Y PARAM "RANGE" VALUE 
     &      TIT)     ! [CHAR*N] (N<40) TITLE FOR HISTOGRAM
C
C
C     ------------------------------------------------------------------
C     WRITE A DUMMY RECORD-1 FOR LATER REPLACEMENT
C     ------------------------------------------------------------------
C
      HID=HID0
      NHWPC=NHWPC0
      RAWLX=RAWLX0
      HSTLX=HSTLX0
      MNX=MNX0
      MXX=MXX0
      RAWLY=RAWLY0
      HSTLY=HSTLY0
      MNY=MNY0
      MXY=MXY0
C
      IF(NCALL.NE.0) GO TO 100
C
      NXOFF=0
      LSOFF=0
      MAXOFF=0 
      CTIT=' '
      CLABX=' '
      CLABY=' '
      DO 85 I=1,32
      IDIRF(I)=0
   85 CONTINUE
      IREC=1
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 777
      NCALL=1
C
C     ------------------------------------------------------------------
C     STORE ONE DIRECTORY ENTRY PER CALL
C     ------------------------------------------------------------------
C
  100 NHID=NHID+1
      LHIS=1
C
      IF(NHID.GT.MXNH)THEN                !CHECK FOR TOO MANY IDs
      WRITE(LUERR,30)HID
      GO TO 777
      ENDIF
C
      DO 105 I=1,NHID                      !CHECK FOR DUPLICATE ID
      IF(HID.EQ.IDLST(I))THEN
      WRITE(LUERR,35)HID
      GO TO 777
      ENDIF
  105 CONTINUE
C
      JDIM=2
      IF(NHWPC.LT.1.OR.NHWPC.GT.2)THEN    !CHECK FOR ILLEGAL HWPC
      WRITE(LUERR,45)HID,NHWPC
      NHWPC=2
      ENDIF
C
      IDIRH(1)=JDIM
      IDIRH(2)=NHWPC
      IDIRH(3)=0
      IDIRH(4)=0
      I=1
      RAWLX=NEAR2(RAWLX)
      HSTLX=NEAR2(HSTLX)
      HSTLX=MIN(HSTLX,16384)
      RAWLX=MIN(RAWLX,16384)
C
      IF(RAWLX.LE.0)RAWLX=2048
      IF(HSTLX.LE.0)HSTLX=RAWLX
      IF(MNX.LT.0)MNX=0
      IF(MXX.LE.0.OR.MXX.GT.HSTLX-1)MXX=HSTLX-1
      XCMP=RAWLX/HSTLX
C
      IF(XCMP.LE.1.)THEN
      HCMP=0
      ELSE
      XCMP=LOG(XCMP)/LOG(2.)
      HCMP=ABS(NINT(XCMP))
      ENDIF
C
      IDIRH(6+I)=RAWLX
      IDIRH(10+I)=HSTLX
      IDIRH(14+I)=MNX
      IDIRH(18+I)=MXX
      ICMP(I,HID)=HCMP
      IMIN(I,HID)=MNX
      IMAX(I,HID)=MXX
      LHIS=LHIS*(MXX-MNX+1)
      I=2
      RAWLY=NEAR2(RAWLY)
      HSTLY=NEAR2(HSTLY)
      HSTLY=MIN(HSTLY,16384)
      RAWLY=MIN(RAWLY,16384)
C
      IF(RAWLY.LE.0)RAWLY=2048
      IF(HSTLY.LE.0)HSTLY=RAWLY
      IF(MNY.LT.0)MNY=0
      IF(MXY.LE.0.OR.MXY.GT.HSTLY-1)MXY=HSTLY-1
C
      XCMP=RAWLY/HSTLY
C
      IF(XCMP.LE.1.)THEN
      HCMP=0
      ELSE
      XCMP=LOG(XCMP)/LOG(2.)
      HCMP=ABS(NINT(XCMP))
      ENDIF
C
      IDIRH(6+I)=RAWLY
      IDIRH(10+I)=HSTLY
      IDIRH(14+I)=MNY
      IDIRH(18+I)=MXY
      ICMP(I,HID)=HCMP
      IMIN(I,HID)=MNY
      IMAX(I,HID)=MXY
      LHIS=LHIS*(MXY-MNY+1)
      LENHIS=LHIS*NHWPC
      LENHIS=LENHIS+MOD(LENHIS,2)	!Make it even
      LSOFF=NXOFF
      NXOFF=NXOFF+LENHIS
      IDIRF(12)=LSOFF
      CTIT=' '
      CLABX=' '
      CLABY=' '
      CTIT=TIT
C
      DO 110 I=19,22
      XDIRF(I)=0.0
  110 CONTINUE
C
      IOFH(HID)=LSOFF+1
      IOFF(HID)=LSOFF/2 + 1
      NDIM(HID)=JDIM
      NHPC(HID)=NHWPC
      LENX(HID)= MXX-MNX+1
      IREC=IREC+1
      IDLST(NHID)=HID
C
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF         !OUTPUT DRR RECORD
      CALL IOFERR(IOS)
      IF(IOS.NE.0)GOTO 777
      RETURN
C
C$ENTR ENDRR     - Windup call - outputs directory & first block
C
C     ------------------------------------------------------------------
C     ENTRY ENDRR - WINDUP CALL - OUTPUTS DIRECTORY AND 1ST BLOCK
C     ------------------------------------------------------------------
C
      ENTRY ENDRR
C
C     ------------------------------------------------------------------
C     OUTPUT "DIRECTORY" TO .DRR-FILE
C     ------------------------------------------------------------------
C
      MAXOFF=NXOFF
      ILO=1                                 !INIT ID-CNTR
200   IF(ILO.GT.NHID) GOTO 250              !TST FOR DONE
      IHI=ILO+31
      IREC=IREC+1
      WRITE(LDR,REC=IREC,IOSTAT=IOS)(IDLST(I),I=ILO,IHI)
      IF(IOS.NE.0)CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 777                !TST FOR ERROR
      ILO=ILO+32                            !INC ID-CNTR
      GO TO 200                             !GO BACK FOR MORE
C
C     ------------------------------------------------------------------
C     SET-UP AND OUTPUT FIRST BLOCK OF .DIR-FILE
C     ------------------------------------------------------------------
C
250   CDIRF(1)='HHIR'
      CDIRF(2)='FDIR'
      CDIRF(3)='0001'
      IDIRF(4)=NHID                         !# OF ID'S ON FILE
      IDIRF(5)=NXOFF                        !# OF HALF-WDS ON FILE
      IDIRF(6)=0                            !NOT USED
      CALL MILYMDHMS(IDATIM)                !GET DATE & TIME
C
      IDIRF(7)= IDATIM(1)                   !YEAR
      IDIRF(8)= IDATIM(2)                   !MONTH
      IDIRF(9)= IDATIM(3)                   !DAY
      IDIRF(10)=IDATIM(4)                   !HOUR
      IDIRF(11)=IDATIM(5)                   !MINUTE
      IDIRF(12)=IDATIM(6)                   !SECOND
C
      IREC=1                                !LOCATE FIRST RECORD
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF   !OUTPUT FIRST RECORD
      CLOSE(LDR)
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 777                !TST FOR ERROR
      RETURN
C
C     ------------------------------------------------------------------
C     Error return
C     ------------------------------------------------------------------
C
  777 NERR=NERR+1
      RETURN
      END
