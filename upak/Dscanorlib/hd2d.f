C$PROG HD2D      - Defines space for 2-D histograms
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 11/17/99
C     ******************************************************************
C
C     ------------------------------------------------------------------
C
      SUBROUTINE HD2D(
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
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    MXNH
C
      PARAMETER   (MXNH=6144)
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
C     ------------------------------------------------------------------
      COMMON/SC24/ IDIRF(32),IDLST(MXNH),NXOFF,IREC,NHID,LSOFF,NCALL
C
      INTEGER*4    IDIRF,    IDLST,      NXOFF,IREC,NHID,LSOFF
      CHARACTER*4                                              NCALL
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
      INTEGER*4    LDR
C
      REAL*4       XDIRF(32),XCMP
C
      EQUIVALENCE (IDIRH(1),IDIRF(1)), (XDIRF(1),IDIRF(1))
C
      EQUIVALENCE (IDIRF(13),CLABX),   (IDIRF(16),CLABY)
C
      EQUIVALENCE (IDIRF(23),CTIT),    (LUC(9),LDR)
C
      INTEGER*4    LUERR
C
      DATA         IDIRF/32*0/
C
      DATA         LUERR/0/
C     ==================================================================
      INTEGER*4    HID,HID0,NHWPC,NHWPC0,HDIM,HDIM0
C
      INTEGER*4    IOS,LHIS,HCMP,LENHIS
C
      INTEGER*4    NEAR2
C
      INTEGER*4    JDIM
C
      INTEGER*4    RAWLX,RAWLX0,HSTLX,HSTLX0,MNX,MNX0,MXX,MXX0
C
      INTEGER*4    RAWLY,RAWLY0,HSTLY,HSTLY0,MNY,MNY0,MXY,MXY0
C
      INTEGER*4    ILO,IHI,I,J
C     ------------------------------------------------------------------
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
   30 FORMAT(1H ,'***HDEF: HID',I5,' MAX# OF HISTOGRAMS EXCEEDED')
   35 FORMAT(1H ,'***HDEF: HID',I5,' ALREADY DEFINED')
   40 FORMAT(1H ,'***HDEF: HID',I5,' HDIM =',I4,' ILLEGAL - SET TO 1')
   45 FORMAT(1H ,'***HDEF: HID',I5,' NHWPC=',I4,' ILLEGAL - SET TO 2')
C
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
      IF(NCALL.EQ.'DONE') GO TO 100
C
C     ------------------------------------------------------------------
C     WRITE A DUMMY RECORD-1 FOR LATER REPLACEMENT
C     ------------------------------------------------------------------
C
      NXOFF=0
      LSOFF=0
      MAXOFF=0 
      NHID=0
      CTIT=' '
      CLABX=' '
      CLABY=' '
      DO 85 I=1,32
      IDIRF(I)=0
   85 CONTINUE
      IREC=1
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1000
      NCALL='DONE'
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
      GO TO 1000
      ENDIF
C
      DO 105 I=1,NHID                      !CHECK FOR DUPLICATE ID
      IF(HID.EQ.IDLST(I))THEN
      WRITE(LUERR,35)HID
      GO TO 1000
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
      IF(IOS.NE.0)GOTO 1000
      RETURN
C
C     ------------------------------------------------------------------
C     Error return
C     ------------------------------------------------------------------
C
 1000 RETURN
      END
