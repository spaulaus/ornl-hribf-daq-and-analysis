C$PROG CMPHGEN   - Sets up header for new histogram & makes it
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/01/04
C     ******************************************************************
C
      SUBROUTINE CMPHGEN(IDONE,IERR)
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/TDX2/ LUDI,LUHI,LUDO,LUHO
      INTEGER*4    LUDI,LUHI,LUDO,LUHO
C     ------------------------------------------------------------------
      COMMON/TDX10/NXID
      INTEGER*4    NXID
C
      DATA         NXID/0/
C     ------------------------------------------------------------------
      CHARACTER*4  IDONE,KMD,KMX
C
      INTEGER*4    IERR
C
      INTEGER*4    HEDF(32),HEDSAV(32)
      INTEGER*2    HEDH(64)
C
      EQUIVALENCE (HEDH,HEDF),(KMD,LWD(1,1)),(KMX,LWD(1,2))
C
      INTEGER*4    LOGB2,IV,KIND,IOF,NCHN(2),NBYTS,N,I,J
C
      REAL*4       XV
C
      INTEGER*2    HDIM,MINC(2),MAXC(2),HPAR(2),HWPC,LRAW(2),LSCL(2)
C
      CHARACTER*12 XLAB,YLAB,LABUF
C
      CHARACTER*40 HTIT,TITBUF
C
      EQUIVALENCE (LABUF,IWD(2)),(TITBUF,IWD(2))
C
      EQUIVALENCE 
     & (HDIM,HEDH(1)),  !HISTOGRAM DIMENSIONALITY (MAX = 4)
     & (HWPC,HEDH(2)),  !NUMBER OF HALF-WORDS PER CHANNEL (1 OR 2)
     & (HPAR,HEDH(3)),  !HISTOGRAM PARM#'S (UP TO 4 PARAMETERS)
     & (LRAW,HEDH(7)),  !LENGTH OF RAW    PARAMETERS (PWR OF 2)
     & (LSCL,HEDH(11)), !LENGTH OF SCALED PARAMETERS (PWR OF 2)
     & (MINC,HEDH(15)), !MIN CHANNEL# LIST
     & (MAXC,HEDH(19)), !MAX CHANNEL# LIST
     & (IOF ,HEDF(12)), !DISK OFFSET IF HALF-WORDS (1ST WORD# MINUS 1)
     & (XLAB,HEDF(13)), !X-PARM LABEL
     & (YLAB,HEDF(16)), !Y-PARM LABEL
     & (HTIT,HEDF(23))  !SUB-TITLE (40 BYTES) (ENTERED VIA $TIT CMD)
C
      DATA HDIM/1/
      DATA HWPC/2/
      DATA HPAR/1,2/
      DATA LRAW/8192,8192/
      DATA LSCL/1024,1024/
      DATA MINC/0,0/
      DATA MAXC/1023,1023/
      DATA XLAB/'X-label'/
      DATA YLAB/'Y-label'/
      DATA HTIT/'Histogram title'/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     STRUCTURE OF .DRR-FILE - DIRECTORY ENTRY (128 BYTES)
C     ------------------------------------------------------------------
C     HEDH(1)     - HISTOGRAM DIMENSIONALITY (MAX = 4)
C     HEDH(2)     - NUMBER OF HALF-WORDS PER CHANNEL (1 OR 2)
C     HEDH(3-6)   - HISTOGRAM PARM#'S (UP TO 4 PARAMETERS)
C     HEDH(7-10)  - LENGTH OF RAW    PARAMETERS (PWR OF 2)
C     HEDH(11-14) - LENGTH OF SCALED PARAMETERS (PWR OF 2)
C     HEDH(15-18) - MIN CHANNEL# LIST
C     HEDH(19-22) - MAX CHANNEL# LIST
C     HEDF(12)    - DISK OFFSET IF HALF-WORDS (1ST WORD# MINUS 1)
C     HEDF(13-15) - X-PARM LABEL
C     HEDF(16-18) - Y-PARM LABEL
C     XHEDF(19-22)- CALIBRATION CONSTANTS (UP TO 4 FP NUMBERS)
C     HEDF(23-32) - SUB-TITLE (40 BYTES) (ENTERED VIA $TIT CMD)
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'HDIM') GO TO 100
      IF(KMD.EQ.'HWPC') GO TO 200
      IF(KMD.EQ.'HPAR') GO TO 300
      IF(KMD.EQ.'LRAW') GO TO 400
      IF(KMD.EQ.'LSCL') GO TO 500
      IF(KMD.EQ.'MINC') GO TO 600
      IF(KMD.EQ.'MAXC') GO TO 700
      IF(KMD.EQ.'XLAB') GO TO 800
      IF(KMD.EQ.'YLAB') GO TO 900
      IF(KMD.EQ.'HTIT') GO TO 1000
      IF(KMD.EQ.'NXID') GO TO 1020
      IF(KMD.EQ.'HEDD') GO TO 1050
C
      IF(KMD.EQ.'HGEN') GO TO 1200
C
      RETURN
C
C     ------------------------------------------------------------------
C     Process - HDIM IV
C     ------------------------------------------------------------------
C
  100 IF(NF.NE.2) GO TO 2000
      CALL LIMIV(LWD(1,2),1,2,IV,IERR)
      IF(IERR.NE.0) GO TO 2010
      HDIM=IV
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - HWPC IV
C     ------------------------------------------------------------------
C
  200 IF(NF.NE.2) GO TO 2000
      CALL LIMIV(LWD(1,2),1,2,IV,IERR)
      IF(IERR.NE.0) GO TO 2020
      HWPC=IV
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - HPAR PX PY
C     ------------------------------------------------------------------
C
  300 IF(NF.LT.2.OR.NF.GT.3) GO TO 2000
      DO 310 I=1,2
      CALL LIMIV(LWD(1,I+1),1,32767,IV,IERR)
      IF(IERR.NE.0) GO TO 2030
      HPAR(I)=IV
  310 CONTINUE
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - LRAW LX LY
C     ------------------------------------------------------------------
C
  400 IF(NF.LT.2.OR.NF.GT.3) GO TO 2000
      DO 410 I=1,2
      CALL LIMIV(LWD(1,I+1),512,16384,IV,IERR)
      IF(IERR.NE.0) GO TO 2040
      LRAW(I)=IV
      IF(LOGB2(IV).LT.0) GO TO 2045
  410 CONTINUE
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - LSCL LX LY
C     ------------------------------------------------------------------
C
  500 IF(NF.LT.2.OR.NF.GT.3) GO TO 2000
      DO 510 I=1,2
      CALL LIMIV(LWD(1,I+1),32,16384,IV,IERR)
      IF(IERR.NE.0) GO TO 2050
      LSCL(I)=IV
      IF(LOGB2(IV).LT.0) GO TO 2055
  510 CONTINUE
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - MINC MINX MINY
C     ------------------------------------------------------------------
C
  600 IF(NF.LT.2.OR.NF.GT.3) GO TO 2000
      DO 610 I=1,2
      CALL LIMIV(LWD(1,I+1),0,32767,IV,IERR)
      IF(IERR.NE.0) GO TO 2060
      MINC(I)=IV
  610 CONTINUE
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - MAXC MAXX MAXY
C     ------------------------------------------------------------------
C
  700 IF(NF.LT.2.OR.NF.GT.3) GO TO 2000
      DO 710 I=1,2
      CALL LIMIV(LWD(1,I+1),0,32767,IV,IERR)
      IF(IERR.NE.0) GO TO 2070
      MAXC(I)=IV
  710 CONTINUE
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - XLAB X-label
C     ------------------------------------------------------------------
C
  800 XLAB=LABUF
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - YLAB Y-label
C     ------------------------------------------------------------------
C
  900 YLAB=LABUF
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - HTIT Histogram-title
C     ------------------------------------------------------------------
C
 1000 HTIT=TITBUF
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - NXID IV - Next output histogram ID to use
C     ------------------------------------------------------------------
C
 1020 CALL LIMIV(LWD(1,2),0,32767,IV,IERR)
      IF(IERR.NE.0) GO TO 2080
      NXID=IV
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - HEDD                   (display header data)
C     ------------------------------------------------------------------
C
 1050 DO 1060 I=1,32
      HEDSAV(I)=HEDF(I)
 1060 CONTINUE
      IF(HDIM.EQ.1) THEN
      MINC(2)=0
      MAXC(2)=0
      LRAW(2)=0
      LSCL(2)=0
      HPAR(2)=0
      YLAB   =' '
      ENDIF
C
      WRITE(CMSSG,1105)HDIM
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1110)HWPC
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1115)HPAR
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1120)LRAW
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1125)LSCL
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1130)MINC
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1135)MAXC
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1140)XLAB
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1145)YLAB
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1150)HTIT
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,1155)NXID
      CALL MESSLOG(LOGUT,LOGUP)
C
 1105 FORMAT('HDIM =',I6)
 1110 FORMAT('HWPC =',I6)
 1115 FORMAT('HPAR =',2I6)
 1120 FORMAT('LRAW =',2I6)
 1125 FORMAT('LSCL =',2I6)
 1130 FORMAT('MINC =',2I6)
 1135 FORMAT('MAXC =',2I6)
 1140 FORMAT('XLAB = ',A)
 1145 FORMAT('YLAB = ',A)
 1150 FORMAT('HTIT = ',A)
 1155 FORMAT('NXID = ',I6)
C
      DO 1160 I=1,32
      HEDF(I)=HEDSAV(I)
 1160 CONTINUE
C
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Process - HGEN NXID or HGEN ID
C     ------------------------------------------------------------------
C
 1200 IF(KMX.EQ.'NXID') THEN
      IF(NXID.LE.0) GO TO 2130
      IV=NXID
      GO TO 1205
      ENDIF
C
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      IF(IV.LE.0)   GO TO 2000
C
 1205 NCHN(1)=1
      NCHN(2)=1
      N=0
      DO 1210 I=1,HDIM
      N=N+1
      IF(MAXC(I).LT.MINC(I)) GO TO 2110
      NCHN(I)=MAXC(I)-MINC(I)+1
 1210 CONTINUE
C
      IF(HDIM.EQ.1) NBYTS=2*HWPC*NCHN(1)
      IF(HDIM.EQ.2) NBYTS=2*HWPC*NCHN(1)*NCHN(2)
C
      IF(NBYTS.GT.268435456) GO TO 2120
C
      DO 1220 I=1,32
      HEDSAV(I)=HEDF(I)
 1220 CONTINUE
      IF(HDIM.EQ.1) THEN
      MINC(2)=0
      MAXC(2)=0
      LRAW(2)=0
      LSCL(2)=0
      HPAR(2)=0
      YLAB   =' '
      ENDIF
C
      CALL EXPANMAN(LUDO,LUHO,IV,HEDF,IERR)
C
      DO 1230 I=1,32
      HEDF(I)=HEDSAV(I)
 1230 CONTINUE
C
      IF(IERR.NE.0) GO TO 2300
C
      IF(KMX.EQ.'NXID'.AND.NXID.GT.0) NXID=NXID+1
C
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 2000 WRITE(CMSSG,2005)
 2005 FORMAT('Syntax error or illegal value - Ignored')
      GO TO 2300
C
 2010 WRITE(CMSSG,2015)
 2015 FORMAT('Syntax error or HDIM out of range 1-2 - Ignored')
      GO TO 2400
C
 2020 WRITE(CMSSG,2025)
 2025 FORMAT('Syntax error or HWPC out of range 1-2 - Ignored')
      GO TO 2400
C
 2030 WRITE(CMSSG,2035)
 2035 FORMAT('Syntax error or HPAR out of range 1-32767 - Ignored')
      GO TO 2400
C
 2040 WRITE(CMSSG,2042)
 2042 FORMAT('Syntax error or LRAW out of range 512-16384 - Ignored')
      GO TO 2400
C
 2045 WRITE(CMSSG,2047)IV
 2047 FORMAT('Value of LRAW =',I10,' is not required power of 2')
      GO TO 2400
C
 2050 WRITE(CMSSG,2052)
 2052 FORMAT('Syntax error or LSCL out of range 32-16384 - Ignored')
      GO TO 2400
C
 2055 WRITE(CMSSG,2057)
 2057 FORMAT('Value of LSCL =',I10,' is not required power of 2')
      GO TO 2400
C
 2060 WRITE(CMSSG,2065)
 2065 FORMAT('Syntax error or MINC out of range 0-32767 - Ignored')
      GO TO 2400
C
 2070 WRITE(CMSSG,2075)
 2075 FORMAT('Syntax error or MAXC out of range 0-32767 - Ignored')
      GO TO 2400
C
 2080 WRITE(CMSSG,2085)
 2085 FORMAT('Syntax error or NXID out of range 0-32767 - Ignored')
      GO TO 2400
C
 2100 WRITE(CMSSG,2105)IV
 2105 FORMAT('Unable to create HID# ',I8,' - Ignored')
      GO TO 2400
C
 2110 WRITE(CMSSG,2115)MINC(N),MAXC(N)
 2115 FORMAT('MINC =',I5,' .GT. MAXC =',I5,' -  HGEN not processed')
      GO TO 2400
C
 2120 WRITE(CMSSG,2125)NBYTS
 2125 FORMAT('NBYTS =',I12,' is just too big - HGEN not processed')
      GO TO 2400
C
 2130 WRITE(CMSSG,2135)
 2135 FORMAT('HGEN NXID request rejected - NXID undefined')
      GO TO 2400
C
C     ------------------------------------------------------------------
C     RETURN - normal or otherwise
C     ------------------------------------------------------------------
C
 2300 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,2305)((LWD(I,J),I=1,2),J=1,4)
 2305 FORMAT('Processing: ',8A4)
      GO TO 2400
C
 2400 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
C
 2500 IDONE='YES '
      RETURN
      END
