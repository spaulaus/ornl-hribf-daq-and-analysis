C$PROG LDFWRIT   - Writes list-data records to LDF disk-file
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE LDFWRIT(LU,MODE,NUMBY,IBUF,IERR)
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
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      INTEGER*4    IBUF(8192),OBUF(8192),LU,NUMBY,IERR
C
      CHARACTER*4  COBUF(8192)
      EQUIVALENCE (COBUF,OBUF)
C
      INTEGER*4    DIR(2,4000)
C
      EQUIVALENCE (DIR,OUDIR(5))
C
      INTEGER*4    IOS,HEDN,NFW,NFWX,I,J
C
      INTEGER*4    NFWD,NPAC
C
      INTEGER*4    FWRECL,NHED,NREC,NT,NMAR
C
      CHARACTER*4  KIND,MODE,LMODE
C
      EQUIVALENCE (FWRECL,OUDIR(1)),          !Full-word block-size
     &            (NHED,  OUDIR(4)),          !Number of Header records
     &            (NREC,  OUDIR(2))           !Total number of records
C     ------------------------------------------------------------------
      DATA         NFWD,NPAC,LMODE/8192,0,'    '/
C
      DATA         FWRECL,NHED,NREC/8194,0,0/
C
      SAVE
C
C     ------------------------------------------------------------------
C     Test for accumulated PAC-buffer
C     If MODE.NE.PAC, Output any PAC-buffers that have been accumulated
C     ------------------------------------------------------------------
C
      IF(OUFOP.NE.'YES ') RETURN
C
      IERR=0
C
      NFW=NUMBY/4
C
      IF(MODE.NE.'PAC '.AND.LMODE.EQ.'PAC ') THEN
      IF(OURECI.LT.NREC) NREC=OURECI
      NREC=NREC+1
      KIND='PAC '
      WRITE(LU,REC=NREC,IOSTAT=IOS)KIND,NPAC,(OBUF(I),I=1,NPAC)
      IF(IOS.NE.0) GO TO 1000
      OURECI=NREC
      ENDIF
C
C     ------------------------------------------------------------------
C     Process the different output MODEs
C     ------------------------------------------------------------------
C
      IF(MODE.EQ.'INIT') GO TO 100
C
      IF(OURECI.EQ.NREC)   GO TO 10    !Tst for positioned for write
      IF(OURECI.EQ.NREC-1) GO TO 10    !Tst for positioned for write
      IF(OURECI.EQ.NREC-2) GO TO 10    !Tst for positioned for write
C
      GO TO 1110
C
   10 IF(MODE.EQ.'DATA') GO TO 200
      IF(MODE.EQ.'HEAD') GO TO 300
      IF(MODE.EQ.'PAC ') GO TO 400
      IF(MODE.EQ.'SCAL') GO TO 500
      IF(MODE.EQ.'DEAD') GO TO 600
      IF(MODE.EQ.'EOF ') GO TO 700
      IF(MODE.EQ.'CLOS') GO TO 800
      IF(MODE.EQ.'OPEN') GO TO 900
      RETURN
C
C     ------------------------------------------------------------------
C     Initialize directory record
C     ------------------------------------------------------------------
C     Directory record structure:
C     ---------------------------
C     1 - KIND     = 'DIR '
C     2 - NFW      =  8192 = Full-word length of OUDIR
C     3 - OUDIR(1) =  Full-word blocksize = 8194 = 32776 bytes
C     4 - OUDIR(2) =  Number of records written on file
C     5 - OUDIR(3) =  Unused 
C     6 - OUDIR(4) =  Number of header records written on file
C
C     7 - OUDIR(5) =  DIR(1,1) = Header-number-1
C     8 - OUDIR(6) =  DIR(2,1) = Record-number for header-number-1
C
C     OURECI       =  Record number just-written or just-read
C     ------------------------------------------------------------------
C
  100 DO 110 I=1,NFWD
      OUDIR(I)=0
  110 CONTINUE
      NREC=1
      FWRECL=8194
      KIND='DIR '
      WRITE(LU,REC=NREC,IOSTAT=IOS)KIND,NFWD,OUDIR
      IF(IOS.NE.0) GO TO 1010
      LMODE='INIT'
      OURECI=NREC
      RETURN
C
C     ------------------------------------------------------------------
C     Output data-record
C     ------------------------------------------------------------------
C
  200 IF(OURECI.LT.NREC) NREC=OURECI
      NREC=NREC+1
      KIND='DATA'
      WRITE(LU,REC=NREC,IOSTAT=IOS)KIND,NFW,(IBUF(I),I=1,NFW)
      IF(IOS.NE.0) GO TO 1020
      LMODE='DATA'
      OURECI=NREC
      RETURN
C
C     ------------------------------------------------------------------
C     Output header-record  &  Update directory-record
C     ------------------------------------------------------------------
C
  300 HEDN=IBUF(33)
      IF(OURECI.LT.NREC) NREC=OURECI
      NREC=NREC+1
      NHED=NHED+1
      DIR(1,NHED)=HEDN
      DIR(2,NHED)=NREC
C
      KIND='HEAD'
      WRITE(LU,REC=NREC,IOSTAT=IOS)KIND,NFW,(IBUF(I),I=1,NFW)
      IF(IOS.NE.0) GO TO 1040
      LMODE='HEAD'
C
      KIND='DIR '
      WRITE(LU,REC=   1,IOSTAT=IOS)KIND,NFWD,OUDIR
      IF(IOS.NE.0) GO TO 1030
      OURECI=NREC
      RETURN
C
C     ------------------------------------------------------------------
C     Accumulate/Output PAC-records
C     ------------------------------------------------------------------
C
  400 IF(LMODE.EQ.'PAC ') GO TO 420
      NPAC=0
      DO 410 I=1,NFWD
      COBUF(I)='    '
  410 CONTINUE
C
  420 DO 430 I=1,NFW
      NPAC=NPAC+1
      OBUF(NPAC)=IBUF(I)
  430 CONTINUE
      LMODE='PAC '
      IF(NPAC.LT.7792) RETURN
      IF(OURECI.LT.NREC) NREC=OURECI
      NREC=NREC+1
      KIND='PAC '
      WRITE(LU,REC=NREC,IOSTAT=IOS)KIND,NPAC,(OBUF(I),I=1,NPAC)
      IF(IOS.NE.0) GO TO 1050
      LMODE='    '
      NPAC=0
      OURECI=NREC
      RETURN
C
C     ------------------------------------------------------------------
C     Output scaler-record
C     ------------------------------------------------------------------
C
  500 IF(OURECI.LT.NREC) NREC=OURECI
      NREC=NREC+1
      KIND='SCAL'
      WRITE(LU,REC=NREC,IOSTAT=IOS)KIND,NFW,(IBUF(I),I=1,NFW)
      IF(IOS.NE.0) GO TO 1060
      LMODE='SCAL'
      OURECI=NREC
      RETURN
C
C     ------------------------------------------------------------------
C     Output deadtime-record
C     ------------------------------------------------------------------
C
  600 IF(OURECI.LT.NREC) NREC=OURECI
      NREC=NREC+1
      KIND='DEAD'
      WRITE(LU,REC=NREC,IOSTAT=IOS)KIND,NFW,(IBUF(I),I=1,NFW)
      IF(IOS.NE.0) GO TO 1070
      LMODE='DEAD'
      OURECI=NREC
      RETURN
C
C     ------------------------------------------------------------------
C     Output EOF-record
C     ------------------------------------------------------------------
C
  700 IF(OURECI.LT.NREC) NREC=OURECI
      NREC=NREC+1
      DO 710 I=1,NFWD
      COBUF(I)='EOF '
  710 CONTINUE
      KIND='EOF '
      WRITE(LU,REC=NREC,IOSTAT=IOS)KIND,NFWD,OBUF
      IF(IOS.NE.0) GO TO 1080
      LMODE='EOF '
      OURECI=NREC
C
      KIND='DIR '
      WRITE(LU,REC=1,IOSTAT=IOS)KIND,NFWD,OUDIR
      IF(IOS.NE.0) GO TO 1010
      RETURN
C
C     ------------------------------------------------------------------
C     Process CLOS request - final output of Directory-record
C     ------------------------------------------------------------------
C
  800 KIND='DIR '
      WRITE(LU,REC=1,IOSTAT=IOS)KIND,NFWD,OUDIR
      IF(IOS.NE.0) GO TO 1010
      LMODE='CLOS'
      RETURN
C
C
C     ------------------------------------------------------------------
C     Process OPEN request - Read in Directory-record
C     ------------------------------------------------------------------
C
  900 READ(LU,REC=1,IOSTAT=IOS)KIND,NFWX,OUDIR
      IF(IOS.NE.0) GO TO 1090
C
      OURECI=NREC
      NMAR=0
C
      DO 910 NT=NREC-1,NREC
      READ(LU,REC=NT,IOSTAT=IOS)KIND
      IF(IOS.NE.0) GO TO 1100
      IF(KIND.EQ.'EOF ') THEN
      WRITE(CMSSG,915)NT
      CALL MESSLOG(LOGUT,LOGUP)
      NMAR=NMAR+1
      ENDIF
  910 CONTINUE
C
  915 FORMAT('EOF detected at record#',I8)
C
      IF(NMAR.EQ.2) OURECI=NREC-1
C
      WRITE(CMSSG,920)OURECI+1
C
  920 FORMAT('Next record to be written is record#',I8)
      CALL MESSLOG(LOGUT,LOGUP)
C
      LMODE='OPEN'
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)NREC
 1005 FORMAT('Error writing record#',I8,'  PAC-record')
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)NREC
 1015 FORMAT('Error writing record#',I8,'  DIRECTORY-record')
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)NREC
 1025 FORMAT('Error writing record#',I8,'  DATA-record')
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)NREC
 1035 FORMAT('Error writing record#',I8,'  DIRECTORY-record')
      GO TO 2000
C
 1040 WRITE(CMSSG,1045)NREC
 1045 FORMAT('Error writing record#',I8,'  HEADER-record')
      GO TO 2000
C
 1050 WRITE(CMSSG,1055)NREC
 1055 FORMAT('Error writing record#',I8,'  PAC-record')
      GO TO 2000
C
 1060 WRITE(CMSSG,1065)NREC
 1065 FORMAT('Error writing record#',I8,'  SCALER-record')
      GO TO 2000
C
 1070 WRITE(CMSSG,1075)NREC
 1075 FORMAT('Error writing record#',I8,'  DEAD-TIME-record')
      GO TO 2000
C
 1080 WRITE(CMSSG,1085)NREC
 1085 FORMAT('Error writing record#',I8,'  EOF-record')
      GO TO 2000
C
 1090 WRITE(CMSSG,1095)
 1095 FORMAT('Error reading in DIRECTORY-record')
      GO TO 2000
C
 1100 WRITE(CMSSG,1105)NT
 1105 FORMAT('Error reading terminating file-mark record# =',I8)
      GO TO 2000
C
 1110 WRITE(CMSSG,1115)
 1115 FORMAT('File-pointer not properly positioned for write')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
