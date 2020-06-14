C$PROG LDFMAN    - Manages list-data file position pointers
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE LDFMAN(IERR)
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
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      INTEGER*4    IERR
C
      CHARACTER*4  KMD,STAT,FLAG
C
      EQUIVALENCE (KMD,LWD(1,1))
C     ------------------------------------------------------------------
      INTEGER*4    IBUF(8192),KIND,NFW,NREC,IDN,IDTST,NDO,I,N
C
      INTEGER*4    NV,NUM,MXREC,MXHED,NEOF
C
      REAL*4       XV
C     ------------------------------------------------------------------
      INTEGER*4    DIRI(2,4000),DIRO(2,4000)
C
      EQUIVALENCE (DIRI(1,1),INDIR(5)),
     &            (DIRO(1,1),OUDIR(5))
C
      SAVE
C
C     ------------------------------------------------------------------
C     LUINF/LUOUF       =  Logical unit# for input/output LDF-file
C     INFOP/OUFOP       =  YES/NO        for input/output LDF-file open
C
C     INTYP             = '    ', TAPE, FILE, or EVEL = input type
C     OUTYP             = '    ', TAPE, FILE          = output type
C
C     INRECI            =  last record# read        from input file
C     OURECI            =  last record# read/written on output file
C
C     INDIR(1)/OUDIR(1) =  Full-word blocksize = 8194 = 32776 bytes
C     INDIR(2)/OUDIR(2) =  Number of records written on file
C     INDIR(3)/OUDIR(3) =  Unused 
C     INDIR(4)/OUDIR(4) =  Number of header records written on file
C
C     INDIR(5)/OUDIR(5) =  DIR(1,1) = Header-number-1
C     INDIR(6)/OUDIR(6) =  DIR(2,1) = Record-number for header-number-1
C     ------------------------------------------------------------------
C
      IERR=0
C
      CALL MILV(LWD(1,2),NV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      NUM=NV
      IF(NUM.LE.0) NUM=1
C
      IF(KMD.EQ.'RWI ') GO TO 50
      IF(KMD.EQ.'BRI ') GO TO 50
      IF(KMD.EQ.'FRI ') GO TO 50
      IF(KMD.EQ.'BTI ') GO TO 50
      IF(KMD.EQ.'FFI ') GO TO 50
      IF(KMD.EQ.'BFI ') GO TO 50
      IF(KMD.EQ.'FIND') GO TO 50
      IF(KMD.EQ.'DTIT') GO TO 50
C
      IF(KMD.EQ.'RWO ') GO TO 60
      IF(KMD.EQ.'BRO ') GO TO 60
      IF(KMD.EQ.'FRO ') GO TO 60
      IF(KMD.EQ.'BTO ') GO TO 60
      IF(KMD.EQ.'FFO ') GO TO 60
      IF(KMD.EQ.'BFO ') GO TO 60
C
      GO TO 1010
C
   50 IF(INTYP.NE.'LDF ') GO TO 1020
      MXREC=INDIR(2)
      IF(MXREC.LE.1)      GO TO 1030
      MXHED=INDIR(4)
      GO TO 70
C
   60 IF(OUTYP.NE.'LDF ') GO TO 1020
      MXREC=OUDIR(2)
      IF(MXREC.LE.1)      GO TO 1040
      MXHED=OUDIR(4)
      GO TO 70
C
   70 CONTINUE
C
      IF(KMD.EQ.'RWI ') GO TO 100
      IF(KMD.EQ.'BRI ') GO TO 150
      IF(KMD.EQ.'FRI ') GO TO 200
      IF(KMD.EQ.'BTI ') GO TO 250
      IF(KMD.EQ.'FFI ') GO TO 300
      IF(KMD.EQ.'BFI ') GO TO 350
      IF(KMD.EQ.'FIND') GO TO 400
      IF(KMD.EQ.'DTIT') GO TO 450
C
      IF(KMD.EQ.'RWO ') GO TO 500
      IF(KMD.EQ.'BRO ') GO TO 550
      IF(KMD.EQ.'FRO ') GO TO 600
      IF(KMD.EQ.'BTO ') GO TO 650
      IF(KMD.EQ.'FFO ') GO TO 700
      IF(KMD.EQ.'BFO ') GO TO 750
C
      GO TO 1010
C
C     ------------------------------------------------------------------
C     Process RWI  -  Rewind input file
C     ------------------------------------------------------------------
C
  100 INRECI=1
      RETURN
C
C     ------------------------------------------------------------------
C     Process BRI  -  Backup record/s on input file
C     ------------------------------------------------------------------
C
  150 DO 160 N=1,NUM
      INRECI=INRECI-1
      IF(INRECI.LT.1) THEN
      INRECI=1
      RETURN
      ENDIF
  160 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Process FRI  -  Forward record/s on input file
C     ------------------------------------------------------------------
C
  200 DO 210 N=1,NUM
      INRECI=INRECI+1
      IF(INRECI.GE.MXREC) THEN
      INRECI=MXREC-1
      RETURN
      ENDIF
  210 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Process BTI  -  Go to bottom of input file
C     ------------------------------------------------------------------
C
  250 NDO=MXHED
      DO 260 N=1,NDO
      NREC=DIRI(2,N)
      CALL LDFREAD(LUINF,NREC,IBUF,KIND,NFW,STAT)
      IDN=IBUF(33)
      WRITE(CMSSG,255)(IBUF(I),I=9,23),IDN,N
  255 FORMAT(15A4,' -hed,fil=',I6,I4)
      CALL MESSLOG(LOGUT,LOGUP)
  260 CONTINUE
C
      NEOF=0
      NREC=MXREC-2
      DO 270 I=1,2
      NREC=NREC+1
      CALL LDFREAD(LUINF,NREC,IBUF,KIND,NFW,STAT)
      IF(STAT.EQ.'EOF ') NEOF=NEOF+1
      WRITE(CMSSG,265)KIND,NREC
  265 FORMAT('Record-type - ',A4,'  found at record#',I8)
      CALL MESSLOG(LOGUT,LOGUP)
  270 CONTINUE
      IF(NEOF.EQ.2) THEN
      WRITE(CMSSG,275)
  275 FORMAT('Double End-of-File encountered on input file')
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
      INRECI=MXREC-1
      RETURN
C
C     ------------------------------------------------------------------
C     Process FFI  -  Forward file/s on input file
C     ------------------------------------------------------------------
C
  300 N=0
      DO 310 I=1,MXHED
      N=N+1
      IF(DIRI(2,I).GT.INRECI) GO TO 320
  310 CONTINUE
      INRECI=MXREC-1
      WRITE(CMSSG,315)MXREC
  315 FORMAT('Positioned at last EOF record =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
  320 N=N+NUM-1
      IF(N.GT.MXHED) THEN
      INRECI=MXREC-1
      WRITE(CMSSG,315)MXREC
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      ENDIF
C
      INRECI=DIRI(2,N)
      CALL LDFREAD(LUINF,INRECI,IBUF,KIND,NFW,STAT)
      IDN=IBUF(33)
      WRITE(CMSSG,325)(IBUF(I),I=9,23),IDN,N
  325 FORMAT(15A4,' -hed,fil=',I6,I4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     ------------------------------------------------------------------
C     Process BFI  -  Backup file/s on input file
C     ------------------------------------------------------------------
C
  350 N=0
      DO 360 I=1,MXHED
      N=N+1
      IF(DIRI(2,I).GE.INRECI) GO TO 370
  360 CONTINUE
      N=MXHED+1
C
  370 N=N-NUM
      IF(N.LT.1) N=1
C
      INRECI=DIRI(2,N)
      CALL LDFREAD(LUINF,INRECI,IBUF,KIND,NFW,STAT)
      IDN=IBUF(33)
      WRITE(CMSSG,380)(IBUF(I),I=9,23),IDN,N
  380 FORMAT(15A4,' -hed,fil=',I6,I4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     ------------------------------------------------------------------
C     Process FIND ID  -  Find header# ID on input file
C     ------------------------------------------------------------------
C
  400 IDTST=NV
      FLAG='NO  '
C
      DO 420 N=1,MXHED
C
      IF(DIRI(2,N).LT.INRECI) GO TO 420
      IF(DIRI(1,N).EQ.IDTST)  FLAG='YES '
C
      CALL LDFREAD(LUINF,DIRI(2,N),IBUF,KIND,NFW,STAT)
      IDN=IBUF(33)
      WRITE(CMSSG,405)(IBUF(I),I=9,23),IDN,N,FLAG
  405 FORMAT(15A4,' -hed,fil=',I6,I4,1X,A4)
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(FLAG.EQ.'YES ') THEN
      INRECI=DIRI(2,N)-1
      RETURN
      ENDIF
C
  420 CONTINUE
C
      WRITE(CMSSG,425)IDTST
  425 FORMAT('Header number ',I8,'  not found')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     ------------------------------------------------------------------
C     Process DTIT  -  Find/display next header on input file
C     ------------------------------------------------------------------
C
  450 DO 460 N=1,MXHED
      IF(DIRI(2,N).GE.INRECI) GO TO 470
  460 CONTINUE
      WRITE(CMSSG,465)
  465 FORMAT('No subsequent headers found on input file')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
  470 INRECI=DIRI(2,N)
      CALL LDFREAD(LUINF,INRECI,IBUF,KIND,NFW,STAT)
      IDN=IBUF(33)
      WRITE(CMSSG,475)(IBUF(I),I=9,23),IDN,N
  475 FORMAT(15A4,' -hed,fil=',I6,I4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     ------------------------------------------------------------------
C     Process RWO  -  Rewind output file
C     ------------------------------------------------------------------
C
  500 OURECI=1
      RETURN
C
C     ------------------------------------------------------------------
C     Process BRO  -  Backup record/s on output file
C     ------------------------------------------------------------------
C
  550 DO 560 N=1,NUM
      OURECI=OURECI-1
      IF(OURECI.LT.1) THEN
      OURECI=1
      RETURN
      ENDIF
  560 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Process FRO  -  Forward record/s on output file
C     ------------------------------------------------------------------
C
  600 DO 610 N=1,NUM
      OURECI=OURECI+1
      IF(OURECI.GE.MXREC) THEN
      OURECI=MXREC-1
      RETURN
      ENDIF
  610 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Process BTO  -  Go to bottom of output file
C     ------------------------------------------------------------------
C
  650 NDO= MXHED
      DO 660 N=1,NDO
      NREC=DIRO(2,N)
      CALL LDFREAD(LUOUF,NREC,IBUF,KIND,NFW,STAT)
      IDN=IBUF(33)
      WRITE(CMSSG,655)(IBUF(I),I=9,23),IDN,N
  655 FORMAT(15A4,' -hed,fil=',I6,I4)
      CALL MESSLOG(LOGUT,LOGUP)
  660 CONTINUE
C
      NEOF=0
      NREC=MXREC-2
      DO 670 I=1,2
      NREC=NREC+1
      CALL LDFREAD(LUOUF,NREC,IBUF,KIND,NFW,STAT)
      IF(STAT.EQ.'EOF ') NEOF=NEOF+1
      WRITE(CMSSG,665)KIND,NREC
  665 FORMAT('Record-type - ',A4,'  found at record#',I8)
      CALL MESSLOG(LOGUT,LOGUP)
  670 CONTINUE
      IF(NEOF.EQ.2) THEN
      WRITE(CMSSG,675)
  675 FORMAT('Double End-of-File encountered on output file')
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
      OURECI=MXREC-1
      RETURN
C
C     ------------------------------------------------------------------
C     Process FFO  -  Forward file/s on output file
C     ------------------------------------------------------------------
C
  700 N=0
      DO 710 I=1,MXHED
      N=N+1
      IF(DIRO(2,I).GT.OURECI) GO TO 720
  710 CONTINUE
      OURECI=MXREC-1
      WRITE(CMSSG,715)MXREC
  715 FORMAT('Positioned at last EOF record =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
  720 N=N+NUM-1
      IF(N.GT.MXHED) THEN
      OURECI=MXREC-1
      WRITE(CMSSG,715)MXREC
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      ENDIF
C
      OURECI=DIRO(2,N)
      CALL LDFREAD(LUOUF,OURECI,IBUF,KIND,NFW,STAT)
      IDN=IBUF(33)
      WRITE(CMSSG,725)(IBUF(I),I=9,23),IDN,N
  725 FORMAT(15A4,' -hed,fil=',I6,I4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     ------------------------------------------------------------------
C     Process BFO  -  Backup file/s on output file
C     ------------------------------------------------------------------
C
  750 N=0
      DO 760 I=1,MXHED
      N=N+1
      IF(DIRO(2,I).GE.OURECI) GO TO 770
  760 CONTINUE
      N=MXHED+1
C
  770 N=N-NUM
      IF(N.LT.1) N=1
C
      OURECI=DIRO(2,N)
      CALL LDFREAD(LUOUF,OURECI,IBUF,KIND,NFW,STAT)
      IDN=IBUF(33)
      WRITE(CMSSG,780)(IBUF(I),I=9,23),IDN,N
  780 FORMAT(15A4,' -hed,fil=',I6,I4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Syntax error or illegal command - ignored')
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('Command not supported for ldf-files - ignored')
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Illegal file type - should never get here - cmd ignored')
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('Input file is virgin - command ignored')
      GO TO 2000
C
 1040 WRITE(CMSSG,1045)
 1045 FORMAT('Output file is virgin - command ignored')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
