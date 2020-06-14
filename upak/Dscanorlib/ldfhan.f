C$PROG LDFHAN    - Manages list-data file position pointers for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/26/99
C     ******************************************************************
C
      SUBROUTINE LDFHAN(IERR)
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
      COMMON/SC15/ NCEOF,LAUTO
      INTEGER*4    NCEOF
      CHARACTER*4        LAUTO
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      CHARACTER*4  KMD,FLAG
      INTEGER*4    IERR
      EQUIVALENCE (KMD,LWD(1,1))
C     ------------------------------------------------------------------
      INTEGER*4    IBUF(8192),KIND,NFW,NREC,IDN,IDTST,NDO,I,N
C
      INTEGER*4    STAT,NV,NUM,MXREC,MXHED,NEOF
C
      REAL*4       XV
C     ------------------------------------------------------------------
      INTEGER*4    DIRI(2,4000)
C
      EQUIVALENCE (DIRI(1,1),INDIR(5))
C
      SAVE
C
C     ------------------------------------------------------------------
C     LUINF    =  Logical unit# for input/output LDF-file
C
C     INTYP    = '    ', TAPE, FILE = input type
C
C     INRECI   =  Current record# pointer for input file
C
C     INDIR(1) =  Full-word blocksize = 8194 = 32776 bytes
C     INDIR(2) =  Number of records written on file
C     INDIR(3) =  Unused 
C     INDIR(4) =  Number of header records written on file
C
C     INDIR(5) =  DIR(1,1) = Header-number-1
C     INDIR(6) =  DIR(2,1) = Record-number for header-number-1
C     ------------------------------------------------------------------
C
      IERR=0
C
      CALL MILV(LWD(1,2),NV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      NUM=NV
      IF(NUM.LE.0) NUM=1
C
      IF(INTYP.NE.'LDF ') GO TO 1000
C
      MXREC=INDIR(2)
      MXHED=INDIR(4)
C
      IF(KMD.EQ.'REW ') GO TO 100
      IF(KMD.EQ.'BR  ') GO TO 150
      IF(KMD.EQ.'FR  ') GO TO 200
      IF(KMD.EQ.'FF  ') GO TO 300
      IF(KMD.EQ.'BF  ') GO TO 350
      IF(KMD.EQ.'FIND') GO TO 400
C
      GO TO 1000
C
C     ------------------------------------------------------------------
C     Process REW  -  Rewind input file
C     ------------------------------------------------------------------
C
  100 INRECI=2
      NCEOF=0
      RETURN
C
C     ------------------------------------------------------------------
C     Process BR   -  Backup record/s on input file
C     ------------------------------------------------------------------
C
  150 DO 160 N=1,NUM
      INRECI=INRECI-1
      IF(INRECI.LT.2) THEN
      INRECI=2
      RETURN
      ENDIF
  160 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Process FR   -  Forward record/s on input file
C     ------------------------------------------------------------------
C
  200 DO 210 N=1,NUM
      INRECI=INRECI+1
      IF(INRECI.GT.MXREC) THEN
      INRECI=MXREC
      RETURN
      ENDIF
  210 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Process FF   -  Forward file/s on input file
C     ------------------------------------------------------------------
C
  300 N=0
      DO 310 I=1,MXHED
      N=N+1
      IF(DIRI(2,I).GT.INRECI) GO TO 320
  310 CONTINUE
      INRECI=MXREC
      WRITE(CMSSG,315)MXREC
  315 FORMAT('Positioned at last EOF record =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
  320 N=N+NUM-1
      IF(N.GT.MXHED) THEN
      INRECI=MXREC
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
C     Process BF   -  Backup file/s on input file
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
      INRECI=DIRI(2,N)
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
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 IERR=1
      RETURN
      END
