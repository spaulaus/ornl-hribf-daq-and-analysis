C$PROG NEWHIS    - Creates, Opens & Initializes new His- & DRR-files
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/17/2004 - for gnu
C     ******************************************************************
C
      SUBROUTINE NEWHIS(CNAMH,CNAMD,LUH,LUD,IERR)
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
      CHARACTER*80 CNAMH,CNAMD
C
      INTEGER*4    LUH,LUD,IERR,STAT,RECLVALU
C
      INTEGER*4    IDIRF(32),DATIM(6),IREC,IOS,I
C
      INTEGER*4    HHIR,FDIR,I0001,BLANK
      character*4  cHHIR,cFDIR,cI0001,cBLANK
      equivalence  (cHHIR,HHIR),(cFDIR,FDIR),
     &             (cI0001,I0001),(cBLANK,BLANK)
      DATA     cHHIR,cFDIR,cI0001,cBLANK/'HHIR','FDIR','0001','    '/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
C     ==================================================================
C     Create/open a new his-file
C     ==================================================================
C
      CALL SYS_CREATE(CNAMH,LUH)
C
      IF(LUH.LT.0) GO TO 1000
C
      CALL HIS_OPENRW(CNAMH,LUH)
C
      IF(LUH.LT.0) GO TO 1000
C
C     ==================================================================
C     Open OLD or create NEW DRR-file
C     ==================================================================
C
      OPEN(UNIT       = LUD,
     &     FILE       = CNAMD,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(128),
     &     IOSTAT     = STAT)
C
      IF(STAT.EQ.0) RETURN
C
      OPEN(UNIT       = LUD,
     &     FILE       = CNAMD,
     &     STATUS     = 'NEW',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(128),
     &     IOSTAT     = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 1000
                    ENDIF
C
C     ==================================================================
C     Setup and output first record of DRR-file
C     ==================================================================
C
      IDIRF(1)=HHIR
      IDIRF(2)=FDIR
      IDIRF(3)=I0001
      IDIRF(4)=0                            !# OF ID'S ON FILE
      IDIRF(5)=0                            !# OF HALF-WDS ON FILE
      IDIRF(6)=0                            !NOT USED
C
      CALL MILYMDHMS(DATIM)                 !Get Y,M,D,H,M,S
C
      DO 50 I=1,6
      IDIRF(I+6)=DATIM(I)                   !Save in header
   50 CONTINUE
C
      DO 60 I=1,20                          !20 WDS OF "TEXT"
      IDIRF(I+12)=BLANK
   60 CONTINUE
C
      IREC=1                                !LOCATE FIRST RECORD
      WRITE(LUD,REC=IREC,IOSTAT=IOS)IDIRF   !OUTPUT FIRST RECORD
      CALL IOERR(IOS)
      IF(IOS.NE.0) GO TO 100                !TST FOR ERROR
      RETURN
C
C     ==================================================================
C     Return error messages
C     ==================================================================
C
  100 WRITE(CMSSG,105)
  105 FORMAT('Error trying to initialize DRR-file')
      CALL MESSLOG(LOGUT,LOGUP)
C
 1000 IERR=1
      RETURN
      END
