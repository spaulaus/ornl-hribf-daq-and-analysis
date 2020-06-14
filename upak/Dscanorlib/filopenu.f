C$PROG FILOPENU  - Opens his- & drr-files & memory seg for SCANU mode
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 02/17/99
C     ******************************************************************
C
      SUBROUTINE FILOPENU
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
      COMMON/SC02/ NAMH(20)
      INTEGER*4    NAMH
C     ------------------------------------------------------------------
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC25/ CNAMS                   !CNAMS contains SHM filename
      CHARACTER*80 CNAMS
C     ------------------------------------------------------------------
      CHARACTER*80 CNAMH,CNAMD
C
      INTEGER*4    NAMD(20), NAMS(20)
C
      EQUIVALENCE (CNAMH,NAMH), (CNAMD,NAMD), (CNAMS,NAMS)
C
      INTEGER*4    IERRNO,LSNB,IERR,LN,JB,STAT,LUH
C
      INTEGER*4    RECLVALU
C
      CHARACTER*4  IEXT
C
      SAVE
C
C     ==================================================================
C     ROUTINE TO OPEN - HIS-FILE FOR HIS_READ (C ROUTINE IN UNIX VERSION)
C                     - DRR-FILE FOR FORTRAN READ
C     ==================================================================
C     SET UP NAMES ETC.
C     ==================================================================
C
      IERR=0
      CNAMD=CNAMH
c     CNAMS=CNAMH
C
      LN=LSNB(NAMH,1,80)                !FIND LAST NON-BLANK
      IF(LN.LE.0) GO TO 530             !ERROR IF NOT FOUND
      IF(LN.GT.76) GO TO 530            !TST FOR TOO LONG
      IEXT='.his'                       !HIS-FILE EXTENSION
      CALL LODUP(IEXT,1,4,NAMH,LN+1)    !HIS-FILE FULL NAME
      IEXT='.drr'                       !DRR-FILE EXTENSION
      CALL LODUP(IEXT,1,4,NAMD,LN+1)    !DRR-FILE FULL NAME
c     IEXT='.shm'                       !Shared Memory id-FILE EXTENSION
c     CALL LODUP(IEXT,1,4,NAMS,LN+1)    !Shared Memory id-FILE FULL NAME
      JB=LN+4                           !LAST BYTE# OF FILENAME
      CALL ISBYTE(0,NAMD,JB)            !STUFF IN A NULL
      CALL ISBYTE(0,NAMH,JB)            !STUFF IN A NULL
c     CALL ISBYTE(0,NAMS,JB)            !STUFF IN A NULL
C
C     ==================================================================
C     OPEN the shared memory ID-FILE
C     ==================================================================
C
*     OPEN(UNIT       = 21,
*    &     FILE       = CNAMS,
*    &     STATUS     = 'UNKNOWN',
*    &     ACCESS     = 'SEQUENTIAL',
*    &     IOSTAT     = STAT)
C
*     IF(STAT.NE.0) GO TO 610
C
C     ==================================================================
C     OPEN THE DRR-FILE 
C     ==================================================================
C
      OPEN(UNIT       = LUC(9),
     &     FILE       = CNAMD,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     FORM       = 'UNFORMATTED',
     &     RECL       = RECLVALU(128),
     &     IOSTAT     = STAT)
C
       IF(STAT.NE.0) GO TO 620
C
C     ==================================================================
C     NOW OPEN THE HIS-FILE
C     ==================================================================
C
      CALL HIS_OPENRW(NAMH,LUH)
C
      IF(LUH.GE.0) THEN                 !Test for existing HIS-file
      LUC(6)=LUH                        !Store chan# in LUC(6)
      LUC(7)=LUH                        !Flag for existing HIS-file
      RETURN
      ENDIF
C
      IERR=IERRNO()
C
      IF(IERR.NE.2) GO TO 550           !Error other than "not found"
C
      CALL DINGER(2)
      WRITE(CMSSG,100)
      CALL MESSLOG(LOGUT,LOGUP)
  100 FORMAT('Specified his-file not found - It will be created')
C
      CALL SYS_CREATE(NAMH,LUH)
C
      IF(LUH.LT.0) GO TO 560             !Test for error
C
      LUC(6)=LUH                         !Store chan# in LUC(6)
      LUC(7)=555                         !Flag to indicate new HIS-file
C
      RETURN
C
C     ==================================================================
C     Send error messages & exit
C     ==================================================================
C
  530 WRITE(CMSSG,535)
  535 FORMAT('Syntax error in HIS-file specification')
      CALL MESSLOG(LOGUT,LOGUP)
      CALL EXIT(1)
C
  550 IERR=IERRNO()
      WRITE(CMSSG,555)IERR
  555 FORMAT('Error trying to open HIS-file - stat  = ',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL EXIT(1)
C
  560 IERR=IERRNO()
      WRITE(CMSSG,565)IERR
  565 FORMAT('Error trying to create a new HIS-file - stat =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL EXIT(1)
C
  610 WRITE(CMSSG,615)STAT
  615 FORMAT('Error trying to open SHM-ID-file      - stat = ',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL EXIT(1)
C
  620 WRITE(CMSSG,625)STAT
  625 FORMAT('Error trying to open DRR-file - stat = ',I8)
      CALL EXIT(1)
C
      END
