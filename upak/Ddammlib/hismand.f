C$PROG HISMAND   - Processes OPEN/CLOSE requests of HIS & SHM for DAMM
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/31/2005 - for gnu
C     ******************************************************************
C
      SUBROUTINE HISMAND(MODE,NAMF,LUD,LUH,ACP,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C     Shared memory information.  (20) allows indexing by WTM units
C
      LOGICAL SHMFLG
      INTEGER SHMID
      LOGICAL SHMUSE
      COMMON /SharedMem/ SHMID(20), SHMFLG(20), SHMUSE
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
      character*80 cnamfil(20)
      equivalence (cnamfil, namfil)
C     ------------------------------------------------------------------
      COMMON/DML8/  NAMH(20)
      INTEGER*4     NAMH
C
      CHARACTER*80  CNAMH
      EQUIVALENCE  (CNAMH,NAMH)
C     ------------------------------------------------------------------
      CHARACTER*4  MODE,ACP
C
      CHARACTER*4  ANS
C
      CHARACTER*4  CKEXT,CIEXT
      INTEGER*4     KEXT, IEXT
      EQUIVALENCE (CKEXT,KEXT),(CIEXT,IEXT)
C
cmm    INTEGER*4    NAMD(20),NAMF(20),NAMS(20)
      INTEGER*4    NAMD(20),NAMF(20)
C
      INTEGER*4    STRLEN, RECLVALU
C
cmm     CHARACTER*80 CNAMD, CNAMS
      CHARACTER*80 CNAMD
C
cmm     EQUIVALENCE  (NAMD,CNAMD),(NAMS,CNAMS)
      EQUIVALENCE  (NAMD,CNAMD)
C
      SAVE
C
C     ------------------------------------------------------------------
C     PROCESS -filnam.HIS       & OPEN HIS-FILE FOR QIO     READ
C                               & OPEN DRR-FILE FOR FORTRAN READ
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(MODE.EQ.'OPEN') GO TO 50
C
C     ==================================================================
C     PROCESS CLOSE REQUEST
C     ==================================================================
C
      CLOSE(UNIT=LUD)                    !CLOSE DRR-FILE
C
      cnamfil(luh) = ' '                 !Clear the file name

      IF (SHMFLG(LUD)) THEN              !If shared memory
cmm   CALL SHM_CLOSE(SHMID(LUD),IERR)
      CALL MMAP_CLOSE(IERR)              ! Use memory-mapped i/o
      SHMFLG(LUD)=.FALSE.
      SHMUSE=.FALSE.
      LUH=-1
      RETURN
      ENDIF
C
      IF(LUH.LT.0) RETURN                !TST FOR HIS-CHAN OPEN
C
      CALL SYS_CLOSE(LUH)
C
      LUH=-1                             !SET HIS-FILE CLOSED
      RETURN
C
C     ==================================================================
C     PROCESS OPEN REQUEST
C     ==================================================================
C
   50 DO 60 I=1,20                       !SAVE FILNAME FOR MODS
      NAMH(I)=NAMF(I)
      NAMD(I)=NAMF(I)
cmm   NAMS(I)=NAMF(I)
   60 CONTINUE
      LD=INDEX(CNAMH,'.')                !LOCATE .EXT
      CKEXT='    '
      CALL LODUP(NAMH,LD,LD+3,KEXT,1)    !GET HIS-FILE EXT
      CIEXT='.DRR'
      IF(CKEXT.EQ.'.his') CIEXT='.drr'
      CALL LODUP(IEXT,1,4,NAMD,LD)       !INSERT .DRR-EXTENSION
c     CIEXT = '.shm'
c     CALL LODUP(IEXT,1,4,NAMS,LD)       !INSERT .shm-EXTENSION
      JB=LD+3
      CALL ISBYTE(0,NAMD,JB)             !STUFF IN A NULL
      CALL ISBYTE(0,NAMH,JB)             !STUFF IN A NULL
c     CALL ISBYTE(0,NAMS,JB)             !STUFF IN A NULL
C
C     ------------------------------------------------------------------
C     Try to open the his-file
C     ------------------------------------------------------------------
C
c    Inserted from hisman with memory mapped i/o
  300 IF(ACP.EQ.'IN  ')THEN
         CALL HIS_OPENRW(NAMH,LUH)
         IF(LUH.GE.0) THEN     ! There is a his file to read
C
C     -----------------------------------------------------------------
C     Now do the memory map, if no one else is already mapped.
C     -----------------------------------------------------------------
C
            if (.not. shmuse) then ! Only one is possible
               WRITE(CMSSG,160) CNAMH
 160           FORMAT('Memory Map the HIS file Segment:',A80)
               CALL MESSLOG(LOGUT,LOGUP)
         
c           Map the memory, with write access
               CALL MMAP_HISSPACE(LUH, 1, IERR) !C-routine to attach
         
               IF (IERR.EQ.0) THEN
                  SHMFLG(LUD)=.TRUE. !Let everyone know we're ok
                  SHMUSE     =.TRUE.
                  SHMID(LUD)=SHMID(LUD)+1  ! Let zotman know we've changed
               else     
                  SHMFLG(LUD)=.FALSE. !Not ok
                  SHMUSE=.FALSE.
                  IERR=0        !Just use the his-file instead
C     
                  WRITE(CMSSG,180) CNAMH
 180              FORMAT('Unable to memory map file ', A80)
                  CALL MESSLOG(LOGUT,LOGUP)
C     
                  WRITE(CMSSG,200)
 200              FORMAT('Using HIS file instead.')
                  CALL MESSLOG(LOGUT,LOGUP)
C     
               endif
            endif
         ELSE
            GOTO 1040                       !Error opening his file
         ENDIF
C
      ELSE IF(ACP.EQ.'RO  ')THEN
         CALL HIS_OPENRW(NAMH,LUH)
         IF(LUH.LT.0) THEN     ! There is no his file to read
            GOTO 1040
         ENDIF
      ELSE 
         CALL HIS_OPENRW(NAMH,LUH)
         IF (LUH .LT. 0) THEN  !no his file to open
C    New feature of DAMM, creation of HIS and DRR if none exist
            CALL DINGER(3)                     !Require confirmation
            WRITE(6,310)CNAMH(1:STRLEN(CNAMH)) !before you create a new one
 310        FORMAT(1H ,A,' does not exist')
            WRITE(6,315)
 315        FORMAT(1H ,'Shall I create it?')
            CALL YESNO(ANS)
            IF(ANS.NE.'YES ') GO TO 1060
C
            CALL NEWHIS(CNAMH,CNAMD,LUH,LUD,IERR) !Try to create HIS & DRR 
            IF(IERR.NE.0) GO TO 1050 !Test for success
            RETURN
         ENDIF
      ENDIF
C
C     ==================================================================
C     OPEN THE DRR-FILE
C     ==================================================================
C
  500 CONTINUE
C
      OPEN(UNIT       = LUD,
     &     FILE       = CNAMD,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(128),
     &     IOSTAT     = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 1000
                    ENDIF
C
      RETURN
C
C     ==================================================================
C     Return error message
C     ==================================================================
C
 1000 WRITE(CMSSG,1005)STAT
 1005 FORMAT('Error trying to open DRR-file - zstat =',Z8)
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)CNAMH(1:STRLEN(CNAMH))
 1015 FORMAT(A,' is open for scanning - writing from DAMM not allowed')
      LUH=-1
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)CNAMH(1:STRLEN(CNAMH))
 1035 FORMAT(A,' is open for scanning & SHM segment already in use')
      LUH=-1
      GO TO 2000
C
 1040 WRITE(CMSSG,1045)
 1045 FORMAT('Error trying to open HIS-file or file not found')
      LUH=-1
      GO TO 2000
C
 1050 WRITE(CMSSG,1055)
 1055 FORMAT('Error trying to open or create output HIS- & DRR-files')
      LUH=-1
      GO TO 2000
C
 1060 WRITE(CMSSG,1065)
 1065 FORMAT('New histogram file not created')
      LUH=-1
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C
      END
