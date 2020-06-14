C$PROG HISMAN    - Processes requests for open/close of his & SHM
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE HISMAN(MODE,NAMF,LUD,LUH,ACP,IERR)
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
      CHARACTER*4  MODE,ACP
C
      CHARACTER*4  CKEXT,CIEXT
      INTEGER*4     KEXT, IEXT
      EQUIVALENCE (CKEXT,KEXT),(CIEXT,IEXT)
C
C      INTEGER*4    NAMH(20),NAMD(20),NAMF(20),NAMS(20)
      INTEGER*4    NAMH(20),NAMD(20),NAMF(20)
C
      INTEGER*4    STRLEN, RECLVALU
C
C      CHARACTER*80 CNAMH,CNAMD, CNAMS
      CHARACTER*80 CNAMH,CNAMD
C
C      EQUIVALENCE  (NAMH,CNAMH),(NAMD,CNAMD),(NAMS,CNAMS)
      EQUIVALENCE  (NAMH,CNAMH),(NAMD,CNAMD)
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
      IF (SHMFLG(LUD)) THEN              !If shared memory

*      CALL SHM_CLOSE(SHMID(LUD),IERR)
      CALL MMAP_CLOSE(IERR)

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
C      NAMS(I)=NAMF(I)
   60 CONTINUE
      LD=INDEX(CNAMH,'.')                !LOCATE .EXT
      CKEXT='    '
      CALL LODUP(NAMH,LD,LD+3,KEXT,1)    !GET HIS-FILE EXT
      CIEXT='.DRR'
      IF(CKEXT.EQ.'.his') CIEXT='.drr'
      CALL LODUP(IEXT,1,4,NAMD,LD)       !INSERT .DRR-EXTENSION
C      CIEXT = '.shm'
C      CALL LODUP(IEXT,1,4,NAMS,LD)       !INSERT .shm-EXTENSION
      JB=LD+3
      CALL ISBYTE(0,NAMD,JB)             !STUFF IN A NULL
      CALL ISBYTE(0,NAMH,JB)             !STUFF IN A NULL
C      CALL ISBYTE(0,NAMS,JB)             !STUFF IN A NULL
C
C     ------------------------------------------------------------------
C     Try to open the his-file
C     ------------------------------------------------------------------
C
  300 IF(ACP.NE.'RW  ')THEN
         CALL HIS_OPENRO(NAMH,LUH)
         IF (LUH .GE.0) THEN
C
C     -----------------------------------------------------------------
C     Now do the memory map, if no one else is already mapped.
C     -----------------------------------------------------------------
C
            if (.not. shmuse) then ! Only one is possible
C              WRITE(CMSSG,160) CNAMH
C160           FORMAT('Memory Map the HIS file Segment:',A80)
C              CALL MESSLOG(LOGUT,LOGUP)
         
               CALL MMAP_HISSPACE(LUH, 0, IERR) !C-routine to attach
         
               IF (IERR.EQ.0) THEN
                  SHMFLG(LUD)=.TRUE. !Let everyone know we're ok
                  SHMUSE     =.TRUE.

               else
C     
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
            GOTO 1040
         ENDIF
      ELSE
         CALL HIS_OPENRW(NAMH,LUH)
         IF(LUH.LT.0) GO TO 1040
      ENDIF
C
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
 2000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C
      END
