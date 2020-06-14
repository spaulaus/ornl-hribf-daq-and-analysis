C$PROG FADDCHECK - Checks fadd input & output files for compatability
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 08/20/2005
C     ******************************************************************
C
      SUBROUTINE FADDCHECK(IERR)
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
      COMMON/HIS1/ NAMI,NAMO,LUHI,LUDI,LUHO,LUDO,INOP,OUOP
      CHARACTER*160 NAMI,NAMO
      INTEGER*4              LUHI,LUDI,LUHO,LUDO
      CHARACTER*4                                INOP,OUOP
C     ------------------------------------------------------------------
      COMMON/HIS2/ NRDI,NRHI,NRDO,NRHO
      INTEGER*4    NRDI,NRHI,NRDO,NRHO
C     ------------------------------------------------------------------
      COMMON/HIS3/ RECLIS(100),HPCLIS(100),NLIS
      INTEGER*4    RECLIS,     HPCLIS,     NLIS
C     ------------------------------------------------------------------
C
      INTEGER*4    NHIS,IERR,NREC,NDO,N,I
C
      INTEGER*4    DIRIF(32),DIROF(32),HISIF(16384),HISOF(16384)
C
      INTEGER*2    DIRIH(64),DIROH(64),HISIH(32768),HISOH(32768)
C
      EQUIVALENCE (DIRIH,DIRIF),(DIROH,DIROF)
C
      EQUIVALENCE (HISIH,HISIF),(HISOH,HISOF)
C
      INTEGER*4    HRECN,HWPC,LHWPC
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(INOP.NE.'YES ') RETURN       !Omit   if input  not open
C
      IF(OUOP.NE.'YES ') RETURN       !Omit   if output not open
C
      IF(NAMI.EQ.NAMO)   GO TO 990    !Reject if input & output same
C
      IF(NRDI.NE.NRDO)   GO TO 1020   !Reject if drr-files not same size
C
      IF(NRHI.NE.NRHO)   GO TO 1030   !Reject if his-files not same size
C
C     ------------------------------------------------------------------
C     Read first record of input & output drr-files
C     ------------------------------------------------------------------
C
      NREC=1
C
      READ(LUDI,REC=NREC,ERR=1900)DIRIF
      READ(LUDO,REC=NREC,ERR=1910)DIROF
C
      IF(DIRIF(4).NE.DIROF(4)) GO TO 1040  !Check # histograms on file
C
      IF(DIRIF(5).NE.DIROF(5)) GO TO 1050  !Check # half-words on file
C
      NHIS=DIRIF(4)                        !Save # histograns in NHIS
C
C     ------------------------------------------------------------------
C     Read and check all directory records
C     ------------------------------------------------------------------
C
      LHWPC=0                              !Init last half-wds/channel
      NLIS=0                               !Init hwpc list index
C
      DO 100 N=1,NHIS                      !Loop on # histograms
C
      NREC=NREC+1                          !Inc record number
      READ(LUDI,REC=NREC,ERR=1900)DIRIF    !Read input  drr-file
      READ(LUDO,REC=NREC,ERR=1910)DIROF    !Read output drr-file
C
      DO 20 I=1,22
      IF(DIRIH(I).NE.DIROH(I)) GO TO 1060  !Check 1st 22 half-wds
   20 CONTINUE
C
      IF(DIRIF(12).NE.DIROF(12))GO TO 1070 !Check disk offset
C
      HRECN=DIRIF(12)/32768+1              !Compute his-file rec#
C
      HWPC=DIRIH(2)                        !Get half-wds/channel
C
      IF(HWPC.NE.LHWPC) THEN               !Check for change in HWPC
      NLIS=NLIS+1                          !If yes, inc list index
      RECLIS(NLIS)=HRECN                   !Store rec# where it changed
      HPCLIS(NLIS)=HWPC                    !Store new HWPC value in list
      LHWPC=HWPC                           !Reset last HWPC value
      ENDIF
C
  100 CONTINUE
C
      NLIS=NLIS+1                          !Inc HWPC-list index
      RECLIS(NLIS)=NRHI+1                  !Store max-value plus 1
      HPCLIS(NLIS)=0                       !Store hwpc=0
C
C     ------------------------------------------------------------------
C     Read and check all ID-number records
C     ------------------------------------------------------------------
C
      NDO=NRDI-NHIS-1                      !Compute # ID records to read
C
      DO 200 N=1,NDO
      NREC=NREC+1
      READ(LUDI,REC=NREC,ERR=1900)DIRIF
      READ(LUDO,REC=NREC,ERR=1910)DIROF
      DO 120 I=1,32
      IF(DIRIF(I).NE.DIROF(I)) GO TO 1080
  120 CONTINUE
  200 CONTINUE
C
      RETURN
C
C     ------------------------------------------------------------------
C     Return error messages
C     ------------------------------------------------------------------
C
  990 WRITE(CMSSG,995)
  995 FORMAT('Input & Output files have same name - not allowed')
      GO TO 2000
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Input file is not open')
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('Ounput file is not open')
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Input & Output drr-file is not same length') 
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('Input & Output his-file is not same length')
      GO TO 2000
C
 1040 WRITE(CMSSG,1045)
 1045 FORMAT('Disagreement in # histograms on his-file')
      GO TO 2000
C
 1050 WRITE(CMSSG,1055)
 1055 FORMAT('Disagreement in # half-wds on his-file')
      GO TO 2000
C
 1060 WRITE(CMSSG,1065)N
 1065 FORMAT('Disagreement in drr-file data for histogram#',I8)
      GO TO 2000
C
 1070 WRITE(CMSSG,1075)N
 1075 FORMAT('Disagreement in his-file offset for histogram#',I8)
      GO TO 2000
C
 1080 WRITE(CMSSG,1085)NREC
 1085 FORMAT('Disagreement in histogram ID-number at rec#',I8)
      GO TO 2000
C
 1900 WRITE(CMSSG,1905)NREC
 1905 FORMAT('Error reading input drr-file at REC#',I8)
      GO TO 2000
C
 1910 WRITE(CMSSG,1915)NREC
 1915 FORMAT('Error reading output drr-file at REC#',I8)
      GO TO 2000
C
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
