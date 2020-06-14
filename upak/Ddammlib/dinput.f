C$PROG DINPUT    - Inputs data from disk or shared memory
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/12/02
C     ******************************************************************
C
      SUBROUTINE DINPUT(LUH,LUD,NCALL,IERR)
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
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     ------------------------------------------------------------------
      COMMON/XAM2/ ILOH,IHIH,ILOF,IHIF,JBOFH,JBOFF,MXBN
C     ------------------------------------------------------------------
C
C     Shared Memory information.  Set by HISMAN
C
      LOGICAL SHMFLG
      LOGICAL SHMUSE
      INTEGER SHMID
      COMMON/SharedMem/ SHMID(20), SHMFLG(20), SHMUSE
      INTEGER StartByte
C
C     ------------------------------------------------------------------
      INTEGER*4 KBUF(16384,2)
C
      EQUIVALENCE (KBUF,IDATF)
C
      DATA NBN/1/
      DATA JR,JP/1,2/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(NCALL.GT.0) GO TO 20
C
      JR=1
      JP=2
      NBN=1
      NBHI=NBN+126
      IF(NBHI.GT.MXBN) NBHI=MXBN
      NBYTS=512*(NBHI-NBN+1)
C
      IF (SHMFLG(LUD)) THEN  ! In shared memory ?
         StartByte=(NBN-1)*512+1      ! Convert block number to byte#
         CALL MEM_READ_BYTE(KBUF(1,JR), StartByte, NBYTS) 
      ELSE                   ! In file
         CALL BUFI(LUH,KBUF(1,JR),NBN,NBYTS,IERR)
      ENDIF
C
      IF(IERR.NE.0) RETURN
C
   20 ILOH=(NBN-1)*256+1
      IHIH=ILOH+32512-1
      ILOF=(NBN-1)*128+1
      IHIF=ILOF+16256-1
      JP=3-JP
      JR=3-JR
      JBOFH=-ILOH+(JP-1)*32768+1
      JBOFF=-ILOF+(JP-1)*16384+1
C
      NBN=NBN+127
      IF(NBN.GT.MXBN) RETURN
      NBHI=NBN+126
      IF(NBHI.GT.MXBN) NBHI=MXBN
      NBYTS=512*(NBHI-NBN+1)
C
      IF (SHMFLG(LUD)) THEN  ! In shared memory ?
         StartByte=(NBN-1)*512+1      ! Convert block number to byte#
         CALL MEM_READ_BYTE(KBUF(1,JR), StartByte, NBYTS) 
      ELSE                   ! In file
         CALL BUFI(LUH,KBUF(1,JR),NBN,NBYTS,IERR)
      ENDIF
C
      RETURN
      END
