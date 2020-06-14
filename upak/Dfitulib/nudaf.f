C$PROG NUDAF     - Opens new data file
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE NUDAF(IWD,LDAT,LCMD,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/FT08/ NAMF(20)
      INTEGER*4    NAMF
C     ------------------------------------------------------------------
      COMMON/FT17/ LASIDL,LASIDN
      INTEGER*4    LASIDL,LASIDN
C     ------------------------------------------------------------------
      INTEGER*4 IWD(20)
C
      CHARACTER*80 CNAMF
C
      EQUIVALENCE (CNAMF,NAMF)
C     ------------------------------------------------------------------
C
      IERR=0
C
      LASIDL=-1
      LASIDN=-1
C
      CLOSE(UNIT=LDAT)
      IS=NXBL(IWD,1,80)
      IF(IS.LE.0) GO TO 120
      IA=NXNB(IWD,IS,80)
      IF(IA.LE.0) GO TO 120
      IB=LSNB(IWD,IA,80)
      IF(IB.LE.0) GO TO 120
C
      DO 20 I=1,20
      NAMF(I)=0
   20 CONTINUE
C
      CALL LODUP(IWD,IA,IB,NAMF,1)
C
      OPEN(UNIT     = LDAT,
     &     FILE     = CNAMF,
     &     STATUS   = 'OLD',
     &     IOSTAT   = STAT)
C
      IF(STAT.NE.0) GO TO 100
      RETURN
C
C     ------------------------------------------------------------------
C     Error RETURNs
C     ------------------------------------------------------------------
C
  100 WRITE(6,110)STAT
  110 FORMAT(1H ,'ERROR TRYING TO OPEN DATA FILE - STAT =',I8)
      GO TO 150
C
  120 WRITE(6,130)
  130 FORMAT(1H ,'SYNTAX ERROR IN DATA FILE SPECIFICATION')
C
  150 WRITE(6,160)
  160 FORMAT(1H ,'CONTROL RETURNED TO VDT')
      IERR=1
      LCMD=5
      RETURN
      END
