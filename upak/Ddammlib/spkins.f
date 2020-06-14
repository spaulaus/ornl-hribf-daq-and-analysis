C$PROG SPKINS    - Reads data from spk-files for fit related routines
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SPKINS(KSOR,ID,IDAT,NDX,NCH,NCHD,IERR)
C
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     ------------------------------------------------------------------
      CHARACTER*4  MODE
C
      INTEGER*4 IDAT(*)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
      ILO=NDX
      IHI=ILO+NCH-1
      MODE='READ'
      IF(NCH.LE.0) MODE='TEST'
C
      CALL SPKIN1(MODE,KSOR,ID,1.0,ILO,IHI,NCHD,IERR)
C
      IF(IERR.NE.0)      RETURN
      IF(MODE.EQ.'TEST') RETURN
C
      DO 20 I=1,NCH
      IDAT(I)=IDATF(I)
   20 CONTINUE
C
      RETURN
      END
