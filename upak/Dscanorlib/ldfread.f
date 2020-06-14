C$PROG LDFREAD   - Reads specified rec# from an LFD-file - for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/27/99
C     ******************************************************************
C
      SUBROUTINE LDFREAD(LU,NREC,IBUF,KIND,NBYT,IERR)
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
      COMMON/SC26/ SWAPLDF
      CHARACTER*4  SWAPLDF
C     ------------------------------------------------------------------
      INTEGER*4    LU,NREC,NFW,NBYT,IERR,I
C
      CHARACTER*4  KIND
C
      INTEGER*4    IBUF(8192)
C
      INTEGER*4    IOS
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      READ(LU,REC=NREC,IOSTAT=IOS)KIND,NFW,IBUF
C
      IF(IOS.NE.0) GO TO 100
C
      IF(KIND.EQ.'EOF ') THEN
      IERR=999
      NBYT=0
      RETURN
      ENDIF
C
      IF(SWAPLDF.NE.'YES ') THEN
      NBYT=4*NFW
      RETURN
      ENDIF
C
      CALL LDFSWAP(2,NFW)
C
      IF(KIND.EQ.'DATA') THEN
      DO 20 I=1,8192
      CALL LDFSWAP(1,IBUF(I))
   20 CONTINUE
      ENDIF
C
      IF(KIND.EQ.'HEAD') THEN
      DO 30 I=33,64
      CALL LDFSWAP(2,IBUF(I))
   30 CONTINUE
      ENDIF
C
      NBYT=4*NFW
C
      RETURN
C
C     ------------------------------------------------------------------
C     Error RETURNs
C     ------------------------------------------------------------------
C
  100 WRITE(CMSSG,110)NREC,LU
  110 FORMAT('Error reading record#',I8,'  from unit#',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      NBYT=0
      IERR=5
      RETURN
      END
