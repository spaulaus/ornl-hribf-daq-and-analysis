C$PROG LDFREAD   - Reads the specified recoerd# from an LFD-file
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE LDFREAD(LU,NREC,IBUF,KIND,NFW,STAT)
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
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM31/ SWAPLDF
      CHARACTER*4  SWAPLDF
C     ------------------------------------------------------------------
      CHARACTER*4  STAT,KIND
C
      INTEGER*4    LU,NREC,NFW,IOS,I
C
      INTEGER*4    IBUF(8192)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      STAT='GOOD'
C
      READ(LU,REC=NREC,IOSTAT=IOS)KIND,NFW,IBUF
C
      IF(IOS.NE.0) GO TO 100
C
      IF(KIND.EQ.'EOF ') THEN
      STAT='EOF '
      NFW=0
      RETURN
      ENDIF
C
      IF(LU.NE.LUINF)       RETURN
C
      IF(SWAPLDF.NE.'YES ') RETURN
C
C     ------------------------------------------------------------------
C     Byte-swap records as required
C     ------------------------------------------------------------------
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
      RETURN
C
C     ------------------------------------------------------------------
C     Error RETURNs
C     ------------------------------------------------------------------
C
  100 WRITE(CMSSG,120)NREC,LU
  120 FORMAT('Error reading record#',I8,'  from unit#',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      NFW=0
      STAT='BAD '
      RETURN
      END
