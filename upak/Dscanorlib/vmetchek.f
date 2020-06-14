C$PROG VMETCHEK  - Checks "buffer" for VME clock within specified limits
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/04/2004
C     ******************************************************************
C
      SUBROUTINE VMETCHEK(LIST,STAT)
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
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      COMMON/SC28/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC,CLONOF
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
      CHARACTER*4                                             CLONOF
C     ------------------------------------------------------------------
C
      INTEGER*2    LIST(*)
C
      INTEGER*4    STAT,TIM,LASTAT
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      STAT=0
C
      IF(CLONOF.NE.'CLON') RETURN
C
      IF(CLIDLO.LE.0) RETURN
C
      IF(VMETHI.LE.0) RETURN
C
      CALL VMETGET(LIST,TIM)
C
      IF(TIM.LE.0)    RETURN
C
      IF(TIM.LT.VMETLO) THEN
      STAT=-1
      ENDIF
C
      IF(TIM.GT.VMETHI) THEN
      STAT=1
      ENDIF
C
      IF(STAT.EQ.LASTAT) RETURN
C
      LASTAT=STAT
C
      IF(STAT.EQ.0) WRITE(CMSSG,10)INRECI-1
      IF(STAT.EQ.1) WRITE(CMSSG,20)INRECI-1
C
      CALL MESSLOG(LOGUT,LOGUP)
C
   10 FORMAT('Processing started at record# ',I8)
   20 FORMAT('Processing ended   at record# ',I8)
C
      RETURN
C
      END

