C$PROG NUMAC     - Accumulates integer - one ASCII digit at a time
C
C     ************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ************************************************************
C
      SUBROUTINE NUMAC(MODE,KEY,NUM,IERR)
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
      CHARACTER*4  MODE
C
#if F77 == g77
      BYTE         BV(4),BASKI(8),BASKJ(8),X30,X39
#else
      integer(kind=1) BV(4),BASKI(8),BASKJ(8),X30,X39
#endif
C
      INTEGER*4    IV,KEY,NUM,IERR,NL,N,IP,JP
C
      CHARACTER*8  IASK,JASK
C
      EQUIVALENCE (BV,IV),(BASKI,IASK),(BASKJ,JASK)
C
      DATA   X30,X39/Z'30',Z'39'/
C
      SAVE
C
C     ************************************************************
C     ACCUMULATES AN INTEGER NUMBER - ONE ASCII DIGIT AT A TIME
C                                     FOR ROUTINE GETINTEGER
C     ************************************************************
C
      IERR=0
C
      IF(MODE.EQ.'INIT') GO TO 10
      IF(MODE.EQ.'ACQ ') GO TO 20
      IF(MODE.EQ.'DONE') GO TO 30
      GO TO 200
C
   10 NL=0
      RETURN
C
   20 NL=NL+1
      IF(NL.GT.8) GO TO 210
      IV=KEY
      IF(BV(1).LT.X30) GO TO 220
      IF(BV(1).GT.X39) GO TO 220
      BASKI(NL)=BV(1)
      RETURN
C
   30 JASK=' '
      IP=NL
      JP=8
      DO 40 N=1,NL
      BASKJ(JP)=BASKI(IP)
      IP=IP-1
      JP=JP-1
   40 CONTINUE
C
      READ(JASK,50)NUM
   50 FORMAT(I8)
      RETURN
C
  200 WRITE(CMSSG,205)
  205 FORMAT('ILLEGAL NUMAC REQUEST')
      GO TO 500
C
  210 WRITE(CMSSG,215)
  215 FORMAT('MORE THAN 8 DIGITS TO NUMAC')
      GO TO 500
C
  220 WRITE(CMSSG,225)
  225 FORMAT('NON-NUMERIC CHARACTER TO NUMAC')
C
  500 CALL MESSLOG(LOGUT,0)
      IERR=1
      RETURN
      END
