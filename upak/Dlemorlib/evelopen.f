C$PROG EVELOPEN  - Opens special simulated-event-file for input
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE EVELOPEN(LU,IERR)
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM09/ KINPUT,INRECN
      CHARACTER*4  KINPUT
      INTEGER*4           INRECN
C     ------------------------------------------------------------------
      COMMON/LM27/ EVLNAM
      CHARACTER*80 EVLNAM
C     ------------------------------------------------------------------
      INTEGER*4    LU,IERR
C
      INTEGER*4    NXNB,LSNB,IA,IB,ISTAT,I
C
      INTEGER*4    RECLVALU
C
      CHARACTER*80 CNAMF
C
      INTEGER*4    NAMF(20)
C
      EQUIVALENCE (CNAMF,NAMF)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CLOSE(UNIT=LU)
      IA=NXNB(IWDRAW,5,80)
      IF(IA.LE.0) GO TO 200
      IB=LSNB(IWDRAW,IA,80)
      IF(IB.LE.0) GO TO 200
      DO 10 I=1,20
      NAMF(I)=0
   10 CONTINUE
      EVLNAM=' '
C
      CALL LODUP(IWDRAW,IA,IB,NAMF,1) 
C
      OPEN(UNIT       = LU,
     &     FILE       = CNAMF,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(LNBY),
     &     IOSTAT     = ISTAT)
C
      IF(ISTAT.EQ.0) THEN
                     KINPUT='FILE'
                     INRECN=0
                     EVLNAM=CNAMF
                     RETURN
                     ENDIF
C
      WRITE(CMSSG,105)ISTAT
  105 FORMAT('ERROR OPENING EVL-FILE - ISTAT = ',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C
  200 WRITE(CMSSG,205)
  205 FORMAT('SYNTAX ERROR IN EVEL-FILE NAME - CMD IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=0
      RETURN
      END
