C$PROG DRREXAM   - Tests for existance of drr-file
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 02/17/99
C     ******************************************************************
C
      SUBROUTINE DRREXAM(IEXIST)
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
      COMMON/SC02/ NAMH(20)
      INTEGER*4    NAMH
C     ------------------------------------------------------------------
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      CHARACTER*80 CNAMH,CNAMD
C
      INTEGER*4    NAMD(20)
C
      INTEGER*4    LSNB,LN,IEXT,JB,IEXIST
C
      EQUIVALENCE (CNAMH,NAMH),(CNAMD,NAMD)
C
      character*4  ciext
      equivalence  (ciext,iext)
      DATA         cIEXT/'.drr'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CNAMD=CNAMH
C
      LN=LSNB(NAMH,1,80)		!FIND LAST NON-BLANK
      IF(LN.LE.0) GO TO 530		!ERROR IF NOT FOUND
      IF(LN.GT.76) GO TO 530		!TST FOR TOO LONG
      CALL LODUP(IEXT,1,4,NAMD,LN+1)	!DRR-FILE FULL NAME
      JB=LN+4				!LAST BYTE# OF FILENAME
      CALL ISBYTE(0,NAMD,JB)		!STUFF IN A NULL
      IEXIST=0
      CALL DRRTEST(LUC(9),CNAMD,IEXIST)
      RETURN
  530 WRITE(LOGUT,535)
  535 FORMAT(1H ,'SYNTAX ERROR IN HIS-FILE SPECIFICATION')
      STOP
      END
