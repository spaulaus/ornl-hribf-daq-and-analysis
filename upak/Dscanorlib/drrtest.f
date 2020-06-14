C$PROG DRRTEST   - Tries to open his- & drr-files 
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 02/17/99
C     ******************************************************************
C
      SUBROUTINE DRRTEST(LU,NAME,IEXIST) 
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
      INTEGER*4    LU,IEXIST,LSNB
C
      INTEGER*4    NAMD(20),LN,IEXT,JB,STAT
C
      INTEGER*4    RECLVALU
C
      CHARACTER*(*) NAME
C
      CHARACTER*80 CNAMH,CNAMD
C
      EQUIVALENCE (CNAMH,NAMH),(CNAMD,NAMD)
C
      character*4 ciext
      equivalence (ciext,iext)
      DATA        cIEXT/'.drr'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO OPEN - HIS-FILE FOR HIS_READ (C ROUTINE IN UNIX VERSION)
C                     - DRR-FILE FOR FORTRAN READ
C     ------------------------------------------------------------------
C     SET UP NAMES ETC.
C     ------------------------------------------------------------------
C
      CNAMD=NAME
C
      LN=LSNB(NAMD,1,80)		!FIND LAST NON-BLANK
      IF(LN.LE.0) GO TO 530		!ERROR IF NOT FOUND
      IF(LN.GT.76) GO TO 530		!TST FOR TOO LONG
      CALL LODUP(IEXT,1,4,NAMD,LN+1)	!DRR-FILE FULL NAME
      JB=LN+4				!LAST BYTE# OF FILENAME
      CALL ISBYTE(0,NAMD,JB)		!STUFF IN A NULL
C
C     ------------------------------------------------------------------
C     OPEN THE DRR-FILE
C     ------------------------------------------------------------------
C
      IEXIST=0
      OPEN(UNIT       = LU,
     &     FILE       = CNAMD,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     FORM       = 'UNFORMATTED',
     &     RECL       = RECLVALU(128),
     &     IOSTAT     = STAT)
C
      IF(STAT.EQ.0)THEN
        IEXIST=1
        CLOSE(LU)
      ELSE
        IEXIST=0
      ENDIF
C
      RETURN
C
  530 WRITE(LOGUT,535)
  535 FORMAT(1H ,'SYNTAX ERROR IN HIS-FILE SPECIFICATION')
      STOP
C
C
      END
