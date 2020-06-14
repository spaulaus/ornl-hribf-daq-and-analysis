C$PROG UDFINP    - UDF-Data-File input routine for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 11/22/2004
C     ******************************************************************
C
      SUBROUTINE UDFINP(IGO,JFIR,JLAS,IERR)
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
      COMMON/SC06/ LIST(16384,2)
      INTEGER*2    LIST
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      INTEGER*4    LISTF(8192,2)
C
      CHARACTER*4  CLISTF(8192,2)
C
      EQUIVALENCE (LISTF(1,1),LIST(1,1)),(CLISTF,LISTF)
C
      INTEGER*4    MOD,LREC,LP,LR,NBYT,IGO,JFIR,JLAS,IERR
C
      DATA         LREC/16384/
C
      CHARACTER*4  KIND
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C     IGO  = 1/2 FOR INITIAL/SUBSEQUENT READ (not used here)
C     JFIR = FIRST WORD IN LIST TO PROCESS
C     JLAS = LAST  WORD IN LIST TO PROCESS
C     IERR = ERROR CODE
C     LR = LIST BUFFER # (2ND INDEX OF LIST) BEING READ
C     LP = LIST BUFFER # (2ND INDEX OF LIST) BEING PROCESSED
C     ------------------------------------------------------------------
C   
      IERR=0                                    !Reset error flag
C
      LR=1                                      !Set to READ-1
      LP=1                                      !Set to PROC-1
C 
  100 CALL UDFREAD(LIST(1,LR),KIND,NBYT,IERR)
C
      JFIR=1                                    !First LIST word to use
      JLAS=NBYT/2                               !Last  LIST word to use
C
      IF(IERR.NE.0) RETURN
C
      IF(MSGF.NE.'    ')   RETURN               !Tsfr Ctrl/C
C
      IF(MOD(NBYT,2).NE.0) GO TO 100            !Skip odd# bytes
C
      IF(KIND.EQ.'DATA')   RETURN               !Tst for DATA
C
      GO TO 100                                 !Otherwise, skip it
C
      END
