C$PROG FILHAN    - Does RWI, FRI, BRI, for evel-file
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE FILHAN(KMD,NV)
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
      COMMON/LM09/ KINPUT,INRECN
      CHARACTER*4  KINPUT
      INTEGER*4           INRECN
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD
C
      INTEGER*4    NV
C
      DATA         INRECN/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'RWI ') GO TO 100
      IF(KMD.EQ.'FRI ') GO TO 200
      IF(KMD.EQ.'BRI ') GO TO 300
      RETURN
C
  100 INRECN=0
      RETURN
C
  200 INRECN=INRECN+NV
      RETURN
C
  300 INRECN=INRECN-NV
      IF(INRECN.LT.0) INRECN=0
      RETURN
      END      
