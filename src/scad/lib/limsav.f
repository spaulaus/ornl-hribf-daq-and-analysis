C$PROG LIMSAV    - Saves/deletes entries in rate-limit table
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LIMSAV(NDX,KIND,XLO,XHI)
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
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
C     ------------------------------------------------------------------
C
      CHARACTER*4  KIND
C
      INTEGER*4    NDX,II,I
C
      REAL*4       XLO,XHI
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KIND.EQ.'SAV ') GO TO 100
      IF(KIND.EQ.'DEL ') GO TO 200
                         GO TO 500
C
C     ------------------------------------------------------------------
C     Save a new entry or replace an old entry
C     ------------------------------------------------------------------
C
  100 DO 110 I=1,NLIM
      IF(NDX.EQ.SCNDX(I)) GO TO 120
  110 CONTINUE
      IF(NLIM.GE.MXLIM) GO TO 510
C
      NLIM=NLIM+1
      SCNDX(NLIM)=NDX
      LIMLO(NLIM)=XLO
      LIMHI(NLIM)=XHI
      RETURN
C
  120 LIMLO(I)=XLO
      LIMHI(I)=XHI
      RETURN
C
C     ------------------------------------------------------------------
C     Remove an entry
C     ------------------------------------------------------------------
C
  200 II=0
      DO 220 I=1,NLIM
      IF(SCNDX(I).EQ.NDX) GO TO 220
      II=II+1
      SCNDX(II)=SCNDX(I)
      LIMLO(II)=LIMLO(I)
      LIMHI(II)=LIMHI(I)
  220 CONTINUE
      NLIM=II
      RETURN
C
C     ------------------------------------------------------------------
C     Report error messages
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Illegal CALL LIMSAV - cmd ignored')
      GO TO 1000
C
  510 WRITE(CMSSG,515)MXLIM
  515 FORMAT('Rate limit table overflow (max# = ',I2,') - cmd ignored')
      GO TO 1000
C
 1000 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
