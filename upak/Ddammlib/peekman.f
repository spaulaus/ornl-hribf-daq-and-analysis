C$PROG PEEKMAN   - Executes peak-finds & library storage (for fitting)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE PEEKMAN(ISPK,IFI,NCH)
C
C     ------------------------------------------------------------------
      COMMON/SM24/ BIAS,IFWHM,IFINDF
      CHARACTER*4             IFINDF
C     ------------------------------------------------------------------
C   
      INTEGER*4 ISPK(*)
C   
      REAL*4 CHANX(500),PSIZE(500)
C   
      DATA MAXPK/499/
C
      SAVE
C
C     ==================================================================
C   
      IF(IFINDF.NE.'ON  ') RETURN
C   
      CALL PFIND(ISPK,CHANX,PSIZE,1,NCH,IFWHM,BIAS,MAXPK,NPK)
C   
      IF(NPK.LE.0) RETURN
C   
      DO 100 N=1,NPK
C   
      X=IFI+CHANX(N)-2.0
C   
      CALL PEEKADD(X)
C   
  100 CONTINUE
      RETURN
      END
