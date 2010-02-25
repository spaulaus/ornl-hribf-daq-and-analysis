C$PROG LOGOPEN   - Opens SCALER specificatiom file for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LOGOPEN(LU,NAMF,IERR)    
C
C     ------------------------------------------------------------------
      COMMON/SD04/ SNITFIL(20)
      INTEGER*4    SNITFIL
C     ------------------------------------------------------------------
      INTEGER*4 NAMF(20),NAMFI(20)
C
      CHARACTER*80 CNAMFI
C
      EQUIVALENCE (CNAMFI,NAMFI)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      DO 10 I=1,20
      NAMFI(I)  =NAMF(I)
      SNITFIL(I)=NAMF(I)
   10 CONTINUE
C
      CLOSE(UNIT=LU)
      IA=LSNB(NAMFI,1,80)
      CALL ISBYTE(0,NAMFI,IA)
C
      OPEN(UNIT     = LU,
     &     FILE     = CNAMFI,
     &     STATUS   = 'OLD',
     &     IOSTAT   = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL OPENERR(ISTAT)
                     IERR=ISTAT
                     ENDIF
      RETURN
      END
