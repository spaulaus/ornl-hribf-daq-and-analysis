C$PROG SYMLOG    - Displays list of symbol names & associated values
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE SYMLOG(KMD)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD
C
      COMMON/ML03/ ISYN(500),ISYV(500),NSYM
C
      CHARACTER*8  ISYN
C   
      DATA IBELL/Z'07070707'/
C
C     ------------------------------------------------------------------
C
      IF(NSYM.LE.0) RETURN
C
      IF(KMD.EQ.'ZSYM') GO TO 50
      IF(KMD.EQ.'DSYM') GO TO 100
      IF(KMD.EQ.'LSYM') GO TO 200
      RETURN
C
   50 NSYM=0
      RETURN
C
  100 NLINES=50
      CALL WINSIZE(NCOLS,NROWS,IERR)
      IF(IERR.EQ.0) NLINES=NROWS-1
      NLN=0
      IB=0
C
  110 IA=IB+1
      IF(IA.GT.NSYM) RETURN
      IB=IA+2
      IF(IB.GT.NSYM) IB=NSYM
C
      NLN=NLN+1
      IF(NLN.GT.NLINES) THEN
                        NLN=0
                        WRITE(LOGUT,120)IBELL
                        READ(5,125)IDUM
                        ENDIF
C
  120 FORMAT(1H ,A1,'There is more - type: [RETURN] to continue',$)
  125 FORMAT(A4)
C
      WRITE(LOGUT,130)(ISYN(I),ISYV(I),I=IA,IB)
  130 FORMAT(1H ,3(A,'=',I8,8X))
      GO TO 110
C
C
  200 DO 220 I=1,NSYM
      WRITE(LOGUP,205)ISYN(I),ISYV(I)
      WRITE(LOGUT,210)ISYN(I),ISYV(I)
  205 FORMAT(A,'=',I8)
  210 FORMAT(1H ,A,'=',I8)
  220 CONTINUE
      RETURN
      END
