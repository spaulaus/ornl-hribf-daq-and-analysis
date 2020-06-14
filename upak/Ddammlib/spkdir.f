C$PROG SPKDIR    - Displays/logs spk-file directory
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SPKDIR(LIN)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/GN01/ NDLINES,IBELL
C     ------------------------------------------------------------------
      INTEGER*4    IDIR(516)
C
      CHARACTER*4  IDUM
C
      SAVE
C
C     ==================================================================
C
      READ(LIN,REC=1,IOSTAT=IOS)(IDIR(I),I=1,512)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   RETURN
                   ENDIF
C
      IA=5
      IB=2*IDIR(3)+3
      IF(IB.GT.511) GO TO 100
      IDIR(IB+3)=IDIR(4)
      DO 20 I=IA,IB,2
      IDIR(I+1)=(IDIR(I+3)-IDIR(I+1)-64)/2
   20 CONTINUE
C
      IC=IB+1
      IB=IA-1
      NLN=0
   50 IA=IB+1
      IF(IA.GT.IC) RETURN
      IB=IA+11
      IF(IB.GT.IC) IB=IC
      WRITE(CMSSG,60)(IDIR(I),I=IA,IB)
   60 FORMAT(6(I6,'/',I5))
      CALL MESSLOG(LOGUT,LOGUP)
      NLN=NLN+1
      IF(NLN.LT.NDLINES) GO TO 50
      NLN=0
      WRITE(LOGUT,65)IBELL
   65 FORMAT(1H ,A1,' THERE IS MORE - DO CR TO CONT - X TO QUIT',$)
      READ(5,70)IDUM
   70 FORMAT(A4)
      IF(IDUM.NE.'    ') RETURN
      GO TO 50
C
  100 WRITE(CMSSG,110)
  110 FORMAT('DIRECTORY SCREWED UP')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
