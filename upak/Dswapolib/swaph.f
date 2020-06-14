C$PROG SWAPH     - Swaps half-words in full-words
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE SWAPH(BUF,IA,IB)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C   
      INTEGER*2 BUF(1),IT,JT
C   
      NHW=IB-IA+1
      NDO=NHW/2
      IF(2*NDO.NE.NHW) GO TO 100
C   
      I=IA
      DO 10 N=1,NDO
      IT=BUF(I)
      JT=BUF(I+1)
      BUF(I)=JT
      BUF(I+1)=IT
      I=I+2
   10 CONTINUE
      RETURN
C   
  100 WRITE(CMSSG,110)
  110 FORMAT('Half-word swap error - odd number requested')
      CALL MESSLOG(LOGUT,LOGUP)
      STOP
      END
