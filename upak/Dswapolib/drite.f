C$PROG DRITE     - Writes records to disk-file via direct access
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE DRITE(LU,IBUF,IREC,NBY,IOS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C   
      INTEGER*4 IBUF(1)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      NW=NBY/4
C
      WRITE(LU,REC=IREC,IOSTAT=IOS)(IBUF(I),I=1,NW)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   WRITE(CMSSG,10)LU,IREC
                   CALL MESSLOG(LOGUT,LOGUP)
                   ENDIF
C
   10 FORMAT('Called from routine DRITE - LU,IREC =',2I6)
C
      RETURN
      END
