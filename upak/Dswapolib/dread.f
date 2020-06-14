C$PROG DREAD     - Reads records from disk-file via direct access
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE DREAD(LU,IBUF,IREC,NBY,IOSV)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      CHARACTER*4  IOSC
C
      INTEGER*4    IOSI
C
      EQUIVALENCE (IOSC,IOSI)
C   
      INTEGER*4 IBUF(1)
      integer*4 eoftst
C
      SAVE
C
C     ------------------------------------------------------------------
C
      NW=NBY/4
C
      READ(LU,REC=IREC,IOSTAT=IOS)(IBUF(I),I=1,NW)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   WRITE(CMSSG,10)LU,IREC
                   CALL MESSLOG(LOGUT,LOGUP)
                   ENDIF
C
   10 FORMAT('Called from routine DREAD - LU,IREC =',2I6)
C
      IOSI=IOS
C
C     IF(IOS.EQ.36.OR.IOS.EQ.39.OR.IOS.EQ.-1) IOSC='EOF '
      IF (eoftst(ios) .ne. 0) IOSC='EOF '
C
      IOSV=IOSI
C
      RETURN
      END
