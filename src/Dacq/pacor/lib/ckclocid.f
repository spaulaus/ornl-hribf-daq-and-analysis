C$PROG CKCLOCID  - Checks 100HZ clock parm-ID vs all others
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 01/06/2004
C     ******************************************************************
C
      SUBROUTINE CKCLOCID
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PACD/ FERID(32,576),FERC(576),FERN(576),FERT(576),NFER,
     &             FERERID
C
      COMMON/PACE/ FASID(256,32),FASC(32), FASN(32), FAST(32), NFAS,
     &             FASERID
C
      COMMON/PACN/ CAMID(32,32,8),CAMC(256),CAMN(256),CAMT(256),NCAM,
     &             CAMERID
C     ------------------------------------------------------------------
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/KLOC/ KLOCID(2)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(KLOCID(1).EQ.0) RETURN
C
      CALL CKVMEID(KLOCID(1))
C
      CALL CKVMEID(KLOCID(2))
C
      DO 100 I=1,2
C
      KLOCI=IAND(KLOCID(I),'7FFF'X)
C
C
      IF(KLOCID(I).EQ.FERERID) THEN
      WRITE(CMSSG,110)KLOCI
      CALL MESSLOG(LOGUT,LOGUP)
      NERR=NERR+1
      ENDIF
C
      IF(KLOCID(I).EQ.FASERID) THEN
      WRITE(CMSSG,120)KLOCI
      CALL MESSLOG(LOGUT,LOGUP)
      NERR=NERR+1
      ENDIF
C
      IF(KLOCID(I).EQ.CAMERID) THEN
      WRITE(CMSSG,130)KLOCI
      CALL MESSLOG(LOGUT,LOGUP)
      NERR=NERR+1
      ENDIF
C
  100 CONTINUE
C
  110 FORMAT('Error - Clock-ID =',I8,' = FERA error ID')
  120 FORMAT('Error - Clock-ID =',I8,' = FASTBUS error ID')
  130 FORMAT('Error - Clock-ID =',I8,' = CAMAC error ID')
C
      RETURN
C
      END
