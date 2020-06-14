C$PROG CMPCOPY   - Command processor for LEMOR - COPY, etc
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CMPCOPY(IDONE,IERR)
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM22/ NBRED,ICNF,INFLG
      INTEGER*4    NBRED
      CHARACTER*4        ICNF,INFLG
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      CHARACTER*4  IDONE,KMD
C
      INTEGER*4    IERR
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C     
C     ------------------------------------------------------------------
C     Process commands
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'TCOP') GO TO 400
      IF(KMD.EQ.'FCOP') GO TO 400
      IF(KMD.EQ.'STEX') GO TO 1500
C
      IF(KMD.EQ.'COPY') GO TO 500
      IF(KMD.EQ.'CREC') GO TO 500
      IF(KMD.EQ.'CC  ') GO TO 500
C
      RETURN
C
C     ------------------------------------------------------------------
C     COPY ASCII RECORDS (TAPE-TO-DISK OR DISK-TO-TAPE)
C     ------------------------------------------------------------------
C
  400 CALL FILCOP(KMD,IERR)
      GO TO 5500
C
C     ------------------------------------------------------------------
C     COPY N (FILES)  OR  CREC N (RECORDS) FROM INPUT TO OUTPUT
C     ------------------------------------------------------------------
C
  500 ICNF='NO  '
C
      IF(INTYP.EQ.'EVEL') THEN
      CALL COPYEVL(KMD,NF,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
      ENDIF
C
      IF(INTYP.EQ.'LDF '.AND.OUFOP.EQ.'YES ') THEN
      CALL COPYFTF(KMD,NF,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
      ENDIF
C
      IF(INTYP.EQ.'UDF '.AND.OUFOP.EQ.'YES ') THEN
      WRITE(6,777)
  777 FORMAT('CALLING COPYUTF')
      CALL COPYUTF(KMD,NF,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
      ENDIF
C
      IF(OUFOP.EQ.'YES ') THEN
      CALL COPYTOF(KMD,NF,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
      ENDIF
C
      IF(INFOP.EQ.'YES ') THEN
      CALL COPYFTT(KMD,NF,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
      ENDIF
C
      CALL COPY(KMD,NF,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
C
C     ------------------------------------------------------------------
C     SAVE TEXT BLOCKS ON DISK FILE (NORMALLY EVENT-HANDLER PROG)
C     ------------------------------------------------------------------
C
 1500 CALL TEXSAV(IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
C
C     ------------------------------------------------------------------
C     NORMAL RETURN
C     ------------------------------------------------------------------
C
 5500 IDONE='YES '
      IERR=0
      RETURN
C
C     ------------------------------------------------------------------
C     ERROR RETURN
C     ------------------------------------------------------------------
C
 6000 IDONE='YES '
      IERR=1
      RETURN
      END
