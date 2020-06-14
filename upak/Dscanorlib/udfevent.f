C$PROG UDFEVENT  - Reads & returns one event from example UDF-file 
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 11/28/2004
C     ******************************************************************
C
      SUBROUTINE UDFEVENT(MXID,PID,DAT,NP,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      COMMON/SC29/ UDFNAM(20),UDFRECL,UDFNPAR
      INTEGER*4    UDFNAM,    UDFRECL,UDFNPAR
C     ------------------------------------------------------------------
      INTEGER*4    MXID,PID(*),DAT(*),BUF(2,8),NP,LU,IERR,I
C
      INTEGER*4    EOFTST,IOS
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      NP=0
      IERR=0
C
  100 INRECI=INRECI+1
C
      IF(UDFRECL.EQ.0) THEN
      READ(LUINF,110,ERR=200,END=300)BUF      !Read ASCII input line
  110 FORMAT(16I5)
      ENDIF
C
      IF(UDFRECL.GT.0) THEN
      READ(LUINF,REC=INRECI,IOSTAT=IOS)BUF    !Read binary input line
      IF(IOS.NE.0) GO TO 400
      ENDIF
C
C
      IF(BUF(1,1).EQ.0.AND.BUF(2,1).EQ.0) RETURN  !If 1st 2 wds 0, 
C                                                 !then its end-event
C
  140 DO 150 I=1,8                            !Loop on buffer values
      IF(BUF(1,I).EQ.0) GO TO 150             !If parm-ID 0, skip it
      NP=NP+1                                 !Inc cntr
      PID(NP)=BUF(1,I)                        !Save parm-ID
      DAT(NP)=BUF(2,I)                        !Save data
  150 CONTINUE
      GO TO 100                               !Go read next record
C
  200 WRITE(6,205)                            !Error return
  205 FORMAT('Error reading UDF file')
      IERR=5
      RETURN
C
  300 WRITE(6,305)                            !EOF return
  305 FORMAT('EOF reading UDF file')
      IERR=999
      RETURN
C
  400 IF(EOFTST(IOS).NE.0) THEN
      WRITE(6,305)
      IERR=999
      RETURN
      ENDIF
C
      WRITE(6,405)IOS
  405 FORMAT('Error reading UDF, IOS =',I5)
      IERR=5
      RETURN
      END
