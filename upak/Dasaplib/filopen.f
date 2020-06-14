C$PROG FILOPEN
      SUBROUTINE FILOPEN(IWD,NAMF,LUS,LUD,LUH,KFIL)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4 IWD(20),NAMF(20),NAMFIL(20),DISK(2)
C
      CHARACTER*80 CNAMFIL
      EQUIVALENCE (CNAMFIL,NAMFIL)
C
      integer*4 jext
      character*4 cjext, cdisk(2)
      equivalence (cjext,jext), (cdisk,disk)
      DATA cJEXT,cDISK/3*'    '/
C
      CHARACTER*4  KFIL,IEXT
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KFIL.EQ.'HIS ') THEN
      CALL HISMAN('CLOS',NAMF,LUD,LUH,'    ',IERR)
                         ENDIF
C
      KFIL='    '
C
      CALL BILNAM(IWD,DISK,JEXT,NAMFIL,IEXT,LDOT,INEW,IERR)
      IF(IERR.NE.0) RETURN
C
      DO 310 I=1,20
      NAMF(I)=NAMFIL(I)
  310 CONTINUE
C
      IF(IEXT.EQ.'.SPK') GO TO 320
      IF(IEXT.EQ.'.spk') GO TO 320
C
      IF(IEXT.EQ.'.HIS') GO TO 340
      IF(IEXT.EQ.'.his') GO TO 340
      GO TO 400
C
  320 CALL SPKMAN('OPEN',NAMF,LUS,'RO  ',IERR)
      IF(IERR.NE.0) RETURN
      KFIL='SPK '
      RETURN
C
  340 CALL HISMAN('OPEN',NAMF,LUD,LUH,'RO  ',IERR)
C
      IF(IERR.NE.0) RETURN
C
      CALL HISIN(LUD,LUH,-1,0,0,0,IERR)
      CALL HISERR(IERR)
      IF(IERR.NE.0) RETURN
C
      KFIL='HIS '
      RETURN
C
  400 WRITE(CMSSG,405)
  405 FORMAT('ILLEGAL DATA-FILE EXTENSION - CMD IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
