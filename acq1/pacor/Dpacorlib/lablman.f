C$PROG LABLMAN
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE LABLMAN(MODE,JLABL,JLABV,IERR)     
C
      PARAMETER (MXI=2000)
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C
      COMMON/PAC6/ LABLIS(2,MXI),LABVAL(MXI),NLA 
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      DATA MXLA/MXI/
C
      CHARACTER*4  MODE,INST,LABL,LABLIS,JLABL(2)
C
      SAVE
C
C     ************************************************************
C     LABEL MANAGER - "SAVES & GETS" LABELS AND ASSOCIATED VALUES
C     ************************************************************
C
      IERR=0
C
      IF(MODE.EQ.'GET ') GO TO 100
C
      NLA=0
      II=0
      DO 20 N=1,NCI
C
      IF(INST(N).EQ.'READ') GO TO 10
      IF(INST(N).EQ.'CNAF') GO TO 10
      II=II+1
C
   10 IF(LABL(N).EQ.'    ') GO TO 20
      NLA=NLA+1
C
      IF(NLA.GT.MXLA) THEN
      WRITE(CMSSG,15)NLA
   15 FORMAT('MAX# LABELS EXCEEDED AT NLA =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NLA=MXLA
                      ENDIF
C
      LABLIS(1,NLA)=LABL(N)
      LABLIS(2,NLA)='    '
      LABVAL(NLA)=II
   20 CONTINUE
C
      DO 50 J=1,NLA
      DO 40 I=J+1,NLA
      IF(LABLIS(1,I).NE.LABLIS(1,J)) GO TO 40
      IF(LABLIS(2,I).NE.LABLIS(2,J)) GO TO 40
      WRITE(CMSSG,30)LABLIS(1,I),LABLIS(2,I)
   30 FORMAT(2A4,' - MULTIPLY DEFINED')
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
   40 CONTINUE
   50 CONTINUE
      RETURN
C
  100 JLABV=0
      DO 120 N=1,NLA
      IF(JLABL(1).NE.LABLIS(1,N)) GO TO 120
      IF(JLABL(2).NE.LABLIS(2,N)) GO TO 120
      JLABV=LABVAL(N)
      RETURN
  120 CONTINUE
      WRITE(CMSSG,130)JLABL
  130 FORMAT('LABL NOT FOUND = ',2A4)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
