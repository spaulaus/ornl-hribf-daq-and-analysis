C$PROG DDAT      - Displays buffer data in various formats for LEMOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/13/99
C     ******************************************************************
C
      SUBROUTINE DDAT(KMD,ILO,IHI)
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
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM05/ IBUF(16384)
      INTEGER*4    IBUF
C     ------------------------------------------------------------------
      CHARACTER*4  KMD
C
      INTEGER*4    ILO,IHI,JLO,JHI,IA,IB,NPARR,N,NM1,NUM,I
C
      CHARACTER*6  KBUF(4000)
C
      INTEGER*2    JBUF(32768),LINE(64)
C
      EQUIVALENCE (JBUF(1),IBUF(1))
C
      INTEGER*2    XFFFF,X0000
      DATA         XFFFF,X0000/-1,Z'0000'/
C
      INTEGER*2    BLANK
      DATA         BLANK/Z'2020'/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'PEVZ') GO TO 100
      IF(KMD.EQ.'DEVZ') GO TO 100
C
      IF(KMD.EQ.'PEV ') GO TO 400
      IF(KMD.EQ.'DEV ') GO TO 400
C
      IF(KMD.EQ.'PZ  ') GO TO 1200
      IF(KMD.EQ.'DZ  ') GO TO 1210
      IF(KMD.EQ.'PI  ') GO TO 1220
      IF(KMD.EQ.'DI  ') GO TO 1230
      IF(KMD.EQ.'PIF ') GO TO 1240
      IF(KMD.EQ.'DIF ') GO TO 1250
C
      IF(KMD.EQ.'PA  ') GO TO 1300
      IF(KMD.EQ.'DA  ') GO TO 1300
C
C     ------------------------------------------------------------------
C     PRINT OR DISPLAY DATA IN HEX "EVENT FORMAT"
C     ------------------------------------------------------------------
C
  100 DO 110 I=ILO,IHI
      IF(JBUF(I).EQ.XFFFF) GO TO 120
  110 CONTINUE
      WRITE(CMSSG,115)
      CALL MESSLOG(LOGUT,LOGUP)
  115 FORMAT('NO "EVENTS" FOUND')
      RETURN
C
  120 IB=I
  130 IA=IB+1
      IF(IB.GT.IHI) RETURN
      IF(MSGF.NE.'    ') RETURN
      NPARR=0
      DO 140 I=IA,IHI
      IF(JBUF(I).EQ.XFFFF) GO TO 150
      IF(JBUF(I).GE.X0000) NPARR=NPARR+1
  140 CONTINUE
      RETURN
C
  150 IB=I
      IF(KMD.EQ.'PEV ') GO TO 180
C
      WRITE(LOGUT,160)IA,NPARR,(JBUF(I),I=IA,IB)
  160 FORMAT(1H ,'I,NP=',I5,I4,' - ',10Z6/(1H ,17X,10Z6))
      GO TO 130
C
  180 WRITE(LOGUP,190)IA,NPARR,(JBUF(I),I=IA,IB)
  190 FORMAT(1H ,'I,NP=',I5,I4,' - ',16Z6/(1H ,17X,16Z6))
      GO TO 130
C
C     ------------------------------------------------------------------
C     PRINT OR DISPLAY DATA IN INTEGER "EVENT FORMAT" - L002 format
C     ------------------------------------------------------------------
C
  400 IF(LFORM.EQ.'L003')  GO TO 500
C
      DO 410 I=ILO,IHI
      IF(JBUF(I).EQ.XFFFF) GO TO 420
  410 CONTINUE
      WRITE(CMSSG,415)
      CALL MESSLOG(LOGUT,LOGUP)
  415 FORMAT('NO "EVENTS" FOUND')
      RETURN
C
  420 IB=I
  430 IA=IB+1
      IF(IB.GT.IHI) RETURN
      IF(MSGF.NE.'    ') RETURN
      NPARR=0
      DO 440 I=IA,IHI
      IF(JBUF(I).EQ.XFFFF) GO TO 450
      IF(JBUF(I).GE.X0000) NPARR=NPARR+1
  440 CONTINUE
      RETURN
C
  450 IB=I
C
      CALL FORMEV(JBUF,KBUF,IA,IB,NUM)
C
      IF(KMD.EQ.'PEV ') GO TO 480
C
      WRITE(LOGUT,460)IA,NPARR,(KBUF(I),I=1,NUM)
  460 FORMAT(1H ,'I,NP=',I5,I4,' - ',8(A6,1X)/(1H ,17X,8(A6,1X)))
      GO TO 430
C
  480 WRITE(LOGUP,490)IA,NPARR,(KBUF(I),I=1,NUM)
  490 FORMAT(1H ,'I,NP=',I5,I4,' - ',12(A6,1X)/(1H ,17X,12(A6,1X)))
      GO TO 430
C
C     ------------------------------------------------------------------
C     PRINT OR DISPLAY DATA IN INTEGER "EVENT FORMAT" - L003 format
C     ------------------------------------------------------------------
C
  500 JLO=ILO
      JHI=IHI
C
      IF(2*(JLO/2).EQ.JLO) JLO=JLO-1
      IF(2*(JHI/2).NE.JHI) JHI=JHI+1
C
      IF(JLO.EQ.1) THEN
      IB=0
      GO TO 530
      ENDIF
C
      DO 510 I=JLO,JHI,2
C
      IF(JBUF(I).EQ.XFFFF.AND.JBUF(I+1).EQ.XFFFF) THEN
      IB=I+1
      GO TO 530
      ENDIF
C
  510 CONTINUE
C
      WRITE(CMSSG,515)
      CALL MESSLOG(LOGUT,LOGUP)
  515 FORMAT('NO "EVENTS" FOUND')
      RETURN
C
  530 IA=IB+1
C
      IF(IB.GT.JHI)      RETURN
      IF(MSGF.NE.'    ') RETURN
C
      NPARR=0
      DO 540 I=IA,JHI,2
      IF(JBUF(I).EQ.XFFFF.AND.JBUF(I+1).EQ.XFFFF) GO TO 550
      NPARR=NPARR+1
  540 CONTINUE
      RETURN
C
  550 IB=I+1
C
      CALL FORMEV3(JBUF,KBUF,IA,IB,NUM)
C
      IF(KMD.EQ.'PEV ') GO TO 580
C
      WRITE(LOGUT,560)IA,NPARR,(KBUF(I),I=1,NUM)
  560 FORMAT(1H ,'I,NP=',I5,I4,' - ',8(A6,1X)/(1H ,17X,8(A6,1X)))
      GO TO 530
C
  580 WRITE(LOGUP,590)IA,NPARR,(KBUF(I),I=1,NUM)
  590 FORMAT(1H ,'I,NP=',I5,I4,' - ',12(A6,1X)/(1H ,17X,12(A6,1X)))
      GO TO 530
C
C     ------------------------------------------------------------------
C     PRINT OR DISP DATA IN 16-BIT HEX OR INTEGER OR 32-BIT INTEGER
C     ------------------------------------------------------------------
C
 1200 WRITE(LOGUP,1205)(JBUF(I),I=ILO,IHI)
 1205 FORMAT(1H ,16Z6)
      RETURN
C
 1210 WRITE(LOGUT,1215)(JBUF(I),I=ILO,IHI)
 1215 FORMAT(1H ,10Z6)
      RETURN
C
 1220 WRITE(LOGUP,1225)(JBUF(I),I=ILO,IHI)
 1225 FORMAT(1H ,16I7)
      RETURN
C
 1230 WRITE(LOGUT,1235)(JBUF(I),I=ILO,IHI)
 1235 FORMAT(1H ,10I7)
      RETURN
C
 1240 WRITE(LOGUP,1245)(IBUF(I),I=ILO,IHI)
 1245 FORMAT(1H ,10I12)
      RETURN
C
 1250 WRITE(LOGUT,1255)(IBUF(I),I=ILO,IHI)
 1255 FORMAT(1H ,6I12)
      RETURN
C
C     ------------------------------------------------------------------
C     PRINT OR DISPLAY DATA IN "ASCII FORMAT"
C     ------------------------------------------------------------------
C
 1300 IB=ILO-1
 1310 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IF(MSGF.NE.'    ') RETURN
      IB=IA+39
      IF(IB.GT.IHI) IB=IHI
      N=0
C
      DO 1320 I=IA,IB
      N=N+1
      LINE(N)=JBUF(I)
 1320 CONTINUE
      NM1=N
      IF(NM1.GT.39.AND.LINE(N).EQ.BLANK) NM1=N-1
C
      CALL CHEKAS(LINE,N)
C
      IF(KMD.EQ.'PA  ') WRITE(LOGUP,1330)(LINE(I),I=1,N)
      IF(KMD.EQ.'DA  ') WRITE(LOGUT,1335)(LINE(I),I=1,NM1)
 1330 FORMAT(1H ,40A2)
 1335 FORMAT(1H ,39A2)
      GO TO 1310
C
      END
