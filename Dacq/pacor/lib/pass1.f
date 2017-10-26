C$PROG PASS1
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ************************************************************
C
      SUBROUTINE PASS1
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/ML02/ IWDRAW(20)
C
      COMMON/III/  LIN,LCM,LCI
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      INTEGER*4 LASLAT,LASCAM,FIRRGA,LASRGA,FIRCGA,LASCGA,FIRCDN
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      BYTE         IBY(80),DOL
C
      character*1 cDOL
      equivalence (cDOL, DOL)
      DATA                 cDOL/'$'/
C
      CHARACTER*4  KMD,IWD1
C
      EQUIVALENCE (KMD,LWD(1,1)),(IBY,IWD),(IWD1,IWD(1))
C
      SAVE
C
C     ************************************************************
C     READ SOURCE FILE & REQUEST PROCESSING OF $xxx TYPES ONLY
C     ************************************************************
C
      REWIND LIN
C
      LASDOL=0            !LINE# FOR LAST  $-DIRECTIVE
      LASLAT=0            !LINE# FOR LAST  $LAT  ENTRY
      LASCAM=0            !LINE# FOR LAST  $CAM  ENTRY
      FIRRGA=0            !LINE# FOR FIRST $RGAT ENTRY
      LASRGA=0            !LINE# FOR LAST  $RGAT ENTRY
      FIRCGA=0            !LINE# FOR FIRST $CGAT ENTRY
      LASCGA=0            !LINE# FOR LAST  $CGAT ENTRY
      FIRCDN=0            !LINE# FOR FIRST $CDN  ENTRY
C
      NL=0
   50 NL=NL+1
      READ(LIN,110,END=60)IWD
C
      CALL LEGLASS(IWD,NL)
C
      IF(IBY(1).NE.DOL)  GO TO 50
C
                         LASDOL=NL
      IF(IWD1.EQ.'$LAT') LASLAT=NL
      IF(IWD1.EQ.'$CAM') LASCAM=NL
C
      IF(IWD1.EQ.'$RGA') THEN
                         LASRGA=NL
      IF(FIRRGA.EQ.0)    FIRRGA=NL
                         ENDIF
C
      IF(IWD1.EQ.'$CGA') THEN
                         LASCGA=NL
      IF(FIRCGA.EQ.0)    FIRCGA=NL
                         ENDIF
C
      IF(IWD1.EQ.'$CDN') THEN
      IF(FIRCDN.EQ.0)    FIRCDN=NL
                         ENDIF
C
      GO TO 50
C
   60 REWIND LIN
      NL=0
C
      IF(FIRRGA.LT.LASLAT.OR.FIRRGA.LT.LASCAM) THEN
      WRITE(CMSSG,65)
   65 FORMAT('$RGAT spec out of order - must follow $LATs & $CAMs')
      CALL ERRLOG(LOGUT,LOGUP)
                                               ENDIF
C
      IF(FIRCGA.LT.LASRGA) THEN
      WRITE(CMSSG,70)
   70 FORMAT('$CGAT spec out of order - must follow $RGATs')
      CALL ERRLOG(LOGUT,LOGUP)
                           ENDIF
C
      IF(FIRCDN.LT.LASCGA) THEN
      WRITE(CMSSG,75)
   75 FORMAT('$CDN spec out of order - must follow $CGAts')
      CALL ERRLOG(LOGUT,LOGUP)
                           ENDIF
C
  100 NL=NL+1
      IF(NL.GT.LASDOL) GO TO 1000
C
      READ(LIN,110,END=1000)IWD
  110 FORMAT(20A4)
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
  115 CONTINUE
      CALL CASEUP1(IWDRAW)
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      CALL LISSOR('SOR ',IWDRAW)
C
      IF(IBY(1).NE.DOL) GO TO 100   
C
      CALL KASUP4(IWD)
C
      IF(KMD.EQ.'$INI') GO TO 210
      IF(KMD.EQ.'$DUN') GO TO 210
      IF(KMD.EQ.'$RUN') GO TO 210
C
      IF(KMD.EQ.'$DLA') GO TO 220
C
      IF(KMD.EQ.'$CAM') GO TO 230
      IF(KMD.EQ.'$FER') GO TO 230
      IF(KMD.EQ.'$FAS') GO TO 230
      IF(KMD.EQ.'$LAT') GO TO 230
C
      IF(KMD.EQ.'$PAT') GO TO 240
      IF(KMD.EQ.'$RIF') GO TO 250
      IF(KMD.EQ.'$KIL') GO TO 260
      IF(KMD.EQ.'$DID') GO TO 270
C
      IF(KMD.EQ.'$RGA') GO TO 280
      IF(KMD.EQ.'$CGA') GO TO 280
      IF(KMD.EQ.'$CDN') GO TO 290
C
      IF(KMD.EQ.'$XIA') GO TO 300
      IF(KMD.EQ.'$VME') GO TO 310
C
      IF(KMD.EQ.'$CID') GO TO 320
C
      WRITE(CMSSG,200)IWD(1)
      CALL ERRLOG(LOGUT,LOGUP)
  200 FORMAT(A4,' - DIRECTIVE NOT RECOGNIZED ******')
      GO TO 100
C
  210 CALL CANITQIT                   !PROCESS $INI, $DUN, $RUN 
      GO TO 100
C
  220 CALL DELAYX                     !Process $DLA
      GO TO 100
C
  230 CALL HARDASS(IWD)               !PROCESS $CAM, $FER, $FAS
      GO TO 100
C
  240 CALL PATTER(IWD)                !PROCESS $PAT ENTRY
      GO TO 100
C
  250 CALL RIFFER(IWD)                !PROCESS $RIF ENTRY
      GO TO 100
C
  260 CALL KILLER(IWD)                !PROCESS $KIL ENTRY
      GO TO 100
C
  270 CALL ERRORID                    !PROCESS $DID ENTRY
      GO TO 100
C
  280 CALL GATER(IWD)                 !PROCESS $RGAT & $CGAT ENTRYS
      GO TO 100
C
  290 CALL CNTDOWN(IWD)               !PROCESS $CDN ENTRY
      GO TO 100
C
  300 CALL XIAASS(IWD)                !PROCESS $XIA ENTRY
      GO TO 100
C
  310 CALL VMEASS(IWD)                !PROCESS $VME ENTRY
      GO TO 100
C
  320 CALL CLOCASS(IWD)               !PROCESS $CID ENTRY
      GO TO 100
C
 1000 RETURN
      END
