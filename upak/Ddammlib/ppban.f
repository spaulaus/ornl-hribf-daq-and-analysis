C$PROG PPBAN     - Passes bananas between current-ban & ban-library
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PPBAN(KMD,IDW,ID,NDX,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PL09/ XL(64,21),YL(64,21),NXYL(21),IBL(21),
     &             IBWN(21),LNBAN(4,21),MAXXYL
C     ------------------------------------------------------------------
      CHARACTER*4  KMD
C   
      DIMENSION   XC(64),YC(64)
C   
      EQUIVALENCE (XC(1),XL(1,21)),
     &            (YC(1),YL(1,21)),
     &            (NXY,NXYL(21)),
     &            (IDOPN,IBL(21))
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO PASS BANANAS BETWEEN CURRENT-BAN (X,Y)
C                                 AND BAN-LIBRARY (XL,YL)
C     ------------------------------------------------------------------
C   
      IERR=0                            !RESET ERROR FLAG
C   
      IF(KMD.EQ.'DELE') GO TO 100       !TST FOR DELETE
      IF(KMD.EQ.'RECL') GO TO 100       !TST FOR RECALL
      IF(KMD.EQ.'SAVE') GO TO 200       !TST FOR SAVE
      IF(KMD.EQ.'RSAV') GO TO 200       !TST FOR RSAV - REPL OR SAVE
      IF(KMD.EQ.'REPL') GO TO 200       !TST FOR REPLACE
      GO TO 500
C   
C     ------------------------------------------------------------------
C     PROCESS  -  DELE, REPL, RECL
C     ------------------------------------------------------------------
C   
  100 DO 110 I=1,MAXXYL                 !FIND REQUESTED BAN-ID
      IF(ID.EQ.IBL(I)) GO TO 120        !IN LIB
  110 CONTINUE
      GO TO 520
C   
  120 NDX=I                             !INDEX FOROREQUESTED BAN-ID
      IF(KMD.EQ.'DELE') GO TO 130
      IF(KMD.EQ.'REPL') GO TO 140
      IF(KMD.EQ.'RECL') GO TO 150
C   
  130 IBL(NDX)=-1                       !DELETE ENTRY FROM LIBRARY
      RETURN
C   
  140 DO 145 I=1,NXY                    !REPL LIB-ENTRY WITH CUR-BAN
      XL(I,NDX)=XC(I)
      YL(I,NDX)=YC(I)
  145 CONTINUE
      NXYL(NDX)=NXY
      IBWN(NDX)=IDW
      RETURN
C   
  150 NXY=NXYL(NDX)                     !RECALL LIB-ENTRY TO CUR-BAN
      DO 155 I=1,NXY
      XC(I)=XL(I,NDX)
      YC(I)=YL(I,NDX)
  155 CONTINUE
      IBWN(21)=IDW
      RETURN
C   
C     ------------------------------------------------------------------
C     PROCESS  -  SAVE
C     ------------------------------------------------------------------
C   
  200 NDXA=0
      NDXB=0
      DO 205 I=1,MAXXYL
      IF(IBL(I).EQ.-1) NDXA=I                      !FIND EMPTY SPACE
      IF(IBL(I).EQ.ID.AND.IBWN(I).EQ.IDW) NDXB=I   !TST FOR ID EXISTS
  205 CONTINUE
      NDX=NDXA
      IF(KMD.EQ.'RSAV'.AND.NDXB.GT.0) NDX=NDXB
      IF(NDX.LE.0) GO TO 530            !TST FOR FAIL
C   
      DO 220 I=1,NXY                    !SAVE CUR-BAN IN LIB
      XL(I,NDX)=XC(I)
      YL(I,NDX)=YC(I)
  220 CONTINUE
      NXYL(NDX)=NXY
      IBL(NDX)=ID
      IBWN(NDX)=IDW
      RETURN
C   
  500 WRITE(CMSSG,505)
  505 FORMAT('ILLEGAL KMD TO PPBAN')
      GO TO 600
C   
  520 WRITE(CMSSG,525)
  525 FORMAT('NOT FOUND')
      GO TO 600
C   
  530 WRITE(CMSSG,535)
  535 FORMAT('BAN-LIB FULL')
C   
  600 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
