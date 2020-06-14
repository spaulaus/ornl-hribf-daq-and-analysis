C$PROG BANX      - Does read, store, repl & delete in ban-file
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BANX(KMD,IDW,IBN,IDX,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DML5/ TITI(20),FILI(6),KPARI(9),IHI,DGI,        !/DML5
     &             TITO(20),FILO(6),KPARO(9),IHO,DGO         !/DML5
      INTEGER*4    TITI,    FILI,   KPARI,   IHI,DGI
      INTEGER*4    TITO,    FILO,   KPARO,   IHO,DGO
C     ------------------------------------------------------------------
      COMMON/PL09/ XL(64,21),YL(64,21),NXYL(21),IBL(21),
     &             IBWN(21),LNBAN(4,21),MAXXYL
C     ------------------------------------------------------------------
      COMMON/PL18/ KHISDAT(9,20)
C     ------------------------------------------------------------------
C
      INTEGER*4 DELE,REPL,RECL,SAVE,RSAV
      character*4 cDELE,cREPL,cRECL,cSAVE, cRSAV
      equivalence (cdele,dele), (crepl,repl), (crecl,recl), 
     &            (csave,save), (crsav,rsav)
C   
      DIMENSION XC(64),YC(64),XT(64),YT(64)
C   
      INTEGER*4 IX(64),JY(64),MSG(7)
C   
      EQUIVALENCE (XC(1),XL(1,21)),
     &            (YC(1),YL(1,21)),
     &            (NXY,NXYL(21)),
     &            (IDOPN,IBL(21))
C   
      DATA cDELE,cREPL,cRECL,cSAVE/'DELE','REPL','RECL','SAVE'/
      DATA cRSAV/'RSAV'/
      DATA LUB/20/
C
      CHARACTER*4  KMD
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(KMD.EQ.'READ') GO TO 100
      IF(KMD.EQ.'STOR') GO TO 200
      IF(KMD.EQ.'REPL') GO TO 210
      IF(KMD.EQ.'DELE') GO TO 220
      GO TO 500
C   
  100 CALL BANIO(1,LUB,FILI,TITI,IHI,IBN,DGI,IX,JY,NP,NID,KPARI,
     &             MSG,IERR)
C   
      IF(IERR.NE.0) GO TO 520
C
      NBSX=LOGRAT2(KHISDAT(4,IDW),KPARI(4))
      NBSY=LOGRAT2(KHISDAT(6,IDW),KPARI(6))
C
      DO 102 I=1,NP
      IX(I)=ISHFT(IX(I),NBSX)
      JY(I)=ISHFT(JY(I),NBSY)
  102 CONTINUE
C   
      NXYT=NXY
      DO 105 I=1,NXY
      XT(I)=XC(I)
      YT(I)=YC(I)
  105 CONTINUE
C   
      NXY=NP
      DO 110 I=1,NP
      XC(I)=IX(I)
      YC(I)=JY(I)
  110 CONTINUE
C   
      CALL PPBAN(RSAV,IDW,IBN,IDX,IERR)
C   
      NXY=NXYT
      DO 115 I=1,NXY
      XC(I)=XT(I)
      YC(I)=YT(I)
  115 CONTINUE
      IBWN(21)=IDW
C   
      RETURN
C   
  200 MO=2
      CALL PPBAN(SAVE,IDW,IBN,IDX,IERR)
      IF(IERR.NE.0) RETURN
      GO TO 300
C   
  210 MO=3
      CALL PPBAN(REPL,IDW,IBN,IDX,IERR)
      IF(IERR.NE.0) RETURN
      GO TO 300
C   
  220 MO=4
      CALL PPBAN(DELE,IDW,IBN,IDX,IERR)
      IF(IERR.NE.0) RETURN
C   
  300 DO 310 I=1,NXY
      IX(I)=XC(I)+0.5
      JY(I)=YC(I)+0.5
  310 CONTINUE
      NP=NXY
C   
      CALL BANIO(MO,LUB,FILO,TITO,IHO,IBN,DGO,IX,JY,NP,NID,
     &           KHISDAT(1,IDW),MSG,IERR)
C   
      IF(IERR.NE.0) GO TO 520
C   
      RETURN
C   
  500 WRITE(CMSSG,505) KMD
  505 FORMAT(A4,'KMD,ERROR IN CALL TO BANX')
      GO TO 600
C   
  520 continue
      write(6,*) 'Error in banio!'
      WRITE(CMSSG,525)MSG
  525 FORMAT(7A4)
C   
  600 IERR=1
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
