C$PROG DODEDX    - Executes DEDX command for program STOPO
C
C     ******************************************************************
C     PROGRAM TO COMPUTE S(E) FROM ZIEGLER, NORTHCLIFF & WARD
C     ******************************************************************
C
      SUBROUTINE DODEDX
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4  LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/ST01/ KSORBSAV,KINTARG,LEM,NLST,ITPF,AP,ZP,AT,ZT,EA,EB,DE
      INTEGER*4    NLST,ITPF
      CHARACTER*4  KSORBSAV,KINTARG,LEM
      REAL*4                                      AP,ZP,AT,ZT,EA,EB,DE
C     ------------------------------------------------------------------
      COMMON/ST02/ KSORB
      CHARACTER*4  KSORB
C     ------------------------------------------------------------------
      REAL*4       ELST(40)
C
      CHARACTER*60 CHELP(21)
C
      CHARACTER*4  MF
C
      CHARACTER*53 CTITLS
C
      INTEGER*4    TITLS(13)
C
      EQUIVALENCE (CTITLS,TITLS)
C
      DATA (ELST(I),I=1,38)/
     1 .01,.02,.03,.04,.05,.06,.07,.08,.09,.10,
     2 .20,.30,.40,.50,.60,.70,.80,.90,1.0,2.0,
     3 3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.,20.,30.,
     4 40.,50.,60.,70.,80.,90.,100.,200./
C     ------------------------------------------------------------------
C
C     AP = PROJ (ION) MASS (AMU)
C     ZP = PROJ (ION) ATOMIC NO.
C     AT = TARGET MASS (AMU)
C     ZT = TARGET ATOMIC NO.
C     ------------------------------------------------------------------
C
      EPMEV0=0.1001
      EPKEV0=100.1
      LUT=6
      LUP=7
C
C     ****************************************************************
C     DO THE LOOP ON ENERGY
C     ****************************************************************
C
      KSORB=KSORBSAV
C
      IF(KINTARG.EQ.'H2O ') KSORB='WATE'
      IF(KINTARG.EQ.'MYLA') KSORB='MYLA'
      IF(KINTARG.EQ.'CH2 ') KSORB='CH2 '
C
      EKEV0=AP*EPKEV0
      EKEVA0=4.0*EPKEV0
      EMEVA0=0.001*EKEVA0
      SEWP0=SWARD(AP,ZP,AT,ZT,EKEV0)
      SEZ0 =SZIEGL(AP,ZP,AT,ZT,EKEV0)
      SNORA0=SNORTH(4.0,2.0,AT,ZT,EKEVA0)
      SWARA0=SWARHE(AT,ZT,EMEVA0,IERR)
      SHELF0=SWARA0/SNORA0
C
      CTITLS=' '
      IF(KINTARG.EQ.'    ') WRITE(CTITLS,205)AP,ZP,AT,ZT
      IF(KINTARG.NE.'    ') WRITE(CTITLS,210)AP,ZP,KINTARG
C
  205 FORMAT('AP,ZP,AT,ZT=',4F6.1)
  210 FORMAT('AP,ZP,TARG =',2F6.1,2X,A4)
C
                          WRITE(LUT,212)TITLS
      IF(LISFLG.EQ.'LON ')WRITE(LUP,214)ITPF,TITLS
C
                          WRITE(LUT,216)KSORB
      IF(LISFLG.EQ.'LON ')WRITE(LUP,216)KSORB
C
  212 FORMAT(1H ,'STOPE results for:  ',13A4)
  214 FORMAT(A4,/1H ,'STOPE results for:  ',13A4/)
  216 FORMAT(1H ,'Absorber type = ',A4/)
C
                           WRITE(LUT,220)
      IF(LISFLG.EQ.'LON ') WRITE(LUP,222)
C
  220 FORMAT(1H ,'    EMEV  MEV/A   NUCL  WARZIG    ZIEGL',
     &'    NORTH   CNORTH    SHELF   WARHE4')
C
  222 FORMAT(1H ,'    EMEV  MEV/A   NUCL  WARZIG    ZIEGL',
     &'    NORTH   CNORTH    SHELF   WARHE4',
     &'  T-WARZIG   T-ZIEGL  T-CNORTH'/)
C
      NPL=0
      NBL=0
      KINDE=1
C
      IF(NF.EQ.3) GO TO 240
      KINDE=2
      NE=37
      NLO=1
      EMEV=0.01*AP
C
      IF(LISFLG.EQ.'LOF ') THEN
      EMEV=0.2*AP
      NE=30
      NLO=11
      ENDIF
      GO TO 250
C
  240 NE=(EB-EA)/DE+1.0
      NLO=1
      EMEV=EA
C
  250 NLD=0
      NLN=0
C
      DO 300 I=NLO,NE
C
      EKEV=1000.0*EMEV
      SEWP=SWARD(AP,ZP,AT,ZT,EKEV)
      SEZ=SZIEGL(AP,ZP,AT,ZT,EKEV)
      SEN=SNORTH(AP,ZP,AT,ZT,EKEV)
      SNU=SNUC(AP,ZP,AT,ZT,EKEV)*0.6023/AT
C
      EAKEV=4.0*EKEV/AP
      EAMEV=4.0*EMEV/AP
      SWARA=SWARHE(AT,ZT,EAMEV,IERR)
      SNORA=SNORTH(4.0,2.0,AT,ZT,EAKEV)
      SHELF=SWARA/SNORA
C
      EPN=EMEV/AP
      MF='    '
C
      IF(EPN.GE.EPMEV0) GO TO 260
C
      SHELF=SHELF0
      SWARA=0.0
C
      IF(LEM.NE.'LMON') THEN
                        SEZ=0.0
                        GO TO 260
                        ENDIF
C
      VRAT=SQRT(EPN/EPMEV0)
      SEWP=VRAT*SEWP0
      SEZ =VRAT*SEZ0
      MF='****'
C
  260 SENSC=SHELF*SEN
C
      TSENSC=SENSC+SNU
      TSEWP=SEWP+SNU
      TSEZ=0.0
      IF(SEZ.GT.0.0) TSEZ=SEZ+SNU
      IF(EPN.GT.5.0) TSENSC=0.0
      IF(EPN.GT.12.) SEN=0.0
C
      IF(KSORB.NE.'SOLI') THEN
      SEWP=0.0
      MF='    '
      SEZ=0.0
      SWARA=0.0
      TSEWP=0.0
      TSEZ=0.0
      ENDIF
C
      IF(KINTARG.NE.'    ') THEN
      SNU=0.0
      SEWP=0.0
      MF='    '
      SEZ=0.0
      SENSC=0.0
      SHELF=0.0
      SWARA=0.0
      TSEWP=0.0
      TSEZ=0.0
      TSENSC=0.0
      ENDIF
C
      WRITE(LUT,265)EMEV,EPN,SNU,SEWP,MF,SEZ,MF,SEN,SENSC,SHELF,SWARA
C
      NLN=NLN+1
      IF(NLN.GE.5) THEN
      WRITE(LUT,275)
      NLN=0
      ENDIF
C
      NLD=NLD+1
      IF(NLD.GE.10) THEN
      WRITE(LUT,220)
      NLD=0
      ENDIF
C
      IF(LISFLG.EQ.'LON ') WRITE(LUP,270)
     & EMEV,EPN,SNU,SEWP,MF,SEZ,MF,SEN,SENSC,SHELF,SWARA,
     & TSEWP,MF,TSEZ,MF,TSENSC
C
  265 FORMAT(1H ,F8.2,2F7.3,2(F8.3,A1),F8.3,3F9.3)
C
  270 FORMAT(1H ,F8.2,2F7.3,2(F8.3,A1),F8.3,3F9.3,F10.3,A1,F9.3,A1,
     & F9.3)
C
  275 FORMAT(1H )
C
      IF(LISFLG.NE.'LON ') GO TO 290
C
      NPL=NPL+1
      IF(NPL.GE.53) THEN
      WRITE(LUP,214)TITLS
      WRITE(LUP,220)
      WRITE(LUP,280)
  280 FORMAT(1H )
      NPL=0
      NBL=0
      GO TO 290
      ENDIF
C
      NBL=NBL+1
      IF(NBL.LT.5)  GO TO 290
      WRITE(LUP,280)
      NBL=0
      NPL=NPL+1
C
  290 IF(KINDE.EQ.1) EMEV=EMEV+DE
      IF(KINDE.EQ.2) EMEV=ELST(I+1)*AP
C
  300 CONTINUE
C
      IF(LISFLG.EQ.'LON ') NLST=NLST+1
C
      RETURN
      END
