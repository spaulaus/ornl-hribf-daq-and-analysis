C$PROG CHARGE
C
C     ******************************************************************
C     EXECUTIVE FOR BIBLE OF CHARGE-STATE DISTRIBUTIONS
C     R.O.S.    23 JAN 78
C
C     Interactive VAX version    WTM 10/24/91
C
C
C     MODIFIED TO SKIP CASES FOR WHICH Q0/ZP .LT. 0.07 FOR GASES
C     MODIFIED TO SKIP CASES FOR WHICH Q0/ZP .LT. 0.15 FOR FOILS
C     ******************************************************************
C
      INTEGER*4  IWD(20),LWD(2,40),ITYP(40)
C
      COMMON/RIP1/ LTR,LOU,LSF
      CHARACTER*4          LSF
C
      COMMON/BLK1/ IGAS,JDATE(3),JTIME(2),IZTARG
C
      COMMON/BLK2/ EOA,JEVAL
C
      DIMENSION  EOA(20)
C
      CHARACTER*4  KMI
      EQUIVALENCE  (KMI,IWD(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      LIN=5
      LTR=6
      LOU=7
      LSF='LOF '
      IBELL=Z'07070707'
C
      OPEN(UNIT       = 7,
     &     FILE       = 'charge.log',
     &     STATUS     = 'REPLACE')
C
      CALL MILDATE(JDATE)
      CALL MILTIME(JTIME)
C
      WRITE(LTR,5)
    5 FORMAT(1H ,'Enter LON/LOF to turn output to charge.log ON/OFF'/
     &       1H ,'the default is OFF, charge.log is appended'/)
c
   10 WRITE(LTR,12)IBELL
      WRITE(LTR,14)
   12 FORMAT(1H ,'Type: END, LON, LOF, or -',A4)
   14 FORMAT(1H ,'Enter Projectile-Z,-A, (Target)-Z ------->',$)
C
      READ(LIN,15)IWD
   15 FORMAT(20A4)
      CALL CASEUP(IWD)
C
      IF(KMI.EQ.'END ') STOP
      IF(KMI.EQ.'LON ') THEN
                        LSF='LON '
                        GO TO 10
                        ENDIF
C
      IF(KMI.EQ.'LOF ') THEN
                        LSF='LOF '
                        GO TO 10
                        ENDIF
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 200
C
      CALL MILV(LWD(1,1),IZP,   XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
      CALL MILV(LWD(1,2),IAP,   XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
      CALL MILV(LWD(1,3),IZTARG,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
C
      IF (IZP.LT.6)   IZP = 6
      IF (IAP.LT.12)  IAP = 12
      ZP = IZP
      AP = IAP
      IF (IZTARG.LE.0 .OR. IZTARG.GT.100)   IZTARG = 6
C
   25 EMIN = 0.0
      EMAX = 0.0
      WRITE(LTR,26)
   26 FORMAT(1H ,'Enter MIN & MAX projectile energy (MeV) ->',$)
C
      READ(LIN,15)IWD
      CALL CASEUP(IWD)
      IF(KMI.EQ.'END ') STOP
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 300
      IF(NF.LE.0)   GO TO 300
C
      CALL MILV(LWD(1,1),IV,EMIN,KIND,IERR)
      IF(IERR.NE.0) GO TO 300
      CALL MILV(LWD(1,2),IV,EMAX,KIND,IERR)
      IF(IERR.NE.0) GO TO 300
C
      IF (EMIN.LT.1.0)   THEN
                         WRITE(LTR, 32)
   32                    FORMAT(' Error: MIN projectile energy < 1 MeV')
                         GO TO 25
                         ENDIF
      JEVAL = 11
      EDEL = 0.10 * (EMAX - EMIN)
      IF (EDEL.LT.2.0)   THEN
                         JEVAL = 6
                         EDEL = EDEL + EDEL
                         ENDIF
      IF (EDEL.LT.2.0)   THEN
                         JEVAL = 3
                         EDEL = 2.5 * EDEL
                         ENDIF
      IF (EDEL.LT.2.0)   JEVAL = 1
      E = EMIN - EDEL
      DO 40  I=1,JEVAL
      E = E + EDEL
      EOA(I) = E / AP
   40 CONTINUE
C
      IGAS = 0
      CALL  AZPAGE (ZP,AP)
      IF (IZTARG.EQ.6)   GO TO 10
      WRITE(LTR,45)
   45 FORMAT(1H ,'Hit [RETURN] to see results for gas stripper ->',$)
C
      READ(5,80) IDO
   80 FORMAT(A4)
      IGAS = 1
      CALL  AZPAGE (ZP,AP)
      GO TO 10
C
  200 WRITE(LTR,210)IBELL
  210 FORMAT(1H ,'SYNTAX ERROR OR ILLEGAL VALUE - TRY AGAIN!',A4/)
      GO TO 10
C
  300 WRITE(LTR,210)IBELL
      GO TO 25
      END
C$PROG AZPAGE
      SUBROUTINE AZPAGE(ZP,AP)
C
C     *************************************************************
C     PRODUCES A PAGE OF CHARGE STATE DISTRIBUTIONS FOR
C     ZP, AP  ON A GRID OF EP/AP VALUES
C     R.O.S.        3 MAY 77
C
C     STRASB COMPUTES CHARGE FRACTIONS FQ FOR GIVEN Q0,RHO,EPS VALUES.
C     SEMPIR COMPUTES  Q0, RHO, EPS  FOR GIVEN  ZP,AP E/A VALUES.
C
C     FCUT =    MINIMUM VALUE OF F CONSIDERED
C     IQI  =    INITIAL Q VALUE COMPUTED -- F(Q(1)
C     IQF  =    FINAL Q VALUE
C     LOW  =    ARRAY OF IQI FOR VARIOUS E/A VALUES
C     LHI  =    ARRAY OF IQF VALUES
C     *************************************************************
C
      COMMON/RIP1/ LTR,LOU,LSF
      CHARACTER*4          LSF
C
      COMMON/BLK1/ IGAS,JDATE(3),JTIME(2),IZTARG
C
      COMMON/BLK2/ EOA,JEVAL
C
      DIMENSION  F(100,20), EOA(20), LOW(20), LHI(20), LZ(100)
      DIMENSION  QBAR(20), WID(20), SKEW(20)
      DIMENSION  QNOT(20), BOAT(20), EPSOM(20)
      DIMENSION  FQ(50), FG(50)
C
Crlv    INTEGER*4  ALAB(3),BLAB(3),FMU(5),FSKIP(20)
      CHARACTER*4  ALAB(3),BLAB(3),FMU(5),FSKIP(20)
C
      CHARACTER*4 FMTE(10),ITR(11),ITL(11)
C
      CHARACTER*44 CITR,CITL
      EQUIVALENCE (CITR,ITR),(CITL,ITL)
C
      CHARACTER*40 CLZ(10),CFMTE
      EQUIVALENCE (CLZ,LZ),(CFMTE,FMTE)
C
      DATA CFMTE/'(6H   E =   TR66, 4H MeV    TL70,11F6.1)'/
C
      DATA  FMU /'(1H ',',I5,','    ','20F6','.2) '/
C
      DATA  FSKIP /'    ',' 6X,','12X,','18X,','24X,','30X,','36X,',
     &         '42X,','48X,','54X,','60X,','66X,','72X,','78X,','84X,',
     &         '90X,','96X,','102X','108X','114X' /
C
      DATA CLZ/'   H  He  Li  Be   B   C   N   O   F  Ne',
     &         '  Na  Mg  Al  Si   P   S  Cl  Ar   K  Ca',
     &         '  Sc  Ti   V  Cr  Mn  Fe  Co  Ni  Cu  Zn',
     &         '  Ga  Ge  As  Se  Br  Kr  Rb  Sr   Y  Zr',
     &         '  Nb  Mo  Tc  Ru  Rh  Pd  Ag  Cd  In  Sn',
     &         '  Sb  Te   I  Xe  Cs  Ba  La  Ce  Pr  Nd',
     &         '  Pm  Sm  Eu  Gd  Tb  Dy  Ho  Er  Tm  Yb',
     &         '  Lu  Hf  Ta   W  Re  Os  Ir  Pt  Au  Hg',
     &         '  Tl  Pb  Bi  Po  At  Rn  Fr  Ra  Ac  Th',
     &         '  Pa   U  Np  Pu  Am  Cm  Bk  Cf  Es  Fm'/
C
      DATA  ALAB,BLAB /'CARB','ON  ','FOIL','DILU','TE G','ASES'/
C
      DATA CITR/'TR6 TR12TR18TR24TR30TR36TR42TR48TR54TR60TR66'/
      DATA CITL/'TL10TL16TL22TL28TL34TL40TL46TL52TL58TL64TL70'/
C
      INTEGER*4 ITPF
C
      DATA ITPF/Z'0C202020'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      FMU(3) = FSKIP(1)
      FCUT = 0.0001
      IZP = ZP + 0.0001
      JZP = IZP
      IAP = AP + 0.0001
      LSKIP = 0
C
      DO 40  J=1,JEVAL
      EDA = EOA(J)
      EOA(J) = EOA(J) * AP
      CALL  SEMPIR (IGAS, ZP,AP, EDA,BETA, Q0,RHO,EPS)
      IF (Q0.GT.0.0)   GO TO 200
      QB = 0.0
      D  = 0.0
      S  = 0.0
      IQI= 0
      IQF= -1
      FQ(1) = 0.0
      FG(1) = 0.0
      GO TO 28
  200 CONTINUE
      IF (Q0/ZP.LT.1.0)   GO TO 250
      Q0 = 0.
      QB = 0.
      D  = 0.
      S  = 0.
      IQI = ZP + 1.
      IQF = ZP + 1.
      FQ(1) = 0.0
      FG(1) = 0.0
      IF(J.EQ.1) THEN
                        WRITE(LTR,240) EOA(1)
      IF(LSF.EQ.'LON ') WRITE(LOU,240) EOA(1)
  240 FORMAT(' *** S.E. formula range exceeded by EMIN=',F6.1)
      EOA(1) = EOA(1) / AP
      RETURN
                  ENDIF
      GO TO 28
  250 CONTINUE
C
      CALL  STRASB (FCUT, QB,D,S, Q0, RHO, EPS, IQI,IQF, FQ, FG)
C
      IF (IQI.GE.0)   GO TO 28
      IADD = - IQI
      KP = JEVAL - IADD
      IF (KP.GT.0)   THEN
                     DO 27  K=1,KP
                     FQ(K) = FQ(K+IADD)
   27                CONTINUE
                     ENDIF
      IQI = 0
   28 CONTINUE
C
      LMAX = IQF - IQI + 1
      LOW(J) = IQI
      LHI(J) = IQF
      QBAR(J) = QB
      WID(J)  = D
      SKEW(J) = S
      QNOT(J) = Q0
      BOAT(J) = RHO
      EPSOM(J) = EPS
C
      K = 0
      I1 = IQI + 1
      I2 = IQF + 1
      DO 30  I=I1,I2
      K = K + 1
      F(I,J) = 100. * FQ(K)
   30 CONTINUE
   40 CONTINUE
C
C     *************************************************************
C     PRINT HEADER
C     CALCULATE CUTOFF FOR HEADER PRINTOUT, LCUT
C     *************************************************************
C
      LZP = ZP
      LCUT = JEVAL
      DO 45  L=1,JEVAL
      IF (LOW(L).LE.LZP)  GO TO 45
      LCUT = L - 1
      GO TO 50
   45 CONTINUE
   50 CONTINUE
C
C         SET MAXIMUM Q VALUE FOR PRINTOUT
C
      IF (LHI(LCUT).LT.IZP)   IZP = LHI(LCUT)
C
      IF(IGAS.NE.0)    GO TO 52
C
      IF (IZTARG.NE.6) THEN
C
      IF(LSF.EQ.'LON ')WRITE(LOU,55)ITPF,JDATE,JTIME,LZ(JZP),IAP,
     &                              LZ(IZTARG),FSKIP(1),ALAB(3)
C
                       WRITE(LTR,55)ITPF,JDATE,JTIME,LZ(JZP),IAP,
     &                              LZ(IZTARG),FSKIP(1),ALAB(3) 
      GO TO 60
                       ENDIF
C
   52 IF(LSF.EQ.'LON '.AND.IGAS.NE.0) WRITE(LOU,55)ITPF,JDATE,JTIME,
     &                                             LZ(JZP),IAP,BLAB
C
      IF(IGAS.NE.0)                   WRITE(LTR,55)ITPF,JDATE,JTIME,
     &                                             LZ(JZP),IAP,BLAB
C
      IF(LSF.EQ.'LON '.AND.IGAS.EQ.0) WRITE(LOU,55)ITPF,JDATE,JTIME,
     &                                             LZ(JZP),IAP,ALAB
C
      IF(IGAS.EQ.0)                   WRITE(LTR,55)ITPF,JDATE,JTIME,
     &                                             LZ(JZP),IAP,ALAB
C
   55 FORMAT(A4/,1H ,3A4,1X,2A4,//
     &1H ,'EQUILIBRIUM CHARGE STATE PREDICTIONS for ',A4,'-',I3,
     &         ' Ions in: ',3A4)
C
   60 FMTE(4) = 'TR66'
      FMTE(8) = 'TL70'
      IF (LCUT.GT.0 .AND. LCUT.LT.12)THEN
                                     FMTE(4) = ITR(LCUT)
                                     FMTE(8) = ITL(LCUT)
                                     ENDIF
C
      IF(LSF.EQ.'LON ') THEN
                        WRITE(LOU,78)
                        WRITE(LOU,FMTE)(EOA(L),L=1,LCUT)
                        ENDIF
C
                        WRITE(LTR,78)
                        WRITE(LTR,FMTE)(EOA(L),L=1,LCUT)
C
      DO 70  J=1,JEVAL
      EOA(J)=EOA(J)/AP
   70 CONTINUE
C
      IF(LSF.EQ.'LON ') WRITE(LOU,80)(EOA(L),L=1,LCUT)
      IF(LSF.EQ.'LON ') WRITE(LOU,85)(QNOT(L),L=1,LCUT)
      IF(LSF.EQ.'LON ') WRITE(LOU,86)(BOAT(L),L=1,LCUT)
      IF(LSF.EQ.'LON ') WRITE(LOU,87)(EPSOM(L),L=1,LCUT)
      IF(LSF.EQ.'LON ') WRITE(LOU,90)(QBAR(L),L=1,LCUT)
      IF(LSF.EQ.'LON ') WRITE(LOU,93)
                        WRITE(LTR,93)
C
   78 FORMAT(1H )
   80 FORMAT(1H ,/1H ,'E/A =',20F6.2)
   84 FORMAT(1H ,     ' Q0 =',20F6.1)
   85 FORMAT(1H ,/1H ,' Q0 =',20F6.1)
   86 FORMAT(1H ,     'RHO =',20F6.2)
   87 FORMAT(1H ,     'EPS =',20F6.2)
   90 FORMAT(1H ,/1H ,'QAVG=',20F6.1)
   93 FORMAT(1H ,'Q =')
C
C     *************************************************************
C     LOW(1) = MINIMUM Q OF INTEREST
C     MQ = CURRENT Q VALUE
C     *************************************************************
C
      J = 0
C
      MQ =  - 1
C
  100 MQ = MQ + 1
      NUP = -1
C
      DO 110  L=1,JEVAL
      NUP = NUP + 1
      IF (LOW(L).GT.MQ)   GO TO 120
  110 CONTINUE
      NUP = JEVAL
C
  120 N1 = 0
      DO 130  L=1,JEVAL
      N1 = N1 + 1
      IF (LHI(L).GE.MQ)   GO TO 140
  130 CONTINUE
  140 ISK = N1
C
      IF (ISK.GT.0.AND.ISK.LT.21)   FMU(3) = FSKIP(ISK)
      MQP = MQ + 1
      IF (MQP.LE.0)   MQP = 1
C
      IF (MQ.NE.41)   GO TO 145
      IF (IGAS.EQ.1.AND.ZP.EQ.45.)   GO TO 145
C
      IF(LSF.EQ.'LON ') WRITE(LOU,142)
C
  142 FORMAT(1H ,/)
  145 CONTINUE
C
      IF(LSF.EQ.'LON '.AND.NUP.GE.N1) WRITE(LOU,FMU)MQ,
     &                                (F(MQP,N), N=N1,NUP)
C
      IF(NUP.GE.N1)                   WRITE(LTR,FMU)MQ,
     &                                (F(MQP,N),N=N1,NUP)
C
      LSKIP = LSKIP + 1
      IF (LSKIP.NE.6)   GO TO 160
      LSKIP = 1
C
      IF(LSF.EQ.'LON ') WRITE(LOU,150)
                        WRITE(LTR,150)
C
  150 FORMAT(1H )
C
  160 IF(MQ.LT.IZP) GO TO 100
C
C     *************************************************************
C
      WRITE(LTR,150)
      WRITE(LTR,84) (QNOT(L), L=1,LCUT)
      WRITE(LTR,170)(QBAR(L), L=1,LCUT)
C
  170 FORMAT(' QAVG=',20F6.1)
C
      IF(LSF.EQ.'LON ') WRITE(LOU,80)(EOA(L),L=1,LCUT)
C
      DO 180  J=1,JEVAL
  180 EOA(J) = AP * EOA(J)
C
      IF(LSF.EQ.'LON ') THEN
                        WRITE(LOU,78)
                        WRITE(LOU,FMTE)(EOA(L),L=1,LCUT)
                        ENDIF
C
                        WRITE(LTR,78)
                        WRITE(LTR,FMTE)(EOA(L),L=1,LCUT)
C
      DO 190  J=1,JEVAL
  190 EOA(J)=EOA(J)/AP
      RETURN
      END
C$PROG MOMQ
      SUBROUTINE MOMQ(Q0,RHO,EPS,FMAX,QBAR,D,S)
C
C     ******************************************************************
C     CALCULATE FIRST 3 MOMENTS OF CHARGE STATE DISTRIBUTION
C     FROM Q0, RHO, EPS.
C     ROS   2 MAY 77
C     ******************************************************************
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DT = 0.05
      TMAX = 8.
      CALL TINT (DT, TMAX, EPS, T0,T1, T2, T3)
      FMAX = T0 * RHO
      IF (FMAX.GT.0.0001)   FMAX = 1./FMAX
C
      QBAR = Q0 + RHO * T1
      D = RHO * SQRT (T2 - T1*T1)
      S = T3 - T1 * (3.*T2 - 2.*T1*T1)
      S = S * (RHO/D)**3
      RETURN
      END
C$PROG SEMPIR
      SUBROUTINE SEMPIR(IGAS,ZP,AP,EDA,BETA,Q0,RHO,EPS)
C
C     ******************************************************************
C     COMPUTE Q0,RHO,EPS  USING BEST SEMI-EMPIRICAL FORMULAS
C     R.O.S. 22 APR 78
C
C     DILUTE   FOR Q0/Z < 0.07    USE AD HOC FORUMLA DEDUCED FROM LOW
C     GASES         ENERGY DATA FOR BR-79,I-127,XE-131,TA-181
C			   AND U-238 IONS.
C
C     IF  Q0/Z > YMAX   SET  Q0 = 100.
C
C     REQUIRE PROJECTILE ENERGY  >  1.0 MEV;  OTHERWISE  Q0 = 0
C     ******************************************************************
C
      COMMON/BLK1/ IDUM,JDATE(3),JTIME(2),IZTARG
C
      SAVE
C
C     ------------------------------------------------------------------
C
      EMEV = AP * EDA
      IF (EMEV.GE.1.0)   GO TO 5
      Q0 = 0.0
      RHO = 0.0
      EPS = 0.0
      RETURN
    5 CONTINUE
C
      X = EDA/931.48
      XP2 = X + 2.
      BETA = SQRT (X * XP2 / (1. + X * XP2) )
C
      IF (IGAS.NE.0)   GO TO 100
C
C		CARBON FOIL
      YMIN = 0.1
      YCUT = 0.15
      YMAX = 0.95
C
      DK = 1.03
      A  = 47.3
      GAM= 0.860
      ALP = -.38
      AR  = 0.480
      ALPR= 0.450
      GAMR= 0.26
      IF (ZP.GT.15.0)   GO TO 20
      DK = 1.0
      A  = 55.8
      ALP= -0.421
      GAM= 0.89
      AR = 0.378
      ALPR= 0.507
      GAMR= 0.20
   20 CONTINUE
C
      Y = 1. - DK * EXP(-A * ZP**ALP * BETA**GAM)
      IF (Y.GT.YMIN)   GO TO 50
      Q0 = 0.
      RHO = 0.
      EPS = 0.
      RETURN
   50 CONTINUE
      IF (Y.LT.YCUT)   EDA = - EDA
C
      IF (Y.LT.YMAX)   GO TO 70
      Q0 = 100.
      RHO= 0.
      EPS= 0.
      RETURN
   70 CONTINUE
C                                     Z target dependence according
C                                     to Shima, et al., NIM 200, 605
C                                     (1982).
      IF (IZTARG.NE.6)   THEN
                      ZFAC = FLOAT(IZTARG) - 6.
                      ZFAC = ZFAC * SQRT (83.33*BETA/ZP**0.45)
                      Y = Y * (1. - 0.0019*ZFAC + 1.E-5 * ZFAC**2)
                      ENDIF
      Q0 = ZP * Y
      RHO = AR * ZP**ALPR * (Y*(1.-Y))**GAMR
      EPS = 0.000001
      IF (ZP.GT.15.)      EPS = RHO * (0.0007 * ZP - 0.7 * BETA)
      RETURN
C
C		DILUTE GASES
C
  100 CONTINUE
      YMIN = 0.03
      YCUT = 0.07
      YMAX = 0.90
      DK = 1.08
      ALP = -.506
      A   = 80.1
      GAM = 0.996
C
      IF (ZP.GT.15.)  GO TO 120
      YMAX = 0.95
      ALP  = -0.530
      A    = 86.8
      GAM  = 0.992
  120 CONTINUE
C
      AR  = 0.35
      ALPR= .55
      GAMR= .27
      AE  = .17
      ALPE= .0012
      GAME= -3.3
C
      Y = 1. - DK * EXP (-A * ZP**ALP * BETA**GAM)
      IF (Y.GE.YCUT)   GO TO 150
C
C	RE-COMPUTE  Y  USING AD HOC FORMULA FOR LOW IONIZATION REGION
C
      DK = 1.02
      A  = 49.5
      Y  = 1. - DK * EXP (-A * ZP**ALP * BETA**GAM)
C
      IF (Y.GT.YMIN)   GO TO 150
      Q0 = 0.
      RHO = 0.
      EPS = 0.
      RETURN
  150 CONTINUE
C
      IF (Y.LT.YMAX)   GO TO 170
      Q0 = 100.
      RHO= 0.
      EPS= 0.
      RETURN
  170 CONTINUE
C
      Q0 = ZP * Y
      IF (Y.LT.YCUT)   EDA = - EDA
      RHO = AR * ZP**ALPR * (Y*(1.-Y))**GAMR
      EPS = RHO * (AE + ALPE * ZP + GAME * BETA)
C
      RETURN
      END
C$PROG STRASB
      SUBROUTINE STRASB(FCUT,QB,D,S,Q0,RHO,EPS,JLO,JHI,FQ,FG)
C
C     ******************************************************************
C     14 SEP 1977    R. O. SAYER
C
C     FCUT = MIN. VALUE OF F CONSIDERED
C     FQ  = CHARGE FRACTIONS FOR SKEWED GAUSSIAN
C     FG  =   "       "       "  SYMMETRIC   "
C     QB  = AVERAGE CHARGE
C     D   = WIDTH (2ND MOMENT OF DISTRIBUTION)
C     S   = SKEWNESS
C
C     Q0  = MOST PROBABLE CHARGE
C     RHO = WIDTH PARAMETER FOR ROS DIST. FUNCTION
C     EPS = SKEWNESS PARAMETER FOR ROS DIST. FUNCTION
C
C     JLO = MINIMUM Q VALUE -- F(1)
C     JHI = MAXIMUM Q VALUE
C     ******************************************************************
C
      DIMENSION  FQ(50), FG(50)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      JBIG = 50
      RPI2 = 1.2533141
      IF (FCUT.EQ.0.0)   FCUT = .0001
C
      CALL  MOMQ (Q0,RHO,EPS, FMAX, QB,D,S)
      FGMAX = 0.5 / (RPI2 * D)
C
C     FIND LOWEST Q VALUE FOR F > FCUT
C
      FTEST =  - ALOG(FCUT/FMAX)
      ETEST = - 100000.
      IF (ABS(EPS).GT.0.00001)   ETEST = -1. / EPS
      JQ = Q0
C
   10 T = (JQ -Q0)/RHO
      IF (T.LT.ETEST.AND.EPS.GT.0.0)   GO TO 20
      FLOW = 0.5 * T*T / (1.+EPS*T)
      IF (FLOW.GT.FTEST)   GO TO 20
      JQ = JQ - 1
      GO TO 10
   20 JLO = JQ + 1
C
C      FIND LARGEST Q VALUE FOR F > FCUT
C
      JQ = Q0 + 1
   30 T = (JQ-Q0)/ RHO
      IF (T.GT.ETEST.AND.EPS.LT.0.0)   GO TO 35
      FHI = 0.5 * T*T / (1.+EPS*T)
      IF (FHI.GT.FTEST)   GO TO 35
      JQ = JQ + 1
      GO TO 30
   35 JHI = JQ - 1
C
      JMAX = JHI - JLO + 1
      IF (JMAX.LE.JBIG)   GO TO 38
      JMAX = JBIG
      JHI = JLO + JMAX - 1
      PRINT 37, JBIG, JLO, JHI
   37 FORMAT('0 JMAX SET =',I5, '  JLO,JHI =',2I4)
   38 CONTINUE
C
      DO 40  J=1,JMAX
      QJ = JLO - 1 +J
      T = (QJ - Q0)/RHO
      TFAC = -0.5 * T*T / (1.+EPS*T)
      FQ(J) = FMAX * EXP (TFAC)
      QFAC = (QJ - QB)**2
      FG(J) = FGMAX * EXP (-0.5*QFAC/D**2)
   40 CONTINUE
C
      RETURN
      END
C$PROG TINT
      SUBROUTINE TINT(DT,TMAX,EPS,T0,T1,T2,T3)
C
C     ******************************************************************
C     COMPUTE INTEGRALS FOR MOMENT CALCULATION FOR THE DISTRIBUTION:
C
C     F / FMAX  =  EXP (-0.5*T**2 / (1 + EPS * T) )
C
C     TLO = LOWER LIMIT OF INTEGRAL
C     THI = UPPER LIMIT OF INTEGRAL
C     ******************************************************************
C
      SAVE
C
C     ------------------------------------------------------------------
C
      THI = TMAX
      TLO = - TMAX
      ZIP = 0.01 * TMAX
      ECHEK = ABS(EPS)
      IF (ECHEK.LT.ZIP)   GO TO 20
      TLO = -1./EPS + DT
      IF (EPS.GT.0.)   GO TO 20
      THI = TLO - DT - DT
      TLO = - TMAX
   20 CONTINUE
C
      NPTS = (THI-TLO)/DT
      T0 = 0.
      T1 = 0.
      T2 = 0.
      T3 = 0.
      T = TLO - DT
C
      DO 40  N=1,NPTS
      T = T + DT
      TSQ = T*T
      ARG = 0.5 * TSQ / (1.+EPS*T)
      IF (ARG.LT.40.)   GO TO 25
      IF (T)  40,40, 60
   25 TVAL = EXP(-ARG)
      IF (N.EQ.1)   TVAL = 0.5 * TVAL
      T0 = T0 + TVAL
      T1 = T1 + T * TVAL
      T2 = T2 + TSQ * TVAL
      T3 = T3 + T*TSQ * TVAL
   30 FORMAT(I4, F8.2, 1PE10.3, 2X, 4E11.3)
   40 CONTINUE
   60 T1 = T1/T0
      T2 = T2/T0
      T3 = T3/T0
      T0 = DT * T0
      RETURN
      END
