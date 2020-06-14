C$PROG STOPIT    - S(ELE) using Northcliff's tables & formulas
C   
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE STOPIT(EPA,DEP,NDU,ZP,MP,ZTARG,STOP,KSORB)
C
      REAL*4 MP
C
      CHARACTER*4  KSORB,KINDAB(5)
C
      INTEGER*4 IBUF(192),KON(18)
C
      REAL*4    XBUF(192),STOP(600)
C
      REAL*4    XVAL(76,3),YVAL(76,3),YVALMD(76),ANAME(3),CHGA(3)
C
      REAL*4    EAMU(38),ZTG(12),ZT(12),ZTSAV(12),AMUA(3)
C
      EQUIVALENCE (A     ,KON(1)),
     &            (B     ,KON(2)),
     &            (C     ,KON(3)),
     &            (D     ,KON(4)),
     &            (E     ,KON(5)),
     &            (ICODE ,KON(6)),
     &            (AMUI  ,KON(7)),
     &            (BNAME ,KON(8)),
     &            (CHGI  ,KON(9)),
     &            (AG    ,KON(10)),
     &            (BG    ,KON(11)),
     &            (CG    ,KON(12)),
     &            (DG    ,KON(13)),
     &            (EG    ,KON(14)),
     &            (ICODG ,KON(15)),
     &            (AMUIG ,KON(16)),
     &            (BNAMEG,KON(17)),
     &            (CHGIG ,KON(18))
C
      EQUIVALENCE (IBUF,YVAL),(XBUF,YVAL)
C
      DATA  ZTG/1.,4.,7.,8.,10.,18.,36.,54.,86.,0.,0.,0./
C
      DATA  ZT/4.,6.,13.,22.,28.,32.,40.,47.,63.,73.,79.,92./
C
      DATA  ZTSAV/4.,6.,13.,22.,28.,32.,40.,47.,63.,73.,79.,92./
C
      DATA  EAMU/.0125,.016,.02,.025,.032,.04,.05,.06,.07,.08,.09,
     & .1,.125,.16,.2,.25,.32,.4,.5,.6,.7,.8,.9,1.,1.25,1.6,2.,2.5,
     & 3.2,4.,5.,6.,7.,8.,9.,10.,11.0,12.0/
C
      DATA SLOPE,JMOD/0,0/
C
      DATA KINDAB/'SOLI','GAS ','MYLA','CH2 ','WATE'/
C
      DATA LU,NCALL/1,0/
C
C     ****************************************************************
C     READ IN THE RATIO CARDS FOR 24 ABSORBERS AND THE COEFFICIENT
C     CARDS FOR AS MANY IONS AS DESIRED (TWO CARDS FOR EACH ION)
C     ****************************************************************
C
      IF(NCALL.GT.0) GO TO 10
C
      CALL REDNORTH(LU)
      NCALL=1
C
   10 NCARDS=0
      M=1
C
      DO 20 L=1,3
      XVAL(1,L)=EAMU(1)
      DO 15 I=3,75,2
      J=(I-1)/2+1
      XVAL(I,L)=EAMU(J)
   15 CONTINUE
   20 CONTINUE
C
      IF(KSORB.EQ.'SOLI') GO TO 60
      IF(KSORB.EQ.'GAS ') GO TO 70
      IF(KSORB.EQ.'MYLA') GO TO 130
      IF(KSORB.EQ.'CH2 ') GO TO 140
      IF(KSORB.EQ.'WATE') GO TO 150
C
      WRITE(6,40)KSORB
   40 FORMAT(1H ,'KSORB=',2X,A4)
      STOP 111
C
C
   60 JMAX=12                                  !SOLID TARGET
      JAKOF=0
      DO 65 I=1,12
      ZT(I)=ZTSAV(I)
   65 CONTINUE
      GO TO 90
C
   70 JMAX=9                                   !GAS   TARGET
      JAKOF=12
      DO 80 I=1,12
      ZT(I)=ZTG(I)
   80 CONTINUE
C
   90 DO 100 J=2,JMAX
      IF(ZT(J).EQ.ZTARG) GO TO 120
      IF(ZT(J).GT.ZTARG) GO TO 110
  100 CONTINUE
C
  110 IF(ZTARG.LT.((ZT(J)+ZT(J-1))/2)) J=J-1
C
  120 IF(J.LT.2) J=2
      IF(J.GE.JMAX) J=JMAX-1
      NSKIP=JAKOF+J-2
      GO TO 160
C
  130 NSKIP=21                                 !MYLAR TARGET
      GO TO 160
C
  140 NSKIP=24                                 !CH2   TARGET
      GO TO 160
C
  150 NSKIP=27                                 !H2O   TARGET
C
  160 NWN=41*NSKIP+1
      ISEC=(NWN-1)/64
      N=NWN-64*ISEC-1
C
      CALL BUFLOD(IBUF,768,ISEC)
C
      DO 180 L=1,3
      DO 170 J=2,76,2
      N=N+1
      XVAL(J,L)=XBUF(N)
  170 CONTINUE
      CHGA(L)=XBUF(N+1)
      AMUA(L)=XBUF(N+2)
      ANAME(L)=XBUF(N+3)
      N=N+3
  180 CONTINUE
C
      KK=1
      NREAD=ZP+0.5
      LL=37
      NWN=30*41+18*(NREAD-1)+1
      ISEC=(NWN-1)/64
      N=NWN-64*ISEC
C
      CALL BUFLOD(IBUF,512,ISEC)
C
      DO 190 J=1,18
      KON(J)=IBUF(N)
      N=N+1
  190 CONTINUE
C
      IAMUI=AMUI
      AAMUI=IAMUI
      IF(AMUI.GE.(AAMUI+0.5)) IAMUI=IAMUI+1
C
C     ****************************************************************
C     FIND THE STOPPING POWER IN ALUMINUM AS A FUNCTION
C     OF THE ENERGY PER NUCLEON
C     ****************************************************************
C
  220 IF(LL.NE.75) GO TO 230
      A=AG
      B=BG
      C=CG
      D=DG
      E=EG
      ICODE=ICODG
      AMUI=AMUIG
      BNAME=BNAMEG
      CHGI=CHGIG
C
  230 A3=1.0/(2.0*E)
      A1=-D*A3
      A2=-C*A3
      A4=C*C-4.0*E*A
      A5=2.0*C*D-4.0*E*B
      A6=D*D+4.0*E
C
      DO 310 I=KK,LL,2
      XX=XVAL(I,1)
      X=ALOG10(XX*100.0)
C
      IF(ICODE.LE.1) GO TO 250
C
      Y=A1+A2*X+A3*SQRT((A4*X+A5)*X+A6)
      GO TO 260
  250 Y=A1+A2*X-A3*SQRT((A4*X+A5)*X+A6)
  260 EXX=Y-2.0
      YVALMD(I)=(10.0**EXX)
C
      IF(I.LE.1) GO TO 310
C
      XX=(XVAL(I,1)+XVAL(I-2,1))*0.5
      X=ALOG10(XX*100.0)
C
      IF(ICODE.LE.1) GO TO 290
C
      Y=A1+A2*X+A3*SQRT((A4*X+A5)*X+A6)
      GO TO 300
  290 Y=A1+A2*X-A3*SQRT((A4*X+A5)*X+A6)
  300 EXX=Y-2.0
      YVALMD(I-1)=(10.0**EXX)
  310 CONTINUE
C
      IF(KK.LT.1) GO TO 490
      IF(KK.GT.1) GO TO 330
C
      KK=39
      LL=75
      GO TO 220
C
C     ****************************************************************
C     MODIFY THE SLOPE OF THE INITIAL PORTION OF EACH
C     ALUMINUM STOPPING POWER CURVE
C     ****************************************************************
C
  330 YMOD=-23.3394+SQRT(106.*106.-(100.-CHGI)*(100.-CHGI))
      EMOD=(10.**(YMOD/50.))/100.
C
      DO 350 JMD=1,75,2
      IF(XVAL(JMD,1).LT.EMOD) GO TO 350
      JMOD=JMD
      GO TO 360
  350 CONTINUE
C
  360 IF(CHGI.GE.20.) SLOPE=.43865+SQRT(.04202-(CHGI/1000.-.21402)**2)
C
      IF(CHGI.LT.20.) SLOPE=0.5
      XINCPT=ALOG10(YVALMD(JMOD))-SLOPE*ALOG10(XVAL(JMOD,1))
      KMOD=JMOD-2
C
      DO 370 JMD=1,KMOD,2
      YVALMD(JMD)=10.**(SLOPE*ALOG10(XVAL(JMD,1))+XINCPT)
      XMOD=(XVAL(JMD,1)+XVAL(JMD+2,1))*0.5
      YVALMD(JMD+1)=10.**(SLOPE*ALOG10(XMOD)+XINCPT)
  370 CONTINUE
C
C     ****************************************************************
C     MULTIPLY ALUMINUM STOPPING POWER CURVE
C     BY EACH ABSORBER RATIO CURVE
C     ****************************************************************
C
      DO 385 L=1,3
      YVAL(1,L)=YVALMD(1)*XVAL(2,L)
      DO 380 I=3,75,2
      YVAL(I,L)=YVALMD(I)*XVAL(I+1,L)
      YVAL(I-1,L)=YVALMD(I-1)*(XVAL(I+1,L)+XVAL(I-1,L))*0.5
  380 CONTINUE
  385 CONTINUE
C
C     ****************************************************************
C     ELIMINATE THE 1/Z SQ FACTOR FROM THE STOPPING POWER
C     ****************************************************************
C
      AAA=CHGI*CHGI
      DO 395 L=1,3
      DO 390 I=1,75
      YVAL(I,L)=YVAL(I,L)*AAA
  390 CONTINUE
  395 CONTINUE
      IZ=CHGI
C
C     ****************************************************************
C     CALCULATE ENERGY LOSS USING THE STOPPING POWER
C     ****************************************************************
C
      EBOMB=EPA
      DO 480 KS=1,NDU
      EPAMU=EBOMB/MP
C
      DO 400 I=3,75,2
      IF(XVAL(I,1).EQ.EPAMU) GO TO 420
      IF(XVAL(I,1).GT.EPAMU) GO TO 410
  400 CONTINUE
C
  410 IF(EPAMU.LT.((XVAL(I,1)+XVAL(I-2,1))/2)) I=I-2
C
  420 I2=I
      IF(I2.LT.3)  I2=3
      IF(I2.GT.73) I2=73
      I1=I2-2
      I3=I2+2
C
      DO 430 I=1,5
      IF(KSORB.EQ.KINDAB(I)) GO TO 440
  430 CONTINUE
C
  440 IGT=I
C
      GO TO (450,450,460,460,460), IGT
C
  450 R1=FIT3(CHGA(1),CHGA(2),CHGA(3),YVAL(I1,1),YVAL(I1,2),
     &        YVAL(I1,3),ZTARG)
      R2=FIT3(CHGA(1),CHGA(2),CHGA(3),YVAL(I2,1),YVAL(I2,2),
     &        YVAL(I2,3),ZTARG)
      R3=FIT3(CHGA(1),CHGA(2),CHGA(3),YVAL(I3,1),YVAL(I3,2),
     &        YVAL(I3,3),ZTARG)
      GO TO 470
C
  460 R1=YVAL(I1,1)
      R2=YVAL(I2,1)
      R3=YVAL(I3,1)
C
  470 RT=FIT3(XVAL(I1,1),XVAL(I2,1),XVAL(I3,1),R1,R2,R3,EPAMU)
      STOP(KS)=RT
      EBOMB=EBOMB+DEP
  480 CONTINUE
C
      RETURN
C
  490 STOP
      END
