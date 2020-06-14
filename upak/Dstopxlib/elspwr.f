C$PROG ELSPWR    - S(ELE) - Electronic stopping power
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      REAL*4 FUNCTION ELSPWR(EA)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/ARRAYS/  ION(92), IONGS(92), C2S(92),
     &                A0S(10), A1S(10),   A2S(10), A3S(10), A4S(10)
C
      REAL*4          ION,     IONGS,     C2S,
     &                A0S,     A1S,       A2S,     A3S,     A4S
C     ------------------------------------------------------------------
      COMMON/CONSTANT/TMC2, PI, E4, CON(92), DENSITY(92)
      REAL*4          TMC2, PI, E4, CON,     DENSITY
C     ------------------------------------------------------------------
      COMMON/CONTROL/ ISW1,ISW2,ISW3,ISW4
      INTEGER*4       ISW1,ISW2,ISW3
      LOGICAL                        ISW4
C     ------------------------------------------------------------------
      COMMON/PARTICLE/IZT,IZP,ZT,ZP,AT,AP,SPWRH
      INTEGER*4       IZT,IZP
      REAL*4                  ZT,ZP,AT,AP,SPWRH
C     ------------------------------------------------------------------
      COMMON/STOPPING/IAVE,CFACT,C2,A0,A1,A2,A3,A4
      REAL*4          IAVE,CFACT,C2,A0,A1,A2,A3,A4
C     ------------------------------------------------------------------
      REAL*4          EA
C
      INTEGER*4       INDX
C
      REAL*4          SLOW,SHIGH,CZT,ALE,BETA2,SPWR,EX,ZH,ZFACT,B
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ROUTINE TO CALCULATE THE ELECTRONIC STOPPING POWER SE(ZH*)
C     OF HYDROGEN IN ANY MATERIAL USING EQ'S OF ZIEGLER.  STOPPING
C     POWER FOR OTHER IONS ARE THEN GIVEN BY SE(HI)=SE(H)*((ZHI*)/
C     (ZH*))**2 WHERE ZH* AND ZHI* ARE THE EFFECTIVE CHARGES OF
C     HYDROGEN AND THE H.I. RESPECTIVELY.
C     IZT=Z OF TARGET
C     EA=KEV/AMU OF PARTICLE
C     TMC2=2 MC**2 TWICE ELECTRON REST MASS
C     BETA2=(V/C)**2 SQUARE OF LAB. VELOCITY.
C     E4=E**4  E=ELECTRON CHARGE.
C     AI,I=0,1,2,3,4 = APPROXIMATE BETHE SHELL CORRECTION PARAMETERS.
C     CZT=APPROXIMATE BETHE SHELL CORRECTION.
C     IAVE=AVERAGE IONIZATION ENERGY.
C     ------------------------------------------------------------------
C
C     INITIALIZATION
C     ------------------------------------------------------------------
C
      IF(ISW1-IZT)      50,25,50
C
   25 IF(ISW3-IFIX(AT)) 50,100,50
C
   50 ISW1=IZT
      C2=C2S(IZT)
      INDX=(IZT+15)/10
      A0=-A0S(INDX)
      A1= A1S(INDX)
      A2=-A2S(INDX)
      A3= A3S(INDX)
      A4=-A4S(INDX)*1.E-04
C
C     ------------------------------------------------------------------
C     CONVERSION FACTOR TO MEV/(MG/CM**2) (=.6023/A, A=TGT MASS #)
C     ------------------------------------------------------------------
C
      CFACT=.6023/AT
C
C     ------------------------------------------------------------------
C     LOW ENERGY STOPPING USING VARELAS-BIERSACK FORMULA
C     (PAGES 10,16 VOL.3 ANDERSON & ZIEGLER).  FOR HYDROGEN
C     PROJECTILE OF 10-1000 KEV.  ALL COEFFICIENS BUT C2 ARE
C     APPROXIMATED.
C     ------------------------------------------------------------------
C
  100 IF(EA-1000.) 150,150,200
C
  150 SLOW=C2*EA**.45
C
      IF(EA.LT.10.) THEN
      ELSPWR=SLOW*CFACT
      ELSE
C
      SHIGH=(243.-.375*ZT)*ZT/EA*ALOG(1.+500./EA+2.195E-06*EA/IAVE)
      ELSPWR=SLOW*SHIGH/(SLOW+SHIGH)*CFACT
      ENDIF
C
      RETURN
C
C     ------------------------------------------------------------------
C     CALCULATION OF STOPPING POWER USING BETHE STOPPING (SEE
C     VOL. 5 OF ZIEGLER).  STOPPING POWER FOR IDEAL UNIT CHARGE.
C
C     APPROXIMATE CALCULATION OF BETHE SHELL CORRECTION. MAKES
C     <10% CORRECTION OVER 1-30MEV/A.
C     ------------------------------------------------------------------
C
  200 CZT=0.
C
      IF(ISW4)      GO TO 250
C
      IF(EA-40000.) 240,240,250
C
  240 ALE=ALOG(EA)
C
      CZT=A0+(A1+(A2+(A3+A4*ALE)*ALE)*ALE)*ALE
C
  250 BETA2=1.-1./(1.+EA/931189)**2
C
      SPWR=8.*PI*E4*ZT/(BETA2*TMC2)*(ALOG(BETA2*TMC2/((1.-BETA2)*IAVE))
     &  -BETA2-CZT)
C
C     ------------------------------------------------------------------
C     EFFECTIVE CHARGE FOR HYDROGEN.
C     ------------------------------------------------------------------
C
      EX=0.2*SQRT(EA)+.0012*EA+1.443E-05*EA*EA
      ZH=1.
      IF(EX.LT.20.) ZH=1.-EXP(-EX)
C
C     ------------------------------------------------------------------
C     CORRECTION FOR TARGET DEPENDENCE OF EFFECTIVE CHARGE.
C     MAKES <5% CORRECTION OVER 1-2 MEV/A
C     ------------------------------------------------------------------
C
      ZFACT=1.
C
      IF(ISW4)     GO TO 300
C
      IF(EA-1999.) 290,290,300
C
  290 B=1.
C
      IF(IZT.LT.35) B=(ZT-1.)/34.
C
      ZFACT=1.+B*(0.1097-5.561E-05*EA)
C
C     ------------------------------------------------------------------
C     STOPPING POWER OF HYDROGEN
C     ------------------------------------------------------------------
C
  300 ELSPWR=SPWR*ZH*ZH*CFACT*ZFACT
      RETURN
      END
