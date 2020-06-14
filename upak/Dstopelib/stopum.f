C$PROG STOPUM    - S(ELE+NUC) (EV/(10**15) atoms/cmsq) - from Ziegler
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      SUBROUTINE STOPUM(AP,ZP,AT,ZT,EKEV,SN,SE,ST,SEHE,STHE)
C
C     ------------------------------------------------------------------
C     ROUTINE TO CALCULATE S(E) IN UNITS OF EV/(10**15 ATOMS/CMSQ)
C     ------------------------------------------------------------------
C
      SN=0.0
      SE=0.0
      ST=0.0
      SEHE=0.0
      STHE=0.0
C
      IZP=ZP+0.5
      IF(IZP.EQ.1) GO TO 100
      IF(IZP.EQ.2) GO TO 200
      IF(IZP.EQ.3) GO TO 200
      IF(IZP.GT.3.AND.IZP.LE.92) GO TO 300
      RETURN
C
C     ------------------------------------------------------------------
C     IT IS A PROTON - MAN CAN WE HANDLE PROTONS
C     ------------------------------------------------------------------
C
  100 EPN=EKEV/AP
      IF(EPN.LT.1.0.OR.EPN.GT.1000000.0) RETURN
      SE=PSTOP(ZT,EPN)
      SN=SNUC(AP,ZP,AT,ZT,EKEV)
      ST=SE+SN
      RETURN
C
C     ------------------------------------------------------------------
C     IT IS HE OR LI
C     ------------------------------------------------------------------
C
  200 EPN=EKEV/AP
      IF(EPN.LT.1.0.OR.EPN.GT.1000000.0) RETURN
      EFCR=ZEFCR(AP,ZP,ZT,EKEV)
      ZEF=EFCR*ZP
      EPRO=1.0079*EPN
      SE=ZEF*ZEF*PSTOP(ZT,EPRO)
      SN=SNUC(AP,ZP,AT,ZT,EKEV)
      ST=SE+SN
      RETURN
C
C     ------------------------------------------------------------------
C     IT IS THE MIGHTY "HEAVY ION" THAT WE ALL LOVE SO WELL
C     ------------------------------------------------------------------
C
  300 EPN=EKEV/AP
      IF(EPN.LT.1.0.OR.EPN.GT.1.0E6) RETURN
C
C     ------------------------------------------------------------------
C     BELOW 1.5 MEV/AMU USE "EFFECTIVE CHARGE" FUNCTION
C     ------------------------------------------------------------------
C
      EFCR=HICRAT(AP,ZP,EKEV)
      ZEF=EFCR*ZP
      EPRO=1.0079*EPN
      SE=ZEF*ZEF*PSTOP(ZT,EPRO)
      SN=SNUC(AP,ZP,AT,ZT,EKEV)
      ST=SE+SN
C
C     ------------------------------------------------------------------
C     ABOVE 1.5 MEV/AMU, USE HI-ENERGY FORMULA
C     ------------------------------------------------------------------
C
      SEHE=SHE(AP,ZP,AT,ZT,EKEV)
      STHE=SN+SEHE
      RETURN
      END
