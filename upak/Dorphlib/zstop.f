C$PROG ZSTOP     - Computes stopping power - from Ziegler
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE ZSTOP(AP,ZP,AT,ZT,EMEV,SN,SE,SEHE,IERR)
C
      DATA NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO CALCULATE S(E) IN UNITS OF MEV*CMSQ/MG
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 10
      CALL ZSTOP_DAT
      NCALL=1
C
   10 IERR=0
      SN=0.0
      SE=0.0
      SEHE=0.0
      EKEV=1000.0*EMEV
      FAC=0.602/AT
C
      IZP=ZP+0.5
      IF(IZP.EQ.1) GO TO 100
      IF(IZP.EQ.2) GO TO 200
      IF(IZP.EQ.3) GO TO 200
      IF(IZP.GT.3.AND.IZP.LE.92) GO TO 300
   50 IERR=1
      RETURN
C
C     ------------------------------------------------------------------
C     IT IS A PROTON - MAN CAN WE EVER HANDLE PROTONS!
C     ------------------------------------------------------------------
C
  100 EPN=EKEV/AP
      IF(EPN.LT.1.0.OR.EPN.GT.1000000.0) RETURN
      SE=FAC*PSTOP_ZS(ZT,EPN)
      SN=FAC*SNUC_ZS(AP,ZP,AT,ZT,EKEV)
      RETURN
C
C     ------------------------------------------------------------------
C     IT IS HE OR LI
C     ------------------------------------------------------------------
C
  200 EPN=EKEV/AP
      IF(EPN.LT.1.0.OR.EPN.GT.1000000.0) GO TO 50
      EFCR=ZEFCR_ZS(AP,ZP,ZT,EKEV)
      ZEF=EFCR*ZP
      EPRO=1.0079*EPN
      SE=FAC*ZEF*ZEF*PSTOP_ZS(ZT,EPRO)
      SN=FAC*SNUC_ZS(AP,ZP,AT,ZT,EKEV)
      RETURN
C
C     ------------------------------------------------------------------
C     IT IS THE MIGHTY "HEAVY ION" THAT WE ALL LOVE SO WELL
C     ------------------------------------------------------------------
C
  300 EPN=EKEV/AP
      IF(EPN.LT.1.0.OR.EPN.GT.1.0E6) GO TO 50
C
C     ------------------------------------------------------------------
C     BELOW 1.5 MEV/AMU USE "EFFECTIVE CHARGE" FUNCTION
C     ------------------------------------------------------------------
C
      EFCR=HICR_ZS(AP,ZP,EKEV)
      ZEF=EFCR*ZP
      EPRO=1.0079*EPN
      SE=FAC*ZEF*ZEF*PSTOP_ZS(ZT,EPRO)
      SN=FAC*SNUC_ZS(AP,ZP,AT,ZT,EKEV)
C
      IF(EPN.LE.1500.0) RETURN
C
C     ------------------------------------------------------------------
C     ABOVE 1.5 MEV/AMU, USE HI-ENERGY FORMULA AS WELL
C     ------------------------------------------------------------------
C
      SEHE=FAC*SHE_ZS(AP,ZP,AT,ZT,EKEV)
      RETURN
      END
