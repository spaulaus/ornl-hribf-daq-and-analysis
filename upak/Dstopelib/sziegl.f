C$PROG SZIEGL    - S(ELE) for all projectiles - from Ziegler
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      FUNCTION SZIEGL(AP,ZP,AT,ZT,EKEV)
C
C     ------------------------------------------------------------------
C     ROUTINE TO CALCULATE S(E) IN UNITS OF MEV*CMSQ/MG
C     ------------------------------------------------------------------
C
      SE=0.0
C
      IZP=ZP+0.5
      IF(IZP.EQ.1) GO TO 100
      IF(IZP.EQ.2) GO TO 200
      IF(IZP.EQ.3) GO TO 200
      IF(IZP.GT.3.AND.IZP.LE.92) GO TO 300
      GO TO 500
C
C     ------------------------------------------------------------------
C     IT IS A PROTON - MAN CAN WE HANDLE PROTONS
C     ------------------------------------------------------------------
C
  100 EPN=EKEV/AP
      IF(EPN.LT.1.0.OR.EPN.GT.100000.0) GO TO 500
      SE=PSTOP(ZT,EPN)
      GO TO 400
C
C     ------------------------------------------------------------------
C     IT IS HE OR LI
C     ------------------------------------------------------------------
C
  200 EPN=EKEV/AP
      IF(EPN.LT.1.0.OR.EPN.GT.100000.0) GO TO 500
      EFCR=ZEFCR(AP,ZP,ZT,EKEV)
      ZEF=EFCR*ZP
      SE=ZEF*ZEF*PSTOP(ZT,EPN)
      GO TO 400
C
C     ------------------------------------------------------------------
C     IT IS THE MIGHTY "HEAVY ION" THAT WE ALL LOVE SO WELL
C     ------------------------------------------------------------------
C
  300 EPN=EKEV/AP
      IF(EPN.LT.1.0.OR.EPN.GT.1.0E6) GO TO 500
C
      EFCR=HICRAT(AP,ZP,EKEV)
      ZEF=EFCR*ZP
      SE=ZEF*ZEF*PSTOP(ZT,EPN)
C
  400 SZIEGL=0.6023*SE/AT
      RETURN
C
  500 SZIEGL=0.0
      RETURN
      END
