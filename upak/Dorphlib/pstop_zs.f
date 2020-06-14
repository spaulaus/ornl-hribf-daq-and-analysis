C$PROG PSTOP_ZS  - Cumputes stopping power of protons in any target
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION PSTOP_ZS(ZT,E)
C
      COMMON/QSQ001/ AA(12,92)
C
      DIMENSION A(12)
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO CALCULATE STOPPING POWER OF PROTONS IN ANY TARGET
C     USING PARAMETERS OF J.F. ZIEGLER (HYDROGEN STOPPING POWERS AND
C     RANGES IN ALL ELEMENTS)
C     S(E) IN UNITS OF EV/(10**15 ATOMS/CMSQ)
C     ZT = TARGET ATOMIC NO. IN AMU
C     E  = PROJECTILE ENERGY IN KEV
C     ------------------------------------------------------------------
C
      J=ZT+0.5
      DO 5 I=1,12
      A(I)=AA(I,J)
    5 CONTINUE
C
C     ------------------------------------------------------------------
C     SELECT APPROPRIATE FORMULA FOR REQUIRED ENERGY
C     ------------------------------------------------------------------
C
      IF(E.GT.10.0) GO TO 10
      PSTOP_ZS=A(1)*SQRT(E)
      RETURN
   10 IF(E.GT.999.0) GO TO 20
      SLO=A(2)*E**0.45
      SHI=(A(3)/E)*ALOG(1.0+A(4)/E+A(5)*E)
      PSTOP_ZS=1.0/(1.0/SLO+1.0/SHI)
      RETURN
   20 BSQ=1.0-1.0/(1.0+E/938837.0)**2
      TA=ALOG(A(7)*BSQ/(1.0-BSQ))
      SUM=0.0
      ELOG=ALOG(E)
      DO 30 I=1,5
      J=I-1
      K=J+8
      SUM=SUM+A(K)*ELOG**J
   30 CONTINUE
      PSTOP_ZS=A(6)*(TA-BSQ-SUM)/BSQ
      RETURN
      END
