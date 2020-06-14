C$PROG RANGI     - Computes "reduced range" 
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      REAL*4 FUNCTION RANGI(EA,DE)
C
C     ------------------------------------------------------------------
      COMMON/RUNGE/   DER,EMIN,E0,DUMY,R0
      REAL*4          DER,EMIN,E0,DUMY
      REAL*8                           R0
C     ------------------------------------------------------------------
      REAL*8          K1,K2,K4,R
C     ------------------------------------------------------------------
      REAL*4          EA,DE
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ROUTINE TO CALCULATE THE REDUCED RANGE R BY SOLVING THE
C     DIFF. EQ. DR/DEA=1/S(E) USING THE 4TH ORDER RUNGE-KUTTA
C     METHOD. THE QUANTITIES ARE DEFINED AS:
C     R=RANGE/AP*1000 (1000 SINCE W IS IN KEV BUT S(E) IS IN MEV)
C     AP=PROJECTILE MASS
C     EA=E/AP ENERGY PER NUCLEON IN KEV
C     S(E)=STOPPING POWER FOR PROJECTILE IN MEV/MG/CM**2
C
C     CALL FIRST TIME WITH R0=0. TO OBTAIN RANGE AT E0.  CALL
C     SUBSEQUENTLY WITH R0=R FROM PREVIOUS CALL.
C     ------------------------------------------------------------------
C
      R=R0
C
      IF(R0 .LE. 0) THEN
C
         IF(EA.GT.EMIN) THEN
            R=(EA/2.)/STOPP(EA)
            RANGI=R
            R0=R
            E0=EA
            RETURN

         ELSE
C
            R=EA/(2.*STOPP(EMIN))
            R0=R
            E0=EA
            RANGI=R
         RETURN
C
         ENDIF
C
C     ------------------------------------------------------------------
C     EXTRAPOLATE BELOW EMIN--ZEFF BECOMES UNRELIABLE
C     FOR LOW ENERGIES
C     ------------------------------------------------------------------
C
      ELSE
         IF(EA.LT.EMIN) THEN
C
            R=EA/(2.*STOPP(EMIN))
            R0=R
            E0=EA
            RANGI=R
            RETURN
         ELSE
C
            N=(EA-E0)/DE
            IF(N.LT.4) N=4
            W=(EA-E0)/N
            K1=1./STOPP(E0)
            DO I=1,N
               K2=1./STOPP(E0+W/2.)
               K4=1./STOPP(E0+W)
               R=R+(K1+4.*K2+K4)*W/6.
               K1=K4
               E0=E0+W
            ENDDO
            R0=R
            RANGI=R
            RETURN
         ENDIF
      ENDIF
      END
