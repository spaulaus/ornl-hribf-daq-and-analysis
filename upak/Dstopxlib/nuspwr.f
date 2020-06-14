C$PROG NUSPWR    - Nuclear stopping power
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      REAL*4 FUNCTION NUSPWR(EA)
C
      IMPLICIT NONE
C
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
      REAL*4          AZ,REDE,SN
C     ------------------------------------------------------------------
C
C     ROUTINE TO CALCULATE THE NUCLEAR (COULOMB) STOPPING
C     POWER SN(E).  MAKES <5% CORRECTION AT 1-2MEV/A.
C     REDE=REDUCED ENERGY
C     SN=REDUCED NUCLEAR STOPPING POWER
C     NUSPWR=NUCLEAR STOPPING POWER IN MEV/(MG/CM**2)
C     ------------------------------------------------------------------
C
      AZ=(AT+AP)*SQRT(ZT**.666666+ZP**.666666)
C
      REDE=32.53*AT*AP*EA/(ZT*ZP*AZ)
C
      SN=0.5*ALOG(1.+REDE)/(REDE+0.10718*REDE**.37544)
C
      NUSPWR=SN*(8.462*ZT*ZP*AP)*CFACT/AZ
C
      RETURN
C
      END
