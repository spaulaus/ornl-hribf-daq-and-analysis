C$PROG SNORTH    - S(ELE) - Jacket routine for Northcliff's STOPIT
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
C
      FUNCTION SNORTH(AP,ZP,AT,ZT,EKEV)
C
C     ------------------------------------------------------------------
      COMMON/ST02/  KSORB
      INTEGER*4     KSORB
C     ------------------------------------------------------------------
      REAL*4       SENS(600)
C
      DATA         DE,NDO/1.0,1/
C     ------------------------------------------------------------------
C
      EMIN=0.001*EKEV
C
      CALL STOPIT(EMIN,DE,NDO,ZP,AP,ZT,SENS,KSORB)
C
      IZ=ZT+0.5
      AN=ANAT(IZ)
C
      SNORTH=SENS(1)*AN/AT
      RETURN
      END
