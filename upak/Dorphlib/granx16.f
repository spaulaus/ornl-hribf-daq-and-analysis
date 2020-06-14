C$PROG GRANX16   - Returns random gaussian (X0=0.0, fwhm=2.0)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      FUNCTION GRANX16(ISEED)
C
      SAVE
C
C     ------------------------------------------------------------------
C     This function returns a gaussian distributed set of random numbers
C     Centered at 0.0
C     With FWHM = 2.0
C     ------------------------------------------------------------------
C
      SUM=0.0
C
      DO 10 I=1,16
C
      SUM=SUM+RAN(ISEED)
C
   10 CONTINUE
C
      GRANX16=0.36785*(2.0*SUM-16.0) !Make fwhm=2.0
C
      RETURN
C
      END
