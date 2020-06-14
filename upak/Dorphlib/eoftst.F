C$PROG EOFTST    - Tests for platform dependent EOF code
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      INTEGER*4 FUNCTION EOFTST(IOS)
C
      INTEGER*4 IOS
C
      SAVE
C
C     ------------------------------------------------------------------
C
      EOFTST=0
C
#ifdef GFORTRAN
      IF ((ios .eq. 5002) .or. (IOS.EQ.3)) EOFTST=1
#else
      IF(IOS.EQ.-1) EOFTST=1
#endif
C
      RETURN
C
      END
