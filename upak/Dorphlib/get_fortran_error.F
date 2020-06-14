C$PROG GET_Fortran_ERROR - G77 or gfortran I/O run-time error message routine
C
C     ******************************************************************
C     Similar to one by
c        W.T. MILNER AT HRIBF 
c      created by RL Varner 12 April 2007
c     This routine is a stub, simply to be sure we call the correct routine
c     for the architecture.
C     ******************************************************************
C
      SUBROUTINE GET_FORTRAN_ERROR(IERR,MESG)
C
      IMPLICIT NONE
C
C---------------------------------------------------------------------
      CHARACTER*80 MSG(100),MESG
C
      INTEGER*4    IERR
C     ------------------------------------------------------------------
C
#if F77== g77
         call get_g77_error(ierr, msg)
#else
         call get_gfortran_error(ierr, msg)
#endif
c
      RETURN
C
      END
