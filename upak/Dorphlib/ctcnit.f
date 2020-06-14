C$PROG CTCNIT    - Enables CTRL/C interrupts at CCAST
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE CTCNIT
C
      IMPLICIT INTEGER*4 (A-Z)
C
      EXTERNAL CCAST                        !CTRL/C HANDLER ROUTINE
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO INIT AND RE-ENABLE CRTL/C TRAPS
C     ------------------------------------------------------------------
C
C
Cg95     IR=SIGNAL(2,CCAST,-1)
Cgfortran      IR=SIGNAL(2,CCAST)
         IR=SIGNAL(2,CCAST)
C
      RETURN
      END
