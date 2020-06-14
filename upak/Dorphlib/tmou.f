C$PROG TMOU      - Dummy routine to mount/dismount tapes (noop in unix)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE TMOU(KMD,NAMT,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
C     ROUTINE TO MOUNT & DISMOUNT MAG-TAPE UNITS
C     Basically a NOOP in UNIX.
C     ------------------------------------------------------------------
C
      IERR=0                            !RESET ERROR FLAG
C
      RETURN
      END
