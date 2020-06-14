C$PROG CTBSNIT   - Enables CTRL-backslash interrupts at CBSAST
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/27/2005 - for gnu
C     ******************************************************************
C
      SUBROUTINE CTBSNIT
C
      IMPLICIT INTEGER*4 (A-Z)
C
      EXTERNAL CBSAST                        !CTRL/Z HANDLER ROUTINE
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO INIT AND RE-ENABLE CRTL-BACKSLASH  TRAPS
C     ------------------------------------------------------------------
C
C
      IR=SIGNAL(3,CBSAST)
C
      RETURN
      END
