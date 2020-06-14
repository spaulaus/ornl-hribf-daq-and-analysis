C$PROG STOPNIT   - Initializes CTRL/Z signal handler
C
C     ------------------------------------------------------------------
C     There those folks who have a habit of using  Control Z
C     to stop a scan task.  This practice results in many 
C     stopped jobs and lots of resources left allocated.
C
C     Suggestion has not stopped the use of Control Z so
C     I am going to!
C
C     Routines to initialize a signal handler for Control Z
C     signal.  The signal handler ignores the signal.
C     ------------------------------------------------------------------
C
      subroutine stopnit
C
      implicit none
C
C     intrinsic signal
      integer*4  ir
C
      external  ccstop
C
CX    ir=signal(18,ccstop,-1)
C
      call signal(18,ccstop)
C
      return
C
      end
