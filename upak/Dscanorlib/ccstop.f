C$PROG CCSTOP    - CTRL/Z signal handler
C
C     ------------------------------------------------------------------
C     Signal handler routine
C     ------------------------------------------------------------------
C
      subroutine ccstop
C
      implicit none
C
C     SAVE
C
      call stopnit
C
      return
C
      end
