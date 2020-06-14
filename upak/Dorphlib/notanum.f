C$PROG NOTANUM   - Checks 1st char in string for "not a number" etc
C
C     ******************************************************************
C     BY J.R. Beene AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      logical function notanum(str)
C
      character*(*) str
C
      integer find_nany
C
      SAVE
C
C     ------------------------------------------------------------------
C     Checks first character in string which is not in the set
C     {space,comma,period}. If this character is numeric (0-9),
C     or if no characters outside the set {space,comma} are
C     found, it returns .FALSE. Otherwise returns .TRUE.
C     ------------------------------------------------------------------
C
      ins=find_nany(str,' ,')
      ind=find_nany(str,' .,')
      notanum=.FALSE.
      if(ind.eq.0)then
        if(ins.ne.0)then
          notanum=.TRUE.
        endif
        return
      endif
      notanum=(str(ind:ind).lt.'0'.OR.str(ind:ind).GT.'9')
      return
      end
