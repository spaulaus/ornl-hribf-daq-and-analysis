C$PROG TERMINATE_STRING - Finds last non-blank in string & appends null
C
C     ******************************************************************
C     BY J.R. Beene AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
       integer function terminate_string(s1)
C
       implicit integer*4 (a-z)
C
       character*(*) s1
C
       character*1 null,sp
C
       SAVE
C
C     ------------------------------------------------------------------
C     Finds the last non-blank or non-null character in an input character
C     variable s1 and inserts a null after it (if there is space).
C     Returns the length of the string up to the las non-blank, if it
C     successfully inserted the null. Returns -length if s1 is full so
c     null can't be inserted.
C     ------------------------------------------------------------------
C
       null=char(0)
       sp=char(32)
       leng=len(s1)
       i=leng
       do while(s1(i:i).eq.sp.or.s1(i:i).eq.null)
          i=i-1
          if(i.lt.1)then
             terminate_string=0
             s1(1:1)=null
             return
          endif
       enddo
       if(i.eq.leng)then
          terminate_string=-i
       else
          s1(i+1:i+1)=null
          terminate_string=i
       endif
       return
       end
