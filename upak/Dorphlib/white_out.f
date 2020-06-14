C$PROG WHITE_OUT - Moves cin to cout & deletes leading & trailing blanks
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
       subroutine white_out(cin,cout,length)
       implicit integer*4 (a-z)
       byte btab
       character*(*) cin, cout
       character*1 blank,tab
       equivalence (tab,btab)
       data blank/' '/,btab/9/
C
      SAVE
C
C     ------------------------------------------------------------------
C     Moves cin to cout deleting leading and trailing white space
C     (tab or space), and returns length of cout.
C     NO**Also puts a null at end of cout.**NO
C     ------------------------------------------------------------------
C
       i=1
       lench=len(cin)
       leno=len(cout)
       do while(cin(i:i).eq.blank.or.cin(i:i).eq.tab)
          i=i+1
          if(i.gt.lench)then
             length=0
             return
          endif
       enddo
       j=lench
       do while(cin(j:j).eq.blank.or.cin(j:j).eq.tab)
          j=j-1
          if(j.lt.0)then
             length=0
             return
          endif
       enddo
       length=j-i+1
       length=min(leno,length)
       cout(1:length)=cin(i:j)
       return
       end
