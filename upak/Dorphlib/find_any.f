C$PROG FIND_ANY  - Finds 1st occur of any char in string s2 in s1
C
C     ******************************************************************
C     BY J.R. Beene AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
C
       integer function find_any(s1,s2)
C
C     ------------------------------------------------------------------
C     Finds first occurance of any single character in input string
C     s2 in the input string s1.
C     ------------------------------------------------------------------
C
       implicit integer*4 (a-z)
       character *(*) s1,s2
C
       SAVE
C
C     ------------------------------------------------------------------

       l1=len(s1)
       l2=len(s2)
       do i=1,l1
          do j=1,l2
             if(s1(i:i).eq.s2(j:j))then
                  find_any=i
                  return
             endif
          enddo
       enddo
       find_any=0
       return
       end
