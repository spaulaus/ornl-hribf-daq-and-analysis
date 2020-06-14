C$PROG FIND_NANY - Finds 1st non-occur of any char in string s2 in s1
C
C     ******************************************************************
C     BY J.R. Beene AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
       integer function find_nany(s1,s2)
C
C     ------------------------------------------------------------------
C     Finds first non-occurance of any single character in input string
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
       non=0
       do i=1,l1
          do j=1,l2
             if(s1(i:i).eq.s2(j:j)) non=1
	  enddo
	if(non.eq.0) then
		find_nany=i
		return
	else
		non=0
	endif
       enddo
       find_nany=0
       return
       end
