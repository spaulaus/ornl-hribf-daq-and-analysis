************************************************************
*
*
      subroutine IPBERR(ipb,stat)

      implicit none

      include 'cam_fast.for'
    
      integer*4 ipb(5),stat,c,n,a,f
      integer*4 i
      character*64 mess
      character*20 msg(10)/
     1 'GOOD X   BAD Q','GOOD Q   BAD X','BAD  Q   - BAD X',
     2 'Timeout',
     3 'Illegal Crate','Illegal Module','Illegal Address',
     4 'Illegal Function','Crate Off_Line','Unknown error code'/

*   
      if(stat.eq.0) return     !return if good
*
*  X = 1 and Q = 0 is ignored in this error handler
*
      if(stat.eq.NOQ) return   !return if bad Q only
*
      call ilbyte(c,ipb,0)     !otherwise, pick up c,n,a,f
      call ilbyte(n,ipb,1)
      call ilbyte(a,ipb,2)
      call ilbyte(f,ipb,3)
*   
      if (stat .eq. NOQ) then
         i = 1
      elseif (stat .eq. NOX) then
         i = 2
      elseif (stat .eq. NOX_NOQ) then
         i = 3
      elseif (stat .eq. TIMEOUT) then
         i = 4
      elseif (stat .eq. ILL_CRATE) then
         i = 5
      elseif (stat .eq. ILL_MODULE) then
         i = 6
      elseif (stat .eq. ILL_ADDRESS) then
         i = 7
      elseif (stat .eq. ILL_FUNCTION) then
         i = 8
      elseif (stat .eq. CRATE_OFF_LINE) then
         i = 9
      else
         i = 10
      endif

      write(mess,10) c,n,a,f,msg(i)
10    format(' CAM ERROR-C,N,A,F=',4I3,'  ',A)

20    write(6,9000) mess       !send error message
9000  format(a)
      return
      end
