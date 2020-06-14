*******************************************************************************
*******************************************************************************
      subroutine hissub(ibuf,nhw)

      implicit none
      integer*4 NPARS,SPECSIZ
      parameter (NPARS = 375)
      parameter (SPECSIZ =4096)

      integer*4 evbuf(2,NPARS)
      integer*2 ibuf(*)
      integer*4 nhw,i,j,id,nparam,ierr,iend

999   call unpackaa(ibuf,nhw,evbuf,nparam,NPARS,ierr,iend)
      if (ierr .ne. 0) then
        write(*,*) 'Too many parameters'
        call exit
      endif
*
***       call count1cc(1,nparam,0)
***       if (nparam .gt. 28) nparam = 28
       do j = 1,nparam
          id = evbuf(1,j)
        evbuf(2,j) = iand(evbuf(2,j),4095)
          call count1cc(id,evbuf(2,j),0)
       enddo
*
       if (nparam .ge. 450) then
         do j = 1,nparam,15
           id = evbuf(1,j)
           write(8,8000) id,(evbuf(2,i),i=j,j+14)
8000       format (i4,15z5)
         enddo
         write(8,8010)
8010     format ('**************')
       endif
900   continue
*  request now next event from unpack subroutine
      if (iend .eq. 0) go to 999
*
1000  if (ierr .ne. 0) then
          write(*,*) 'Buffer format error'
          call exit
        endif
        return
	end
*******************************************************************************
*******************************************************************************
      subroutine drrsub(iexist)
*
      implicit none
      integer*4 NPARS,SPECSIZ
      parameter (NPARS = 375)
      parameter (SPECSIZ = 4096)
      integer iexist,i
*
      call drrmake
*
      do i = 1,NPARS
        call hd1d(i,2,SPECSIZ,SPECSIZ,0,SPECSIZ-1,' testing ')
      enddo
*
      call endrr
*
      return
      end
