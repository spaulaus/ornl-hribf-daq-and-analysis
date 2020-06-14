*******************************************************************************
*******************************************************************************
      subroutine hissub(ibuf,nhw)

      implicit none
      integer*4 MAXID,SPECSIZ
      parameter (MAXID = 1024)
      parameter (SPECSIZ = 4096)
      integer*4 idused(MAXID),pbuf(MAXID),ievent
      integer*4 id,ierr
      integer*2 ibuf(*)
      integer*4 nhw,i


999   call unpackbb(ibuf,nhw,idused,pbuf,MAXID,ievent,ierr,&1000)
      if (ierr .ne. 0) then
        type *,'Too many parameters'
        call exit
      endif
*
****       call count1cc(1,ievent,0)
****       go to 999
      do i = 1,ievent
        id = idused(i)
        pbuf(id) = (pbuf(id) .and. SPECSIZ-1)
        call count1cc(id,pbuf(id),0)
      enddo
*
900   continue
*request now next event from unpack subroutine
      go to 999
*
1000  if (ierr .ne. 0) then
        type *,'Buffer format error'
        call exit
      endif
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine drrsub(iexist)
*
      implicit none
      integer*4 MAXID,SPECSIZ
      parameter (MAXID = 128)
      parameter (SPECSIZ = 4096)
      integer iexist,i
*
      call drrmake
*
      do i = 1,MAXID
        call hd1d(i,2,SPECSIZ,SPECSIZ,0,SPECSIZ-1,' testing ')
      enddo
*
      call endrr
*
      return
      end
