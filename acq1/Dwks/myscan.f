*******************************************************************************
*******************************************************************************
      subroutine hissub(ibuf,nhw)

      implicit none
      integer*4 NPARS,SPECSIZ
      parameter (NPARS = 200)
      parameter (SPECSIZ =65536)

      integer*4 evbuf(2,NPARS)
      integer*2 ibuf(*)
      integer*4 nhw,i,id,nparam,ierr
      integer*4 t10,t11,t12,t20,t21,t22,hisit,decay
      integer*8 t1t,t2t

999   call unpackaa(ibuf,nhw,evbuf,nparam,NPARS,ierr,&1000)
      if (ierr .ne. 0) then
        type *,'Too many parameters'
        call exit
      endif
*
      t2t = 0
      hisit = 0
      decay = 0
      do i = 1,nparam
        id = evbuf(1,i)
        if (id .eq. 1) then
          evbuf(2,i) = (evbuf(2,i) .and. SPECSIZ-1)
          t10 = evbuf(2,i)
          call count1c(id,evbuf(2,i),0)
        else if (id .eq. 2) then
          evbuf(2,i) = (evbuf(2,i) .and. SPECSIZ-1)
          t11 = evbuf(2,i)
          call count1c(id,evbuf(2,i),0)
        else if (id .eq. 3) then
          evbuf(2,i) = (evbuf(2,i) .and. SPECSIZ-1)
          t12 = evbuf(2,i)
          call count1c(id,evbuf(2,i),0)
        else if (id .eq. 5) then
          evbuf(2,i) = (evbuf(2,i) .and. SPECSIZ-1)
          t20 = evbuf(2,i)
          call count1c(id,evbuf(2,i),0)
        else if (id .eq. 6) then
          evbuf(2,i) = (evbuf(2,i) .and. SPECSIZ-1)
          t21 = evbuf(2,i)
          call count1c(id,evbuf(2,i),0)
        else if (id .eq. 7) then
          evbuf(2,i) = (evbuf(2,i) .and. SPECSIZ-1)
          t22 = evbuf(2,i)
          call count1c(id,evbuf(2,i),0)
****        else if (id .eq. 191 .and. evbuf(2,i) .eq. 2) then
****        else if (id .eq. 1 .and. evbuf(2,i) .eq. 3) then
****          hisit = 1
        else if (id .eq. 191 .and. evbuf(2,i) .eq. 2) then
          hisit = 1
        else if (id .ge. 88 .and. id .le. 167) then
          decay = 1
        else if (id .eq. 1 .and. evbuf(2,i) .eq. 2) then
          hisit = 1
        endif
      enddo
      if (t10 .ne. 0 .and. t11 .ne. 0 .and. t12 .ne. 0) then
        t1t = t12 * 65536 + t11
        t1t = t1t * 65536 + t10
      endif
      t2t = t22 * 65536 + t21
      t2t = t2t * 65536 + t20
      if (t1t .ne. 0 .and. t2t .ne. 0) call count1cc(17,(t2t-t1t))
      if (t1t .ne. 0 .and. t2t .ne. 0) call count1cc(16,(t2t-t1t)/10)
      if (t1t .ne. 0 .and. t2t .ne. 0) call count1cc(15,(t2t-t1t)/100)
      if (hisit .ne. 0) call count1cc(14,t2t-t1t,0)
      if (hisit .ne. 0 .and. decay .ne. 0) call count1cc(13,t2t-t1t,0)
*
900   continue
*  request now next event from unpack subroutine
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
      integer*4 NPARS,SPECSIZ
      parameter (NPARS = 17)
      parameter (SPECSIZ = 16384)
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
