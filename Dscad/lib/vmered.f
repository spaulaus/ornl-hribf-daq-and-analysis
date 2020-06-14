*
      subroutine vmered

      implicit none

C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      common/sd02a/ modty(512),vmemod(20),vmesn(20),vmeidx(20),nvme
      character*8   modty,     vmemod
      integer*4                           vmesn,    vmeidx,    nvme
C     ------------------------------------------------------------------
      COMMON/SD09/ CC(512),NN(512),AA(512),FF(512),VBUF(512),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
*
      integer*4  i,j,k,ierr
      character*40  msg

      do i=1,nvme
        if (vmemod(i) .eq. 'SIS3820') then
          call sis_sca_ctl(vmesn(i),0,vbuf(vmeidx(i)),ierr)
          if (ierr .ne.  0) then
            call sis_error(ierr,msg)
            write(6,*) msg
            return
          endif
          k = vmeidx(i) - 1
          do j=1,32
             vbuf(k+j) = iand(vbuf(k+j),'7ffffff'x)
          enddo
        endif
      enddo
      return
*
      end     

