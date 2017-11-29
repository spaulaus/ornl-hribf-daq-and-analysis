*
      subroutine vmered

      implicit none
      integer*4 nsc
      parameter (nsc=1024)
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      common/scat2a/ modty(NSC),vmemod(20),vmesn(20),vmeidx(20),nvme
      character*8   modty,     vmemod
      integer*4                           vmesn,    vmeidx,    nvme
C     ------------------------------------------------------------------
      COMMON/SCAT3/ CC(NSC),NN(NSC),AA(NSC),FF(NSC),VBUF(NSC),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
*
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
*    Do not supress the sign bit, it is part of the data.
         write(6,*) "Raw Scalers:"
         do j=1,33
            write(6,*) i, k, j, k+j, vbuf(k+j)
*           vbuf(k+j) = iand(vbuf(k+j),'7fffffff'x)
*           vbuf(k+j) = vbuf(k+j)
         enddo

        endif
      enddo
      return
*
      end     

