*
      subroutine vmeclr

      implicit none
      integer*4 nsc
      parameter (NSC=1024)

C     ------------------------------------------------------------------
      COMMON/Scat2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
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
      integer*4  i,j,ierr
      character*40  msg

      do i=1,nvme
        if (vmemod(i) .eq. 'SIS3820') then
          call sis_sca_ctl(vmesn(i),1,vbuf(vmeidx(i)),ierr)
          if (ierr .ne.  0) then
            call sis_error(ierr,msg)
            write(6,*) msg
            return
          endif
        endif
      enddo
      return
*
      end     

