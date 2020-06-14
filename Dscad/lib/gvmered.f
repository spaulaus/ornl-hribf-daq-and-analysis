*
      subroutine gvmered

      implicit none

C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      common/dd13a/ gmodty(16),gvmod(16),gvsn(16),gvidx(16),ngvme,
     $              gty(16),gpv(16),gecn(16),gesn(16),ngecl
      character*8   gmodty,    gvmod
      integer*4                          gvsn,    gvidx,    ngvme
      integer*4             gpv,    gecn,    gesn,    ngecl
      character*4   gty
C     ------------------------------------------------------------------
      common/dd13b/ gc(280),gn(280),ga(280),gf(280),gbuf(280),ngrap
      integer*4     gc,     gn,     ga,     gf,     gbuf,     ngrap
C     ------------------------------------------------------------------
*
      integer*4  i,j,k,ierr
      character*40  msg

      do i=1,ngvme
        if (gvmod(i) .eq. 'SIS3820') then
          call sis_sca_ctl(gvsn(i),0,gbuf(gvidx(i)),ierr)
          if (ierr .ne.  0) then
            call sis_error(ierr,msg)
            write(6,*) msg
            return
          endif
          k = gvidx(i) -1
          do j=1,32
             gbuf(k+j) = iand(gbuf(k+j),'7ffffff'x)
          enddo
        endif
      enddo
      return
*
      end     

