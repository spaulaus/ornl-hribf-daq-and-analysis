      subroutine gsetecl
C
      implicit none
C
C     ------------------------------------------------------------------
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
      integer*4  nonec,n,i,j,nei

      SAVE
C
C     ------------------------------------------------------------------
C     SETECL - DETERMINES WHICH SCALERS (IF ANY) ARE ECL-TYPE AND
C     SETS UP AUXILIARY ARRAYS TO DIRECT THE READING OF SUCH SCALERS
C     ------------------------------------------------------------------
C
      NONEC=0
      DO 10 N=1,NSCA
      IF(gty(N).EQ.'ECL ') GO TO 10
      if(gty(n) .eq. 'VME ') go to 10
      NONEC=NONEC+1
   10 CONTINUE
**       write(*,800) nonec
**800    format('Number of CAMAC = ',i3)
C
      ngecl=0
      DO 50 N=1,NSCA
      gpv(N)=0
C
      IF(gty(N).NE.'ECL ') GO TO 50
C
      NEI=0
      DO 20 I=1,ngecl
      NEI=NEI+1
      IF(gecn(I).EQ.CC(N).AND.gesn(I).EQ.NN(N)) GO TO 40
   20 CONTINUE
C
      ngecl=ngecl+1
      gecn(ngecl)=CC(N)
      gesn(ngecl)=NN(N)
      gpv(N)=32*(ngecl-1)+AA(N)+1+NONEC+ngecl
**       write(*,801) n,gpv(n),ngecl
**801    format('N = ',i3,' gpv(n) = ',i3,' ngecl = ',i3)
      GO TO 50
C
   40 gpv(N)=32*(NEI-1)+AA(N)+1+NONEC+ngecl
**       write(*,801) n,gpv(n),ngecl
C
   50 CONTINUE
C
      ngvme=0
      do 100 n=1,nsca
C
      if(gty(n).ne.'VME ') GO TO 100
C
      nei=0
      DO 110 I=1,ngvme
      nei=nei+1
      if(gmodty(n) .eq. gvmod(i) .and. gvsn(i) .eq. nn(n)) go to 130
  110 continue
C
      ngvme=ngvme+1
      gvmod(ngvme) = gmodty(n)
      gvsn(ngvme) = nn(n)
      gpv(n)=32*(ngvme-1)+aa(n)+nonec+33*ngecl+ngvme
      gvidx(ngvme) = gpv(n) - aa(n)
**       write(*,802) n,gpv(n),ngvme
**802    format('N = ',i3,' gpv(n) = ',i3,' ngvme = ',i3)
      GO TO 100
C
  130 gpv(n)=32*(nei-1)+aa(n)+nonec+33*ngecl+ngvme
**       write(*,802) n,gpv(n),ngvme
C
  100 continue

      j = 1
      do i=1,nsca
        if (gpv(i) .eq. 0) then
          gpv(i) = j
          j = j + 1
        endif
**        write(*,802) i,gpv(i),ngvme
      enddo
*
*
**      do i=1,ngvme
**        write(*,901) gvmod(i),gvsn(i),gvidx(i)
**  901   format(a8,2i5)
**      enddo

      RETURN
      END
