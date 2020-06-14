      integer*4 i,j
      integer*4 rv(16),lin(16),recp(16),exp(16)
      real*4  a,b,tmp1,tmp2

      a = -225.0/1060.0
      b = 255.0 - 140.0*a
      j = 1
      do i=140,1240,100
        lin(j) = i
        if (i .gt. 1200) lin(j) = 1200
        tmp1 = lin(j) * a + b
        rv(j) = nint(tmp1)
**         write(*,*) rv(j),lin(j)
        j = j + 1
      enddo
      j = 1
      do i=140,1240,100
        tmp1 = 35660.3774/(rv(j)-0.2830)
        recp(j) = nint(tmp1)
**         write(*,*) rv(j),recp(j)
        j = j + 1
      enddo
      j = 1
      do i=140,1240,100
        tmp1 = rv(j)
        tmp2 = log(tmp1)
        tmp1 = 2884.695 - tmp2*495.3192
        exp(j) = nint(tmp1)
**         write(*,*) rv(j),exp(j)
        j = j + 1
      enddo
      j = 1
      do i=140,1240,100
         write(*,*) lin(j),rv(j),recp(j),exp(j)
        j = j + 1
      enddo
      end
