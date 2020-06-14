C
      subroutine gsetpar
C
      implicit  none
C
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
C
      integer*4 n,edat,i,j

      SAVE
C
C     ------------------------------------------------------------------
C     SETPAR - SETS UP C,N,A,F ARRAYS FOR READING ALL SCALERS
C     IN ONE FELL-SWOOP VIA CALL TO CAMLIST
C     ------------------------------------------------------------------
C
      EDAT='1FA0'X                      !ECL ENABLE DATA
C
      N=0                               !INIT DCW COUNTER
C
C     ------------------------------------------------------------------
C     SET UP LISTS FOR NON-ECL SCALERS (READ 1 AT A TIME)
C     ------------------------------------------------------------------
C
      DO 40 I=1,NSCA                    !LOOP ON # SCALERS TO READ
C
      IF(gty(I).EQ.'ECL ') GO TO 40      !SKIP IF ECL-TYPE
      if (gty(i) .eq. 'VME ') go to 40   !skip if VME-type
C
      N=N+1                             !INC ARRAY INDEX
C
      GC(N)=CC(I)                       !CRATE#
      GN(N)=NN(I)                       !SLOT#
      GA(N)=AA(I)                       !SUB-ADDRESS
      GF(N)=0                           !F(0) READ
**      write(*,900) n,gc(n),gn(n),ga(n),gf(n),gbuf(n)
**900   format (5i4,i8)
C
   40 CONTINUE
C
C     ------------------------------------------------------------------
C     SET UP LISTS FOR ECL SCALERS - READ 32 AT A TIME
C     ------------------------------------------------------------------
C
      DO 50 I=1,ngecl                   !LOOP ON # ECL SCALER MODS
C
      N=N+1                             !INC DCW-CNTR
C
      GC(N)=gecn(I)                      !CRATE#
      GN(N)=gesn(I)                      !SLOT#
      GA(N)=0                           !A(0)
      GF(N)=16                          !F(16)
      GBUF(N)=EDAT                      !DATA TO ENABLE ECL READ
**      write(*,900) n,gc(n),gn(n),ga(n),gf(n),gbuf(n)
C
      DO 45 J=1,32                      !LOOP TO READ ALL 32
      N=N+1                             !INC DCW-CNTR
      GC(N)=gecn(I)                      !CRATE#
      GN(N)=gesn(I)                      !SLOT#
      GA(N)=0                           !A(0)
      GF(N)=2                           !F(2)
**      write(*,900) n,gc(n),gn(n),ga(n),gf(n),gbuf(n)
   45 CONTINUE      
C
   50 CONTINUE
      ngrap=N
**      write(*,901) ngrap
**901   format('ngrap = ',i5)
      RETURN
      END
