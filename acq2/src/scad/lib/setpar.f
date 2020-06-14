C$PROG SETPAR    - Sets up C,N,A,F array for reading scalers via CAMLIST
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE SETPAR
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 TY
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
C
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
      DO 40 I=1,NR                      !LOOP ON # SCALERS TO READ
C
      IF(TY(I).EQ.'ECL ') GO TO 40      !SKIP IF ECL-TYPE
      if (ty(i) .eq. 'VME ') go to 40   !skip if VME-type
C
      N=N+1                             !INC ARRAY INDEX
C
      CC(N)=CN(I)                       !CRATE#
      NN(N)=SN(I)                       !SLOT#
      AA(N)=A(I)                        !SUB-ADDRESS
      FF(N)=0                           !F(0) READ
**      write(*,900) n,cc(n),nn(n),aa(n),ff(n),vbuf(n)
**900   format (5i4,i8)
C
   40 CONTINUE
C
C     ------------------------------------------------------------------
C     SET UP LISTS FOR ECL SCALERS - READ 32 AT A TIME
C     ------------------------------------------------------------------
C
      DO 50 I=1,NEC                     !LOOP ON # ECL SCALER MODS
C
      N=N+1                             !INC DCW-CNTR
C
      CC(N)=ECN(I)                      !CRATE#
      NN(N)=ESN(I)                      !SLOT#
      AA(N)=0                           !A(0)
      FF(N)=16                          !F(16)
      VBUF(N)=EDAT                      !DATA TO ENABLE ECL READ
**      write(*,900) n,cc(n),nn(n),aa(n),ff(n),vbuf(n)
C
      DO 45 J=1,32                      !LOOP TO READ ALL 32
      N=N+1                             !INC DCW-CNTR
      CC(N)=ECN(I)                      !CRATE#
      NN(N)=ESN(I)                      !SLOT#
      AA(N)=0                           !A(0)
      FF(N)=2                           !F(2)
**      write(*,900) n,cc(n),nn(n),aa(n),ff(n),vbuf(n)
   45 CONTINUE      
C
   50 CONTINUE
      NLIST=N
**      write(*,901) nlist
**901   format('nlist = ',i5)
      RETURN
      END
