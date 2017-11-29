C$PROG SETPAR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE SETPAR
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      real*8 vn, vo, vd
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
      CHARACTER*4   TY,KI,LA
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
C     ------------------------------------------------------------------
      common/scat2a/ modty(NSC),vmemod(20),vmesn(20),vmeidx(20),nvme
      character*8   modty,     vmemod
      integer*4                           vmesn,    vmeidx,    nvme
C     ------------------------------------------------------------------
      COMMON/SCAT3/ CC(NSC),NN(NSC),AA(NSC),FF(NSC),VBUF(NSC),NLIST
C     ------------------------------------------------------------------
C
      SAVE
C
C     ******************************************************************
C     SETPAR - SETS UP C,N,A,F ARRAYS FOR READING ALL SCALERS
C     IN ONE FELL-SWOOP VIA CALL TO CAMLIST
C     ******************************************************************
C
      EDAT='1FA0'X                      !ECL ENABLE DATA
C
      N=0                               !INIT DCW COUNTER
C
C     ******************************************************************
C     SET UP LISTS FOR NON-ECL SCALERS (READ 1 AT A TIME)
C     ******************************************************************
C
      DO 40 I=1,NR                      !LOOP ON # SCALERS TO READ
C
      IF (TY(I) .EQ. 'ECL ') GO TO 40   !SKIP IF ECL-TYPE
      if (ty(i) .eq. 'VME ') go to 40   !skip if VME-type
C
      N=N+1                             !INC ARRAY INDEX
C
      CC(N)=CN(I)                       !CRATE#
      NN(N)=SN(I)                       !SLOT#
      AA(N)=A(I)                        !SUB-ADDRESS
      FF(N)=0                           !F(0) READ
C
   40 CONTINUE
C
C     ******************************************************************
C     SET UP LISTS FOR ECL SCALERS - READ 32 AT A TIME
C     ******************************************************************
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
C
      DO 45 J=1,32                      !LOOP TO READ ALL 32
      N=N+1                             !INC DCW-CNTR
      CC(N)=ECN(I)                      !CRATE#
      NN(N)=ESN(I)                      !SLOT#
      AA(N)=0                           !A(0)
      FF(N)=2                           !F(2)
   45 CONTINUE      
C
   50 CONTINUE
      NLIST=N
      RETURN
      END
