C$PROG SETECL    - Determines which scalers (in any) are ECL-type
*                  or VME-type
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE SETECL
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
C
      SAVE
C
C     ------------------------------------------------------------------
C     SETECL - DETERMINES WHICH SCALERS (IF ANY) ARE ECL-TYPE AND
C     SETS UP AUXILIARY ARRAYS TO DIRECT THE READING OF SUCH SCALERS
C     ------------------------------------------------------------------
C
      NONEC=0
      DO 10 N=1,NR
      IF(TY(N).EQ.'ECL ') GO TO 10
      if(ty(n) .eq. 'VME ') go to 10
      NONEC=NONEC+1
   10 CONTINUE
**       write(*,800) nonec
**800    format('Number of CAMAC = ',i3)
C
      NEC=0
      DO 50 N=1,NR
      PV(N)=0
C
      IF(TY(N).NE.'ECL ') GO TO 50
C
      NEI=0
      DO 20 I=1,NEC
      NEI=NEI+1
      IF(ECN(I).EQ.CN(N).AND.ESN(I).EQ.SN(N)) GO TO 40
   20 CONTINUE
C
      NEC=NEC+1
      ECN(NEC)=CN(N)
      ESN(NEC)=SN(N)
      PV(N)=32*(NEC-1)+A(N)+1+NONEC+NEC
**       write(*,801) n,pv(n),nec
**801    format('N = ',i3,' pv(n) = ',i3,' NEC = ',i3)
      GO TO 50
C
   40 PV(N)=32*(NEI-1)+A(N)+1+NONEC+NEC
**       write(*,801) n,pv(n),nec
C
   50 CONTINUE
C
      nvme=0
      do 100 n=1,nr
C
      if(ty(n).ne.'VME ') GO TO 100
C
      nei=0
      DO 110 I=1,nvme
      nei=nei+1
      if(modty(n) .eq. vmemod(i) .and. vmesn(i) .eq. sn(n)) go to 130
  110 continue
C
      nvme=nvme+1
      vmemod(nvme) = modty(n)
      vmesn(nvme) = sn(n)
      pv(n)=32*(nvme-1)+a(n)+nonec+33*nec+nvme
      vmeidx(nvme) = pv(n) - a(n) 
***      write(*,802) n,pv(n),nvme
***802    format('N = ',i3,' pv(n) = ',i3,' NVME = ',i3)
      GO TO 100
C
  130 pv(n)=32*(nei-1)+a(n)+nonec+33*nec+nvme
***       write(*,802) n,pv(n),nvme
C
  100 continue

      do i=1,nr
**        write(*,900) i,cn(i),sn(i),a(i),pv(i),ty(i)
**  900   format(5i5,a4)
      enddo
      do i=1,nvme
**        write(*,901) vmemod(i),vmesn(i),vmeidx(i)
**  901   format(a8,2i5)
      enddo

      RETURN
      END
