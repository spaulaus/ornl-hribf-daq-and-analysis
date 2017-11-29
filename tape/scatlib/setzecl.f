C$PROG SETZECL
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE SETZECL
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
      COMMON/SCATC/ CNZ(100),SNZ(100),AZ(100),NZOT
      INTEGER*4     CNZ,     SNZ,     AZ,     NZOT
C     ------------------------------------------------------------------
C
      SAVE
C
C     ******************************************************************
C     SET UP ZOT-LIST for NON-ECL SCALERS
C     ******************************************************************
C
      NZOT=0
      DO 50 I=1,NR
      IF(TY(I).EQ.'ECL ') GO TO 50
      DO 20 J=1,NZOT
      IF(CNZ(J).EQ.CN(I).AND.SNZ(J).EQ.SN(I)) GO TO 50
   20 CONTINUE
      NZOT=NZOT+1
      CNZ(NZOT)=CN(I)
      SNZ(NZOT)=SN(I)
      AZ(NZOT) =0
   50 CONTINUE
C
C     ******************************************************************
C     DETERMINE WHICH SCALERS (IF ANY) ARE ECL-TYPE AND
C     SETS UP AUXILIARY ARRAYS TO DIRECT THE READING OF SUCH SCALERS
C     ******************************************************************
C
      NONEC=0
      DO 100 N=1,NR
      IF(TY(N).EQ.'ECL ') GO TO 100
      if(ty(n) .eq. 'VME ') go to 100
      NONEC=NONEC+1
  100 CONTINUE
C
      NEC=0
      DO 200 N=1,NR
      PV(N)=0
C
      IF(TY(N).NE.'ECL ') GO TO 200
C
      NEI=0
      DO 120 I=1,NEC
      NEI=NEI+1
      IF(ECN(I).EQ.CN(N).AND.ESN(I).EQ.SN(N)) GO TO 140
  120 CONTINUE
C
      NEC=NEC+1
      ECN(NEC)=CN(N)
      ESN(NEC)=SN(N)
      PV(N)=32*(NEC-1)+A(N)+1+NONEC+NEC
      GO TO 200
C
  140 PV(N)=32*(NEI-1)+A(N)+1+NONEC+NEC
C
  200 CONTINUE
C
      nvme=0
      do 1100 n=1,nr
C
      if(ty(n).ne.'VME ') GO TO 1100
C
      nei=0
      DO 1110 I=1,nvme
      nei=nei+1
      if(modty(n) .eq. vmemod(i) .and. vmesn(i) .eq. sn(n)) go to 1130
 1110 continue
C
      nvme=nvme+1
      vmemod(nvme) = modty(n)
      vmesn(nvme) = sn(n)
*      pv(n)=32*(nvme-1)+a(n)+nonec+33*nec+nvme
      pv(n)=33*(nvme-1)+a(n)+nonec+33*nec 
      vmeidx(nvme) = pv(n) - a(n) 
      write(*,802) n,a(n),pv(n),nvme
802   format('N = ',i3,' a(n) = ', i3,' pv(n) = ',i3,' NVME = ',i3)
      GO TO 1100
C
 1130 pv(n)=32*(nei-1)+a(n)+nonec+33*nec+nvme
       write(*,802) n,a(n),pv(n),nvme
C
 1100 continue

      do i=1,nr
        write(*,900) i,cn(i),sn(i),a(i),pv(i),ty(i)
  900   format(5i5,a4)
      enddo
      do i=1,nvme
        write(*,901) vmemod(i),vmesn(i),vmeidx(i)
  901   format(a8,2i5)
      enddo
      RETURN
      END
