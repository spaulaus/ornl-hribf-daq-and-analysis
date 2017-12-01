C$PROG SCATREAD
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     Modified: RLVarner 21 Nov 2017 add VME scalers, based on SCAD
C     ******************************************************************
C
      SUBROUTINE SCATREAD
C
      IMPLICIT INTEGER*4 (A-Z)

      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      COMMON/SCATA/ MAXT,MAXS,SEC,ISET,NERR,MODEGO
      CHARACTER*4                 ISET,     MODEGO
      INTEGER*4     MAXT,MAXS,SEC,     NERR
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
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
*     ------------------------------------------------------------------
*
      COMMON/SCATE/ NSECI
C     ------------------------------------------------------------------
*     REAL*4 VNF(NSC),VOF(NSC),VDF(NSC),VDNOR,DENO
      REAL*8 VNF(NSC),VOF(NSC),VDF(NSC),VDNOR,DENO
C
      REAL*4 RAND,SECC
      integer*8 tmpvbuf
C
      EQUIVALENCE (VNF,VN),(VOF,VO),(VDF,VD)
C
      DATA ISEED/-1/
C
      SAVE
C
C     ******************************************************************
C     SCATREAD - READS THE SCALERS (VIA CALL TO CAMRED), COPIES ANY
C     ECL-SCALERS FROM SPECIAL INPUT ARRAYS TO NORMAL ARRAYS, COMPUTES
C     DIFFERENCES, NORMALIZES, AND CALLS COMPUM TO DO COMPUTED SCALERS
C     ******************************************************************
C
      DO 10 I=1,NT                      !SAVE PREVIOUS SCALER VALUES
      VO(I)=VN(I)
   10 CONTINUE
C
      IF(MODEGO.EQ.'TST ') GO TO 30     !TEST FOR TST-MODE
C
      CALL CAMRED                       !READ NEW VALUES
      call vmered                       !read VME scalers
C     
      N=0
      DO 15 I=1,NR                      !COPY NON-ECL TYPES
        IF(TY(I).EQ.'ECL ') GO TO 15      !SKIP IF ECL TYPE
        IF(TY(I).EQ.'VME ') GO TO 15      !SKIP IF VME TYPE
        N=N+1
        VN(I)=VBUF(N)                     !SAVE IN VN
   15 CONTINUE
C
      DO 20 I=1,NR                      !COPY ANY ECL-TYPES
      IF(PV(I).EQ.0) GO TO 20           !TST FOR ECL-TYPE
      NDX=PV(I)                         !GET INDEX IN VBUF
      if (ty(i) .eq. "VME ") then
*        write(6,*) "For a(i)=", a(i), " and index", ndx
         tmpvbuf = iand(vbuf(ndx),'7fffffff'x) !Get the unsigned bits
         tmpvbuf = tmpvbuf + iand(vbuf(ndx), '80000000'x) ! get the sign bit
*        write(6,"(A,Z20)") "32bit value = ", tmpvbuf
         if (a(i) .eq. 0) then
*           write(6,*) "extended precision words=", vbuf(ndx-a(i)+32)
*           write(6,*) "bits=", ibits(vbuf(ndx-a(i)+32),0,16)
*           write(6,*) "located at ", ndx-a(i)+32
            tmpvbuf = tmpvbuf +
     .              ibits(vbuf(ndx-a(i)+32),0,16) *
     .              '100000000'x
*        write(6,"(A,Z20)") "48bit value = ", tmpvbuf
         elseif (a(i) .eq.16) then
*           write(6,*) "extended precision words=", vbuf(ndx-a(i)+32)
*           write(6,*) "bits=", ibits(vbuf(ndx-a(i)+32),16,16)
*           write(6,*) "located at ", ndx-a(i)+32
            tmpvbuf = tmpvbuf +
     .              ibits(vbuf(ndx-a(i)+32),16,16) *
     .              '100000000'x
         endif  
         vn(i) = tmpvbuf
      else
         VN(I)=VBUF(NDX)                !SAVE IN NEW-VALUE ARRAY
      endif

   20 CONTINUE
      GO TO 50
C
   30 DO 40 I=1,NR                      !GENERATE RANDOM VALUES
      VN(I)=10000*RAN(ISEED)            !FOR TEST ONLY
   40 CONTINUE
C
   50 DO 60 I=1,NR                      !COMPUTE INCREMENTS
      VDF(I)=VN(I)-VO(I)                !SAVE IN DIFFERENCE ARRAY
   60 CONTINUE
C
      IF(NORI.EQ.0) GO TO 100           !TST FOR NO NORMALIZATION
      IF(NORI.GT.0) GO TO 80            !TST FOR NORM TO SCALER 
C
      SECC=SEC
      IF(SECC.LE.0) THEN
      CALL TIMEKEEP('GET ',TNOW)
      SECC=TNOW-NSECI
      IF(SECC.LT.1.0) SECC=1.0
      NSECI=TNOW
      ENDIF
C
      DO 70 I=1,NR                      !OTHERWISE, NORM TO CTS/SEC
      VDF(I)=VDF(I)/SECC                !VIA INTERNAL CLOCK
   70 CONTINUE
      GO TO 100  
C
   80 DENO=VDF(NORI)                    !COMPUTE NORMALIZATION FACTOR
      IF(DENO.LT.1.0) DENO=1.0          !FROM SPECIFIED SCALER
      VDNOR=NORF/DENO
C
      DO 85 I=1,NR                      !LOOP TO NORMALIZE
      VDF(I)=VDNOR*VDF(I)               !DIFFERENCES (RATES)
   85 CONTINUE
C
  100 CALL COMPUM                       !DO ANY "COMPUTED VALUES"
C
      RETURN
      END
