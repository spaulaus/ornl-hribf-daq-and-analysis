C$PROG READUM    - Reads scalers via call to CAMRED for process SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE READUM
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
      common/sd02a/ modty(512),vmemod(20),vmesn(20),vmeidx(20),nvme
      character*8   modty,     vmemod
      integer*4                           vmesn,    vmeidx,    nvme
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      INTEGER*4    KMD,SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD09/ CC(512),NN(512),AA(512),FF(512),VBUF(512),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
      COMMON/SD10/ MODEGO
      CHARACTER*4  MODEGO
C     ------------------------------------------------------------------
      REAL*4   RAN
C
      REAL*4 VNF(512),VOF(512),VDF(512),VDNOR,DENO
C
      EQUIVALENCE (VNF,VN),(VOF,VO),(VDF,VD)
C
      DATA ISEED/-1/
C
      SAVE
C
C     ------------------------------------------------------------------
C     READUM - READS THE SCALERS (VIA CALL TO CAMRED), COPIES ANY
C     ECL-SCALERS FROM SPECIAL INPUT ARRAYS TO NORMAL ARRAYS, COMPUTES
C     DIFFERENCES, NORMALIZES, AND CALLS COMPUM TO DO COMPUTED SCALERS
C     ------------------------------------------------------------------
C
      DO 10 I=1,NT                      !SAVE PREVIOUS SCALER VALUES
      VO(I)=VN(I)
   10 CONTINUE
C
      IF(MODEGO.EQ.'TST ') GO TO 30     !TEST FOR TST-MODE
C
      CALL CAMRED                       !READ NEW VALUES
      call vmered                       !Read VME scalers
C
      N=0
      DO 15 I=1,NR                      !COPY NON-ECL TYPES
      IF(TY(I).EQ.'ECL ') GO TO 15      !SKIP IF ECL TYPE
      if (ty(i) .eq. 'VME ') go to 15   !skip if VME type
      N=N+1
      VN(I)=VBUF(N)                     !SAVE IN VN
   15 CONTINUE
C
      DO 20 I=1,NR                      !COPY ANY ECL-TYPES
      IF(PV(I).EQ.0) GO TO 20           !TST FOR ECL-TYPE
      NDX=PV(I)                         !GET INDEX IN VBUF
      VN(I)=VBUF(NDX)                   !SAVE IN NEW-VALUE ARRAY
   20 CONTINUE
      GO TO 50
C
   30 DO 40 I=1,NR                      !GENERATE RANDOM VALUES
      VN(I)=9200.0+1000*RAN(ISEED)     !FOR TEST ONLY
   40 CONTINUE
C
   50 DO 60 I=1,NR                      !COMPUTE INCREMENTS
      VDF(I)=VN(I)-VO(I)                !SAVE IN DIFFERENCE ARRAY
   60 CONTINUE
C
      IF(NORI.EQ.0) GO TO 100           !TST FOR NO NORMALIZATION
      IF(NORI.GT.0) GO TO 80            !TST FOR NORM TO SCALER 
C
      DO 70 I=1,NR                      !OTHERWISE, NORM TO CTS/SEC
      VDF(I)=VDF(I)/SEC                 !VIA INTERNAL CLOCK
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
