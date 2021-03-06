C$PROG CAMREDD   - Reads CAMAC scalers for RATE data
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 03/21/2000
C     ******************************************************************
C
      SUBROUTINE CAMREDD
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/DD12/ NRATSCA(16)
      INTEGER*4    NRATSCA
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
      INTEGER*4    VBUF(16),STAT(280),NCALL,MODE,I
C
      REAL*4       TLAST,TNOW,DELT,VLAST(16),VNOW(16),RATE
C
      REAL*4       SECVLU
C
      DATA MODE/1/
      DATA NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 100
      TLAST=SECVLU(0.0)
      CALL CAMLIST(GC,GN,GA,GF,MODE,GBUF,ngrap,STAT)
      call gvmered
*
      NCALL=1
*
      do i=1,nsca
        if (gpv(i) .eq. 0) vbuf(i) = gbuf(i)
        if (gpv(i) .ne. 0)  vbuf(i) = gbuf(gpv(i))
      enddo
*
      DO 10 I=1,NSCA
      VLAST(I)=VBUF(I)
   10 CONTINUE
      RETURN
C
  100 DO 110 I=1,ngrap
      STAT(I)=0
  110 CONTINUE
C
      TNOW=SECVLU(0.0)
      DELT=TNOW-TLAST
      TLAST=TNOW
C
      CALL CAMLIST(GC,GN,GA,GF,MODE,GBUF,ngrap,STAT)
C
      DO 150 I=1,ngrap
C
      IF(STAT(I).NE.0) CALL CAMERRR(GC(I),GN(I),GA(I),GF(I),STAT(I))
C
  150 CONTINUE
C
      call gvmered
*
      do i=1,nsca
        if (gpv(i) .eq. 0) vbuf(i) = gbuf(i)
        if (gpv(i) .ne. 0)  vbuf(i) = gbuf(gpv(i))
      enddo
*
      DO 160 I=1,NSCA
C
      VNOW(I)=VBUF(I)
C
      RATE=(VNOW(I)-VLAST(I))/DELT
C
      NRATSCA(I)=RATE
C
      VLAST(I)=VNOW(I)
C
  160 CONTINUE
C
      RETURN
      END
