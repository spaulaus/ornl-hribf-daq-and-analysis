C$PROG BACKER
      SUBROUTINE BACKER(NCMP,M1,M2)
C
      COMMON/MAINV8/ A(50,50),B(50,50),DET,IFS
      REAL*8         A,       B,       DET
C
      COMMON/AAX/ SPECT(256,19),ISPK(16384),IBAK(16384),ICAL(16384)
      COMMON/BBB/ XAG(250,7),NPK
      COMMON/CCC/ BGD(256),WT(256),YCAL(256),GAU(200),NG(16)
      COMMON/DDD/ IHOL(16),XP(16),AREA(16),BETA(18),LOCA(250),BIAS
      COMMON/EEE/ ID,ILO,IHI,KWD,FWHMA,FWHMB,EOOO,GAINN,WDNC,DELCH,BSTD
      COMMON/FFF/ HWID(16),QFN,QFLO,IFBGD,NTRY,MSN,NCH,KPK,I1,I2
      COMMON/GGG/ MXNPK,MXRGL,MXPIS,NBNF
      COMMON/HHH/ IGAUS,KNBAK,KNVAR,KVW
C
      SAVE
C
      IF(M1.GT.M2.OR.NCMP.LT.1) RETURN
      NMAX=NCMP+1
      DO 40 N=2,NMAX
      J=N-1
      SUM=0.0
      DO 38 I=M1,M2
      IF(WT(I).EQ.0.0) GO TO 38
      SUM=SUM+WT(I)*SPECT(I,1)*SPECT(I,N)
   38 CONTINUE
      B(J,1)=SUM
   40 CONTINUE
      NUP=NMAX-1
      DO 60 I=1,NUP
      DO 58 J=1,I
      SUM=0.0
      DO 56 K=M1,M2
      IF(WT(K).EQ.0.0) GO TO 56
      SUM=SUM+WT(K)*SPECT(K,I+1)*SPECT(K,J+1)
   56 CONTINUE
      A(I,J)=SUM
      A(J,I)=SUM
   58 CONTINUE
   60 CONTINUE
      CALL DMAINV8(NUP,1)
      DO 70 I=M1,M2
   70 BGD(I)=0
      DO 90 J=1,NCMP
      DO 90 I=M1,M2
   90 BGD(I)=BGD(I)+B(J,1)*SPECT(I,J+1)
      IF(BSTD.GT.0.0) GO TO 150
      DO 100 I=M1,M2
      IF(SPECT(I,1).GT.BGD(I)) WT(I)=0.0
  100 CONTINUE
      RETURN
  150 DO 160 I=M1,M2
      IF(BGD(I).LE.1.0) GO TO 160
      IF((SPECT(I,1)-BGD(I)).GT.BSTD*SQRT(BGD(I))) WT(I)=0.0
  160 CONTINUE
      RETURN
      END
