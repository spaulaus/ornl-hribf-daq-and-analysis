C$PROG SPLOT     - Generates a printer-plot of 1-D data
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SPLOT(IDATA,ISKF,ILOX,NA,NB,NS)
C
      INTEGER*4    IDATA(2048,3),A(92),JSYL(4),JSYN(4),IKK(3)
      character*4  cjsyl(4), cjsyn(4)
      equivalence (cjsyl, jsyl), (cjsyn, jsyn)
C
      INTEGER*4    BLANK
      character*4  cBLANK
      equivalence (cblank,blank)
C
      DIMENSION ISKF(1),FSUM(3)
C
      DATA ICMAX,XCMAX/92,92.0/
C
      DATA (cJSYL(I),I=1,4)/'+   ','O   ','B   ','    '/
      DATA (cJSYN(I),I=1,4)/'F   ','N   ','M   ','    '/
C
      DATA cBLANK/'    '/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     FIND MAX AND MIN OF IDATA
C     ------------------------------------------------------------------
C   
      SUM=0.0
      NDP=NS
      IF(NS.GT.3) NDP=3
      JMIN=4000000
      JMAX=0
      DO 8 J=1,NS
      DO 8 I=NA,NB
      ITST=IABS(IDATA(I,J))
      IF(ITST.LT.JMIN) JMIN=ITST
      IF(ITST.GT.JMAX) JMAX=ITST
    8 CONTINUE
      IF(JMIN.LT.1) JMIN=1
      YMIN=JMIN
      BIG=JMAX
      RAT=BIG/YMIN
      DO 20 I=1,ICMAX
      A(I)=BLANK
   20 CONTINUE
C   
C     ------------------------------------------------------------------
C     CHOOSE NO. OF CYCLES 0.5 THRU 5
C     ------------------------------------------------------------------
C   
      IF(RAT.GT.2.0) GO TO 25
      CYCL=0.5
      GO TO 40
   25 DO 30 I=1,5
      RATST=10**I
      IF(RAT.LE.RATST) GO TO 32
   30 CONTINUE
      CYCL=5
      GO TO 40
   32 CYCL=I
   40 BIGL=ALOG(BIG)
      BB=XCMAX/(CYCL*ALOG(10.0))
      AA=XCMAX-BB*BIGL
      WRITE(7,110)CYCL,BIG
  110 FORMAT(17H NO. OF CYCLES = ,F4.1,6X,5HMAX =,F7.0/)
      WRITE(7,112)
  112 FORMAT(1H ,' CHAN# DAT-BGD     CAL     BGD')
      IX=ILOX
      FSUM(1)=0.0
      FSUM(2)=0.0
      FSUM(3)=0.0
      DO 300 NN=NA,NB
      DO 188 NSET=1,NS
      JRT=JSYL(NSET)
      ITST=IDATA(NN,NSET)
      IF(ITST.LT.0) JRT=JSYN(NSET)
      ITST=IABS(ITST)
      IF(ITST.LT.1) ITST=1
      DATA=ITST
      DAT=ALOG(DATA)
      IK=AA+BB*DAT+0.5
      IF(IK.LT.1) IK=1
      IF(IK.GT.ICMAX) IK=ICMAX
      A(IK)=JRT
      IKK(NSET)=IK
      FSUM(NSET)=FSUM(NSET)+IDATA(NN,NSET)
  188 CONTINUE
      SUM=SUM+IDATA(NN,1)
      IF(NDP.EQ.1) WRITE(7,210)ISKF(NN),IX,(IDATA(NN,I),I=1,NDP),A
      IF(NDP.EQ.2) WRITE(7,212)ISKF(NN),IX,(IDATA(NN,I),I=1,NDP),A
      IF(NDP.EQ.3) WRITE(7,214)ISKF(NN),IX,(IDATA(NN,I),I=1,NDP),A
  210 FORMAT(1H ,A1,I5,I8,92A1)
  212 FORMAT(1H ,A1,I5,2I8,92A1)
  214 FORMAT(1H ,A1,I5,3I8,92A1)
      IX=IX+1
      DO 220 I=1,NS
      J=IKK(I)
      A(J)=BLANK
  220 CONTINUE
  300 CONTINUE
      WRITE(7,310)(FSUM(I),I=1,NS)
  310 FORMAT(1H /1H ,'SPECTRA SUMS =',3F10.0)
      RETURN
      END
