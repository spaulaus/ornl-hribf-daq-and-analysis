C$PROG KVALUE
      FUNCTION KVALUE(IWD,IA,IB,IERR,MSER)
C
      INTEGER*4 IWD(1)
      INTEGER*4 MSG(10,1),MSER(10)
      CHARACTER*40 MSC(1)
      INTEGER*4 ILO(32),IHI(32),KNOP(32)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/
     1'NULL EXPRESSION FIELD                   '/
C
      SAVE
C
C     **************************************************************
C     RETURNS THE VALUE OF AN "EXPRESSION"
C     **************************************************************
C
      IERR=0
C
      CALL LTERMS(IWD,IA,IB,KNOP,ILO,IHI,NT,IERR,MSER) !LOCATE TERMS
C
      IF(IERR.NE.0) GO TO 320                 !TST FOR ERROR
      IF(NT.LE.0)   GO TO 210                 !TST FOR NULL
C
      ISUM=0                                  !ZERO SUM
      DO 150 N=1,NT                           !LOOP ON # TERMS
      ITERM=ITERV(IWD,ILO(N),IHI(N),IERR,MSER)!GET TERM VALUE
      IF(IERR.NE.0) GO TO 320                 !TST FOR ERROR
      IGO=KNOP(N)                             !GET OP-TYPE
      GO TO (110,120,130,140)IGO              !DO APPROPRIATE OPER
C
  110 ISUM=ISUM+ITERM                         !ADD TERM
      GO TO 150
  120 ISUM=ISUM-ITERM                         !SUB TERM
      GO TO 150
  130 ISUM=ISUM*ITERM                         !MUL TERM
      GO TO 150
  140 ISUM=ISUM/ITERM                         !DIV TERM
C
  150 CONTINUE
      KVALUE=ISUM
      RETURN
C
  210 JJ=1
      GO TO 300
C
  300 DO 310 I=1,10
      MSER(I)=MSG(I,JJ)
  310 CONTINUE
      IERR=JJ
  320 KVALUE=0
      RETURN
      END
