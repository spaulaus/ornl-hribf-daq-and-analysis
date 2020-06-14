C$PROG SLAPIT    - Produces printer-plots of 1-D data
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SLAPIT(DATA,ID,KIPL,LCYC,NCFS,ILO,IHI,JPRZ)
C
      CHARACTER*4  KIPL
C   
      INTEGER*4    A(100)
C
      DIMENSION    DATA(*)
C
      DATA         ICMAX,XCMAX/100,98.0/
C
      DATA         IKH,IKL,IC,KCYS/4*0/
C
      INTEGER*4    BLANK,STAR,MINUS
      character*4  cBLANK,cSTAR,cMINUS
      equivalence  (cblank, blank), (cstar,star), (cminus,minus)
C
      DATA         cBLANK,cSTAR,cMINUS/'    ','*   ','-   '/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ID    = ID NUMBER
C     KIPL  = 'LIN ' OR 'LOG '
C     LCYC  = # OF CYCLES            FOR LOG PLOTS
C     NCFS  = # OF COUNTS FULL-SCALE FOR LINEAR PLOTS
C     ILO   = FIRST ARRAY INDEX TO PLOT
C     IHI   = LAST  ARRAY INDEX TO PLOT
C     JPRZ  = 0 SAYS DO NOT PRINT "REPEATED ZEROS"
C     JPRZ  = 1 SAYS        PRINT "REPEATED ZEROS"
C     IPLER - NOT USED
C     ------------------------------------------------------------------
C   
      JATL=0
      KCYC=LCYC
      CFS=NCFS
      KSUPP=JPRZ
      DO 10 I=1,ICMAX
      A(I)=BLANK
   10 CONTINUE
C   
C     ------------------------------------------------------------------
C     FIND MIN AND MAX OF DATA
C     ------------------------------------------------------------------
C   
      YMIN=DATA(ILO)
      YMAX=YMIN
      DO 20 I=ILO,IHI
      IF(DATA(I).LT.YMIN) YMIN=DATA(I)
      IF(DATA(I).GT.YMAX) YMAX=DATA(I)
   20 CONTINUE
C   
      MINY=YMIN
      MAXY=YMAX
      BIG=YMAX
      NEGY=0
      IF(MINY.LT.0) NEGY=MINY
      IF(KCYC.GT.0.OR.NCFS.GT.0) GO TO 25
      IF(KIPL.EQ.'LOG ') JATL=1
   25 WRITE(7,30)ID
   30 FORMAT(1H ,'RUN NO.',I8)
      IF(JATL.EQ.1) GO TO 50
      IF(KCYC.GT.0) GO TO 60
C   
C     ------------------------------------------------------------------
C     SET UP SCALE FOR LINEAR PLOTS
C     ------------------------------------------------------------------
C   
      IF(CFS.GT.0.0) GO TO 45
      RANGE=YMAX-YMIN
C   
      DO 35 I=1,10
      RANGE=RANGE/10.0
      IF(RANGE.LT.1.0) GO TO 40
   35 CONTINUE
      RETURN
C   
   40 IEX=I-1
      MULY=10**IEX
      JSND=MAXY/MULY
      KSND=JSND
      IF(KSND.LT.2) JSND=2
      IF(KSND.GE.2.AND.KSND.LT.5) JSND=5
      IF(KSND.GE.5) JSND=10
      CFS=JSND*MULY
      IF(CFS.LT.50.0) CFS=50.0
C   
   45 AA=0.0
      DENO=CFS
      IF(YMIN.LT.0.0) DENO=CFS-YMIN
      BB=XCMAX/DENO
      IF(YMIN.LT.0.0) AA=-BB*YMIN
      GO TO 70
C   
C     ------------------------------------------------------------------
C     FIND # OF CYCLES FOR AUTO-SCALED LOG PLOTS
C     ------------------------------------------------------------------
C   
   50 IF(YMAX.LT.1.0) RETURN
      RAT=YMAX
      IF(YMIN.GT.1.0) RAT=YMAX/YMIN
C   
      DO 54 I=1,7
      RATST=10**I
      IF(RAT.LE.RATST) GO TO 56
   54 CONTINUE
      I=7
C   
   56 KCYC=I
   60 CYCL=KCYC
      BIGL=ALOG(YMAX)
      BB=XCMAX/(CYCL*ALOG(10.0))
      AA=XCMAX-BB*BIGL
      KCYS=KCYC/2
      IF(KCYS.LT.1) KCYS=1
C   
   70 ISUM=0
      SUM=0.0
      N=ILO-2
C   
      WRITE(7,104)
      IF(KCYC.GT.0) WRITE(7,102)KCYC
      IF(KCYC.EQ.0) WRITE(7,103)CFS
  102 FORMAT(1H ,'SEMILOG PLOT  NO. OF CYCLES = ',I1/)
  103 FORMAT(1H ,'LINEAR PLOT - COUNTS FULL SCALE = ',F8.0/)
  104 FORMAT(1H )
C   
C     ------------------------------------------------------------------
C     THIS IS THE MAIN LOOP
C     ------------------------------------------------------------------
C   
      KLZ=1                             !KLZ=1 SAYZ LAST DATA NON-ZERO
      DO 300 I=ILO,IHI
C   
      SUM=SUM+DATA(I)
      IF(DATA(I).GT.0.0) GO TO 110
      IF(KIPL.EQ.'LIN '.AND.NEGY.EQ.0) GO TO 162
      IF(KIPL.EQ.'LOG ') GO TO 162
  110 KLZ=1
C   
      DAT=DATA(I)
      IF(KCYC.GT.0) DAT=ALOG(DAT)
  142 IC=AA+BB*DAT+1.5
      IF(IC.GE.1.OR.KCYC.EQ.0) GO TO 160
      BIG=BIG*10.0**(-KCYS)
  152 BIGL=ALOG(BIG)
      AA=XCMAX-BB*BIGL
      GO TO 142
  160 IF(IC.LE.ICMAX.OR.KCYC.EQ.0) GO TO 175
      BIG=BIG*10.0**KCYS
      GO TO 152
  162 IF(KSUPP.NE.0) GO TO 175          !TST FOR REPEATED-ZERO SUPP
C   
      IF(KLZ.LE.1) THEN                 !FIRST ZERO - PRINT IT
                   KLZ=2
                   GO TO 175
                   ENDIF
C   
      IF(KLZ.EQ.2) THEN                 !SECOND ZERO - FLAG IT
                   KLZ=3
                   WRITE(7,170)
  170              FORMAT(1H ,'* * *')
                   N=N+1
                   GO TO 300
                   ENDIF
C   
      IF(KLZ.GE.3) THEN                 !3RD ZERO (OR GREATER) - SKIP
                   N=N+1
                   GO TO 300
                   ENDIF
C   
  175 IF(IC.GT.ICMAX) IC=ICMAX
      IF(IC.LT.1) IC=1
C   
      A(IC)=STAR
      IF(DATA(I).LT.0.0) A(IC)=MINUS
      N=N+1
      WRITE(7,210)N,DATA(I),SUM,A
  210 FORMAT(1H ,I6,2F10.0,100A1)
      A(IC)=BLANK
  300 CONTINUE
      WRITE(7,310)SUM
  310 FORMAT(1H ,/1H ,'SUM = ',E12.4)
      WRITE(7,320)
  320 FORMAT(1H1)
      RETURN
      END
