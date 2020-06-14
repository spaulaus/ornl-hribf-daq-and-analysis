C$PROG LOOPER
      SUBROUTINE LOOPER(JWD,IERR,MSER)
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/OOO/ LLLST(40),NULST(40),NLLST
C
      INTEGER*4 IWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4 JWD(32),KWD(32),LOPL(32,40),LLSAV(4)
C
      INTEGER*4 LINDX(2),MSG(10,6),MSER(10)
      CHARACTER*40 MSC(6)
C
      CHARACTER*4  CIWD(20),CKWD(32)
C
      CHARACTER*8  CKWD8
C
      EQUIVALENCE (IWD,KWD),(CIWD,KWD),(CKWD,KWD),(CKWD8,KWD(6))
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/'ERRORS DETECTED IN DO STATEMENT         ',
     2         'DO LOOP NOT TERMINATED AFTER 40 LINES   ',
     3         'DO LOOP NOT TERMINATED - EOF ENCOUNTERED',
     4         'LOOP NOT TERMINATED WITH "CONTINUE"     ',
     5         '"DO" ENCOUNTERED BEFORE LOOP TERMINATION',
     6         'SYNTAX ERROR OR ILLEGAL DO LOOP INDEX   '/
C
      SAVE
C
C     **************************************************************
C     ROUTINE TO EXPAND LOOPS
C     READS SOURCE CODE FROM "LIN" & WRITES EXPANDED CODE ON "LOU"
C     LISTING AND DIAGNOSTICS ARE OUTPUT TO "LPR"
C     **************************************************************
C
C     ANALYZE "DO LLABL LINDX=NDXLO,NDXHI,[LINC]" - PASSED IN "JWD"
C
C     **************************************************************
C
      LINC=1
      NDXLO=1
      NDXHI=1
      IERR=0
      NSOLS=NSOL                        !SORCE LINE# FOR "DO"
      NLLST=0                           !LOOP LABEL CNTR
      ILO=JWD(23)                       !SCAN LO-LIMIT
      IHI=JWD(24)                       !SCAN HI-LIMIT
      LLABL=JWD(27)                     !END-LOOP LABLE
C
      CALL REFOR(JWD,LWD,ITYP,NF,ILO,IHI,NTER)
C
      IF(NTER.NE.0)    IERR=IERR+1      !TST FOR TRUNK-ERROR
      IF(NF.LT.5)      IERR=IERR+1      !TST FOR TOO FEW  FIELDS
      IF(NF.GT.6)      IERR=IERR+1      !TST FOR TOO MANY FIELDS
      IF(ITYP(3).NE.1) IERR=IERR+1      !TST LOOP-INDEX TYPE
      IF(IERR.NE.0) GO TO 10            !TST FOR ANY ERRORS SO FAR
C
      LINDX(1)=LWD(1,3)                 !SAVE END-LOOP LABEL
      LINDX(2)=LWD(2,3)                 !SAVE END-LOOP LABEL
C
      NDXLO=ISVAV(LWD(1,4),IERR,MSER)   !1ST LOOP-INDEX
      IF(IERR.NE.0) GO TO 20
      NDXHI=ISVAV(LWD(1,5),IERR,MSER)   !2ND LOOP-INDEX
      IF(IERR.NE.0) GO TO 20
      LINC=1
      IF(NF.NE.6) GO TO 5
      LINC=ISVAV(LWD(1,6),IERR,MSER)    !LOOP STEP SIZE
      IF(IERR.NE.0) GO TO 10
C
    5 IF(NDXLO.LT.1.OR.NDXLO.GT.1000)     GO TO 10
      IF(NDXHI.LT.NDXLO.OR.NDXHI.GT.1000) GO TO 10
      IF(LINC.LT.1.OR.LINC.GT.NDXHI)      GO TO 10
      GO TO 20
C
   10 IERR=1
      DO 15 I=1,10
      MSER(I)=MSG(I,6)
   15 CONTINUE
C
   20 CALL SAVLIN(JWD,MSER,IERR,'DONE',NSOL,1)
      IF(IERR.EQ.0) GO TO 25
      RETURN
C
C     **************************************************************
C     READ IN ALL LINES (TO END OF LOOP) AND SAVE IN "LOPL"
C     **************************************************************
C
   25 NL=0                              !ZERO LINE CNTR
   30 NL=NL+1                           !INC LINE CNTR
      IF(NL.GT.40) GO TO 1020           !TST FOR TOO MANY LINES
      READ(LIN,40,END=1030)IWD          !READ IN NEXT LINE
   40 FORMAT(20A4)
      NSOL=NSOL+1
C
      CALL CLINE(KWD,JERR,MSER)         !CLASSIFY LINE
      KWD(30)=NSOL                      !SAVE SOURCE LINE #
      KWD(31)=0
      KWD(32)=0
C
      CALL SAVLIN(KWD,MSER,JERR,'DONE',NSOL,1)
C
      IF(CKWD(21).EQ.'DO  ') GO TO 1050 !TST FOR "DO" ENCOUNTERED
C
      IF(JERR.NE.0) IERR=IERR+1         !TST FOR ERROR
C
      DO 50 I=1,32                      !SAVE IN LOOP-LINE LIST
      LOPL(I,NL)=KWD(I)
   50 CONTINUE
      IF(CKWD(26).EQ.'    ') GO TO 30
      NLLST=NLLST+1
      LLLST(NLLST)=KWD(26)
      IF(KWD(26).NE.LLABL)  GO TO 30    !TST FOR END-LOOP LABEL
      IF(CKWD(21).NE.'CONT') GO TO 1040 !TST FOR "CONTINUE"
C
      IF(IERR.NE.0) RETURN              !DON'T EXPAND IF ANY ERRORS
C
C     **************************************************************
C     EXPAND THE LOOP - REPEAT REQUIRED # OF TIMES
C     **************************************************************
C
      DO 200 NN=NDXLO,NDXHI,LINC
C
      DO 70 I=1,21
      CKWD(I)='    '
   70 CONTINUE
      DO 72 I=22,32
      KWD(I)=0
   72 CONTINUE
      DO 74 I=26,29
      CKWD(I)='    '
   74 CONTINUE
C
      KWD(3)=LINDX(1)
      KWD(4)=LINDX(2)
      CKWD(5)='=   '
      WRITE(CKWD8,80)NN
   80 FORMAT(I8)
      CKWD(21)='EQU '
      KWD(22)=0
      KWD(23)=9
      KWD(24)=28
      KWD(25)=17
      KWD(30)=NSOLS
C
      WRITE(LOU,IOSTAT=IOS)KWD
      CALL IOERR(IOS)
C
      DO 90 I=1,NLLST
      NULST(I)=NXLABL(IDUM)
   90 CONTINUE
C
      DO 120 N=1,NL
      K=0
      DO 100 I=26,29
      K=K+1
      LLSAV(K)=LOPL(I,N)
      CALL LASUB(LOPL(I,N))
  100 CONTINUE
C
      WRITE(LOU,IOSTAT=IOS)(LOPL(K,N),K=1,32)
      CALL IOERR(IOS)
C
      K=0
      DO 110 I=26,29
      K=K+1
      LOPL(I,N)=LLSAV(K)
  110 CONTINUE
  120 CONTINUE
  200 CONTINUE
      RETURN
C
C     **************************************************************
C     LOAD UP ERROR MESSAGES AND COPY REMAINDER OF SOURCE TO .IMF
C     **************************************************************
C
C1010 JJ=1
C     GO TO 1100
 1020 JJ=2
      GO TO 1100
 1030 JJ=3
      GO TO 1100
 1040 JJ=4
      GO TO 1100
 1050 JJ=5
C
 1100 DO 1110 I=1,10
      MSER(I)=MSG(I,JJ)
 1110 CONTINUE
C
      DO 1210 I=1,20
      CIWD(I)='    '
 1210 CONTINUE
      WRITE(CIWD,1220)
 1220 FORMAT('! ! ! ! ! COMPILATION TERMINATED ! ! ! ! !')
      CALL LODJWD(IWD,KWD)
      IERR=1
      CALL SAVLIN(KWD,MSER,IERR,'DONE',0,1)
      IERR=0
      KWD(22)=0
C
 1230 READ(LIN,40,END=1300)IWD
      NSOL=NSOL+1
      CALL SAVLIN(KWD,MSER,IERR,'DONE',NSOL,1)
      GO TO 1230
C
 1300 IERR=1
      RETURN
      END
