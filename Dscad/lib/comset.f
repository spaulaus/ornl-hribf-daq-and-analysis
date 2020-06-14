C$PROG COMSET    - Reads & processes input lines for computed scalers
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/26/2005
C     ******************************************************************
C
      SUBROUTINE COMSET
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MAXLIS=400)
C
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD05/ LISI(400),SYM(3,50),OPR(50),NLS, NSY, DSPF
      INTEGER*4    LISI,     SYM,      OPR,    NLS, NSY, DSPF
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      COMMON/SD13/ ISET,LULG
      CHARACTER*4  ISET
      INTEGER*4         LULG
C     ------------------------------------------------------------------
      INTEGER*4    IWD(20)
C
      INTEGER*4    X26
      DATA         X26/'26'X/
C
      CHARACTER*4  IDONE
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     COMSET - READS AND PROCESSES INPUT LINES FOR COMPUTED SCALERS
C     ------------------------------------------------------------------
C     LISI     - CONTAINS CONCATINATED INPUT LINES FOR SYMBOL DEF
C     NLS      = # OF WORDS SET IN LISI
C
C     SYM      - CONTAINS SYMBOL LIST FOR SINGLE COMPUTED VALUE
C     SYM(I,1) - CONTAINS SYMBOL BEING DEFINED
C     SYM(I,N) - CONTAINS N-1TH SYMBOL USED IN COMPUTATION
C     OPR(N)   = OPERATOR (1,2,3,4 FOR +-/*) ASSOCIATED WITH SYM(I,N)
C     NSY      = # OF SYMBOLS (INCLUDING DEFINED) FOR SINGLE VALUE
C
C     DSPF     = 'YES '/'NO  ' SAYS SYMBOL TO BE/NOT-BE DISPLAYED
C     ------------------------------------------------------------------
C
      NPO=0                             !ZERO COMP-LIST COUNTER
      NERR=0                            !ZERO # COMPILATION ERRORS
      IDONE='NO  '                      !RESET DONE-FLAG
C
   10 NSY=0                             !ZERO # OF SYMBS FOR THIS LIST
      NLS=0                             !ZERO CONCATINATED-LIST CNTR
      READ(LULG,20,END=100)IWD          !READ FIRST SOURCE LINE
   20 FORMAT(20A4)
C
      DO 30 I=1,20                      !SAVE IN CONCATINATED LIST
      NLS=NLS+1
      IF(NLS.GT.MAXLIS) GO TO 200
      LISI(NLS)=IWD(I)
   30 CONTINUE
C
   40 READ(LULG,20,END=60)IWD           !READ SUBSEQUENT LINES
C
      CALL ILBYTE(IT,IWD,0)             !TST FOR CONTINUATION &
C
      IF(IT.NE.X26) GO TO 70            !IF NOT, GO PROCESS LISI
C
      CALL ISBYTE('20'X,IWD,0)          !OTHERWISE, ZOT THE &
      DO 50 I=1,20                      !AND ACCUMULATE IT
      NLS=NLS+1
      IF(NLS.GT.MAXLIS) GO TO 200
      LISI(NLS)=IWD(I)
   50 CONTINUE
      GO TO 40                          !GO READ IN NEXT LINE
C
   60 IDONE='YES '
   70 CALL SYMBOP                       !CONSTRUCT SYMB-OPER LIST
      CALL COMPRO                       !PROCESS OUT TO POL-GOL LIST
      IF(IDONE.EQ.'YES ') GO TO 100     !TST FOR DONE
      BACKSPACE LULG                    !BACKSPACE OVER LAST RECORD
      GO TO 10                          !GO BACK FOR NEXT LIST
C
  100 RETURN
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'MORE THAN 19 CONTINUATION LINES - NOT ALLOWED')
      WRITE(6,210)
  210 FORMAT(1H ,'sorry about that')
      CALL EXIT(3)
      END
