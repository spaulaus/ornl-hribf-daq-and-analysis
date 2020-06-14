C$PROG SCATCOM
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE SCATCOM(LU)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MAXLIS=400)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SCATA/ MAXT,MAXS,SEC,ISET,NERR,MODEGO
      CHARACTER*4                 ISET,     MODEGO
      INTEGER*4     MAXT,MAXS,SEC,     NERR
C     ------------------------------------------------------------------
      COMMON/SCAT0/ SCATBUF(8000,2),NSCAT,SCATDMP,SCATCLR,SCATERR
      CHARACTER*4                         SCATDMP,SCATCLR
      INTEGER*4     SCATBUF,        NSCAT,                SCATERR
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SCAT4/ LISI(400),SYM(3,50),OPR(50),NLS,NSY,DSPF
C     ------------------------------------------------------------------
C
      INTEGER*4     IWD(20)
C
      INTEGER*4     X26
C
      DATA          X26/'26'X/
C
      CHARACTER*4   IDONE
C
      SAVE
C
C     ******************************************************************
C     SCATCOM - READS AND PROCESSES INPUT LINES FOR COMPUTED SCALERS
C     ******************************************************************
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
C     ******************************************************************
C
      NPO=0                             !ZERO COMP-LIST COUNTER
      NERR=0                            !ZERO # COMPILATION ERRORS
      IDONE='NO  '                      !RESET DONE-FLAG
C
   10 NSY=0                             !ZERO # OF SYMBS FOR THIS LIST
      NLS=0                             !ZERO CONCATINATED-LIST CNTR
      READ(LU,20,END=100)IWD            !READ FIRST SOURCE LINE
   20 FORMAT(20A4)
C
      DO 30 I=1,20                      !SAVE IN CONCATINATED LIST
      NLS=NLS+1
      IF(NLS.GT.MAXLIS) GO TO 200
      LISI(NLS)=IWD(I)
   30 CONTINUE
C
   40 READ(LU,20,END=60)IWD             !READ SUBSEQUENT LINES
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
      BACKSPACE 1                       !BACKSPACE OVER LAST RECORD
      GO TO 10                          !GO BACK FOR NEXT LIST
C
  100 RETURN
C
  200 WRITE(CMSSG,205)
  205 FORMAT('MORE THAN 19 SNIT CONTINUATION LINES - NOT ALLOWED')
      CALL MESSLOG(LOGUT,LOGUP)
      SCATERR=1
      RETURN
      END
