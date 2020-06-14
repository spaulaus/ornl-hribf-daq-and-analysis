C$PROG LTERMS
      SUBROUTINE LTERMS(IWD,IA,IB,KNOP,ILO,IHI,NT,IERR,MSER)
C
      INTEGER*4 IWD(1),KNOP(1),ILO(1),IHI(1)
C
      INTEGER*4 MSG(10,2),MSER(10)
      CHARACTER*40 MSC(2)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/
     1'SYNTAX ERROR - BLANK EXPRESSION FIELD   ',
     2'SYNTAX ERROR IN EXPRESSION FIELD        '/
C
      SAVE
C
C     **************************************************************
C     LOCATES THE TERMS IN EXPRESSION CONTAINED IN IWD (IA-IB)
C     ILO(I) = LOCATION OF FIRST CHARACTER OF ITH TERM
C     IHI(I) = LOCATION OF LAST  CHARACTER OF ITH TERM
C     KNOP(I)= 1,2,3,4 SAYS PRECEDING OPERATOR + - * /
C     NT     = # OF TERMS
C     **************************************************************
C
      IT=NXNB(IWD,IA,IB)                    !FIND FIRST NON-BLANK
      IF(IT.LE.0) GO TO 210                 !TST FOR NULL FIELD
      JO=NXOP(IWD,IT,IB,KOP)                !FIND FIRST OPERATOR
C
      N=0                                   !INIT # TERMS CNTR
      KNOP(1)=1                             !DEFAULT 1ST OP TO +
      ILO(1)=IT                             !DEFAULT 1ST LO-LIMIT
      JA=IT                                 !DEFAULT SCAN PNTR
C
      IF(IT.NE.JO) GO TO 10                 !TST FOR 1ST CHAR=OP
      KNOP(1)=KOP                           !SET FIRST KIND OP
      ILO(1)=JO+1                           !SET FIRST LO-LIMIT
      JA=IT+1                               !SET SCAN PNTR
      IF(JA.GT.IB) GO TO 220                !TST FOR ERROR
C
   10 JO=NXOP(IWD,JA,IB,KOP)                !FIND NEXT OP IF ANY
      IF(JO.LE.0) GO TO 20                  !IF NOT, MUST BE DONE
      N=N+1                                 !OTHERWISE, INC TERM CNTR
      IHI(N)  =JO-1                         !SET HI-LIMIT
      ILO(N+1)=JO+1                         !SET "NEXT" LO-LIMIT
      KNOP(N+1)=KOP                         !SET "NEXT" KIND OP
      JA=JO+1                               !INC SCAN PNTR
      IF(JA.GT.IB) GO TO 220                !BAD TO END WITH OP
      GO TO 10                              !GO BACK FOR MORE
C
   20 N=N+1                                 !NORMAL END - INC # TERMS
      IHI(N)=IB                             !SET HI-LIMIT
      NT=N                                  !SAVE # TERMS IN NT
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP ERROR MESSAGE
C     **************************************************************
C
  210 JJ=1
      GO TO 300
  220 JJ=2
C
  300 DO 310 I=1,10
      MSER(I)=MSG(I,JJ)
  310 CONTINUE
      IERR=JJ
      RETURN
      END
