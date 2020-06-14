C$PROG IDVAV
      FUNCTION IDVAV(NAME,NDX,IERR,MSER)
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
C
      INTEGER*4 NAME(2)
      INTEGER*4 MSG(10,4)
      CHARACTER*40 MSC(4)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/
     1'SYMBOL NOT FOUND ---                    ',
     2'ILLEGAL INDEXING ---                    ',
     3'ILLEGAL INDEX VALUE-                    ',
     4'VALUE UNDEFINED FOR-                    '/
C
      INTEGER*4    MSGTMP(10)
      CHARACTER*40 CMSG10
      CHARACTER*20 CMSG5
      EQUIVALENCE (CMSG10,MSGTMP(1))
      EQUIVALENCE (CMSG5, MSGTMP(6))
      CHARACTER*40 MSER
C
      INTEGER*4    X80000000
      DATA         X80000000/-2147483648/
C
      SAVE
C
C     **************************************************************
C     RETURNS VALUE ASSOCIATED WITH SUBSCRIPTED VARIABLE "NAME"
C     "NDX" SPECIFIES THE REQUIRED INDEX
C     **************************************************************
C
      IERR=0
      LDX=LOCSYM(NAME)                      !FIND SYMBOL IN TABLE
      IF(LDX.LE.0)          GO TO 210       !TST FOR EXIST
      IF(ISYT(LDX).NE.2)    GO TO 220       !TST FOR DIMENSIONED
      IF(NDX.LT.1)          GO TO 230       !TST INDEX VALUE
      IF(NDX.GT.ISYD(LDX))  GO TO 230       !TST INDEX VALUE
      IDX=ISYP(LDX)+NDX-1                   !GET INDEX IN ISYV-ARRAY
      IT=ISYV(IDX)                          !PICK UP VALUE
      IF(IT.EQ.X80000000) GO TO 240         !TST FOR UN-DEFINED
      IDVAV=IT                              !LOAD VALUE
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP MESSAGE
C     **************************************************************
C
  210 JJ=1
      GO TO 300
  220 JJ=2
      GO TO 300
  230 JJ=3
      GO TO 300
  240 JJ=4
      GO TO 300
C
  300 CMSG10=MSC(JJ)
      WRITE(CMSG5,320)NAME,NDX
  320 FORMAT(1X,2A4,'(',I5,')')
      CALL SQUEZL(MSER,22,40)
C
      MSER=CMSG10
C
      IERR=JJ
      IDVAV=0                               !RETURN ZERO
      RETURN
      END
