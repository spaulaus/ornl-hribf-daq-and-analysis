C$PROG ISVAV
      FUNCTION ISVAV(NAME,IERR,MSER)
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
C
      INTEGER*4 NAME(2)
      INTEGER*4 MSG(10,3),MSER(10)
      CHARACTER*40 MSC(3)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/
     1'SYMBOL NOT FOUND IN TABLE ----- XXXXXXXX',
     2'INDEX REQUIRED FOR ------------ XXXXXXXX',
     3'VALUE UNDEFINED FOR ----------- XXXXXXXX'/
C
      INTEGER*4   X41,X5A,X80000000
      DATA        X41,X5A,X80000000/z'41',z'5A',-2147483648/
C
      SAVE
C
C     **************************************************************
C     RETURNS VALUE ASSOCIATED WITH SIMPLE VARIABLE "NAME"
C     OR DECODES "NAME" AS AN INTEGER IF 1ST CHAR NOT ALPHABETIC
C     **************************************************************
C
      IERR=0
      CALL ILBYTE(IT,NAME,0)                !PICK UP FIRST BYTE
      IF(IT.LT.X41) GO TO 20                !AND TST FOR ALPHABETIC
      IF(IT.GT.X5A) GO TO 20
C
      LDX=LOCSYM(NAME)                      !FIND SYMBOL IN TABLE
      IF(LDX.LE.0)          GO TO 210       !TST FOR EXIST
      IF(ISYT(LDX).NE.1)    GO TO 220       !TST FOR SIMPLE VARIABLE
      IT=ISYV(ISYP(LDX))
      IF(IT.EQ.X80000000)   GO TO 230       !TST FOR UN-DEFINED
      ISVAV=IT                              !LOAD THE VALUE
      RETURN
C
   20 ISVAV=NVALU(NAME,IERR,MSER)           !DECODE AS NUMERIC
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
C
  300 DO 310 I=1,10
      MSER(I)=MSG(I,JJ)
  310 CONTINUE
      MSER(9)=NAME(1)
      MSER(10)=NAME(2)
      IERR=JJ
      ISVAV=0                               !RETURN ZERO
      RETURN
      END
