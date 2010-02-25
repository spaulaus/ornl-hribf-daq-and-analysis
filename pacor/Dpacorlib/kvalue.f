C$PROG KVALUE
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      FUNCTION KVALUE(IWD,IA,IB,IERR)
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C   
      INTEGER*4 IWD(20),LWD(2,40),ITYP(40),KNOP(40)
C
      SAVE
C   
C     ************************************************************
C     FUNCTION TO EVALUATE OPERAND EXPRESSIONS
C     ************************************************************
C   
      IF(IA.GT.IB) GO TO 210
      IERR=0
      IVAL=0
      CALL REFMT(IWD,LWD,ITYP,KNOP,IA,IB,NF,NTER)
      IF(NTER.NE.0) GO TO 210
      IF(NF.LE.0)   GO TO 210
C   
      DO 200 N=1,NF
      IF(ITYP(N).EQ.1) GO TO 40
C   
C     IT IS A CONSTANT
C   
      ITERM=JOCO(LWD(1,N),IERR)
      IF(IERR.NE.0) GO TO 210
      GO TO 100
C   
C     IT IS A SYMBOL - LOOK UP VALUE IN TABLE
C   
   40 DO 50 J=1,NSYM
      IF(LWD(1,N).NE.ISYN(J)) GO TO 50
      GO TO 60
   50 CONTINUE
      GO TO 220
C   
   60 ITERM=ISYV(J)
  100 IGO=KNOP(N)
      IF(IGO.LT.1.OR.IGO.GT.4) GO TO 210
C   
      GO TO (110,120,130,140),IGO
C   
  110 IVAL=IVAL+ITERM
      GO TO 200
  120 IVAL=IVAL-ITERM
      GO TO 200
  130 IVAL=IVAL*ITERM
      GO TO 200
  140 IVAL=IVAL/ITERM
  200 CONTINUE
      KVALUE=IVAL
      RETURN
C   
C     ************************************************************
C     RETURN ERROR MESSAGES
C     ************************************************************
C   
  210 WRITE(CMSSG,215)
  215 FORMAT('SYNTAX ERROR IN EXPRESSION')
      GO TO 300
C   
  220 WRITE(CMSSG,225)LWD(1,N)
  225 FORMAT('ERROR - UNDEFINED SYMBOL = ',A4)
C   
  300 CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
      KVALUE=0
      RETURN
      END
