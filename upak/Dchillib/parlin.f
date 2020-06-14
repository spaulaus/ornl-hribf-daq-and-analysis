C$PROG PARLIN
      SUBROUTINE PARLIN(IWD,IERR,MSER)
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      INTEGER*4 IWD(1),LWD(2,40),ITYP(40)
C
      INTEGER*4 MSG(10,3),MSER(10)
      CHARACTER*40 MSC(3)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/
     1'SYNTAX ERROR OR ILLEGAL VALUE           ',
     2'ILLEGAL ENTRY IN PARM-LIST              ',
     3'ILLEGAL VALUE OF PARM LENGTH            '/
C
      SAVE
C
C
C     *************************************************************
C     PROCESS - $LPR P-LIST = LENG (SET PARM LENGTH FOR P-LIST)
C     *************************************************************
C
      IB=IFIND(IWD,z'3D',5,80)              !LOOK FOR THE "= SIGN"
      IF(IB.LE.0) GO TO 110                 !ERROR IF NOT FOUND
      IB=IB-1                               !ADJUST SCAN PNTR
      CALL REFOR(IWD,LWD,ITYP,NF,5,IB,NTER) !REFORMAT LIST P-LIST
      IF(NTER.NE.0) GO TO 110               !TST FOR ERROR
      CALL PLIST(LWD,ITYP,NF,LIST,NL,IERR,MSER) !CONSTRUCT LIST
      IF(IERR.NE.0) RETURN                  !TST FOR ERROR
C
      DO 10 I=1,NL                          !LOOP TO TST VALUES
      IF(LIST(I).LT.1)    GO TO 120         !TST VS LO-LIMIT
      IF(LIST(I).GT.NPAR) GO TO 120         !TST VS HI-LIMIT
   10 CONTINUE
C
      IB=IB+1                               !ADJUST SCAN PNTR
      CALL REFOR(IWD,LWD,ITYP,NF,IB,80,NTER)!REFORMAT LENGTH PART
      IF(NTER.NE.0)     GO TO 110           !TST FOR ERROR
      CALL LIMIV(LWD,1,32768,LENG,IERR)     !GET LENGTH
      IF(IERR.NE.0)     GO TO 130           !TST FOR ERROR
      IT=LOGB2(LENG)                        !TST FOR PWR OF 2
      IF(2**IT.NE.LENG) GO TO 130           !TST FOR PWR OF 2
C
      DO 20 I=1,NL                          !LOOP TO SET LPAR
      NDX=LIST(I)                           !GET PARM#
      LPAR(NDX)=LENG                        !SET LENGTH
   20 CONTINUE
C
      DO 30 I=1,NPAR                        !TST FOR ALL LPAR'S SET
      IF(LPAR(I).LE.0) GO TO 40             !TST FOR ZERO
   30 CONTINUE
      LPARF=1                               !SAYS ALL ARE SET
      RETURN
C
   40 LPARF=0                               !SAYS NOT ALL SET
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP ERROR MESSAGE
C     **************************************************************
C
  110 JJ=1
      GO TO 200
  120 JJ=2
      GO TO 200
  130 JJ=3
C
  200 DO 210 I=1,10
      MSER(I)=MSG(I,JJ)
  210 CONTINUE
      IERR=JJ
      RETURN
      END
