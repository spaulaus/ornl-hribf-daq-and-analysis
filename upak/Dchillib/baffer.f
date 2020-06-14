C$PROG BAFFER
      SUBROUTINE BAFFER(IERR,MSER)
C
      INTEGER*2 BNID,BLEN
C
      INTEGER*4 BLOC
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
C
      COMMON/BBB/ BNID(880),BLEN(880),BLOC(880),MXBAN,NBAN
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      INTEGER*2 MILH(524288)
C
      INTEGER*4 MESS(10,6),MSER(10)
      CHARACTER*40 MSC(6)
C
      EQUIVALENCE (MSC(1),MESS(1,1))
      EQUIVALENCE (MILH(1),MILF(1))
C
      DATA  MSC/'SYNTAX ERROR                           ',
     2          'SYNTAX ERROR OR ILLEGAL VALUE          ',
     3          'BAN-LENGTH NOT A PWR-OF-2              ',
     4          'MULTIPLE DEFINITION OF BAN-ID          ',
     5          'TOO MANY BANANAS SPECIFIED - MAX# = 880',
     6          'INSUFFICIENT SPACE TO STORE BANANA/S   '/
C
      SAVE
C
C     **************************************************************
C     PROCESS - $BAF (LENG) ID1,ID2,ID3 ......
C     **************************************************************
C
      IERR=0                                !RESET ERROR FLAG
C                                           !
      CALL REFOR(IWD,LWD,ITYP,NF,5,80,NTER) !RE-FORMAT LINE
      IF(NTER.NE.0) GO TO 510               !TST FOR ERROR
      IF(NF.LT.2)   GO TO 510               !REQUIRE 2 FIELDS MIN
      CALL LIMIV(LWD(1,1),2,8192,LEN,IERR)  !GET LENGTH
      IF(IERR.NE.0) GO TO 520               !TST FOR ERROR
      IT=LOGB2(LEN)                         !REQUIRE LENGTH TO BE
      IF(2**IT.NE.LEN) GO TO 530            !PWR-OF-2
C                                           !
      DO 100 N=2,NF                         !LOOP ON BAN-ID FIELDS
      CALL LIMIV(LWD(1,N),1,32767,IBN,IERR) !GET BAN-ID
      IF(IERR.NE.0) GO TO 520               !TST FOR ERROR
C                                           !
      DO 20 I=1,NBAN                        !LOOP ON CURRENT BAN-IDS
      IF(IBN.EQ.BNID(I)) GO TO 540          !ERROR IF ALREADY EXISTS
   20 CONTINUE                              !
      IF(NBAN.GE.MXBAN) GO TO 550           !TST FOR BAN-LIST OVFLO
C                                           !
      NBAN=NBAN+1                           !INC # BAN'S
      BNID(NBAN)=IBN                        !STORE BAN-ID
      BLEN(NBAN)=LEN                        !STORE BAN-LENGTH
C                                           !
      IF(NMAPL+2*LEN.GT.MXMAPL) GO TO 560   !TST FOR MAP SPACE
C                                           !
      NDX=IMAPOF+NMAPL                      !INIT MIL-INDEX
      BLOC(NBAN)=NDX+1                      !STORE BAN "LOCATION"
      DO 40 I=1,LEN                         !LOOP ON # CHANNELS
      NDX=NDX+1                             !INC MIL-INDEX
      MILH(NDX)=0                           !STORE HI-LIMIT
      NDX=NDX+1                             !INC MIL-INDEX
      MILH(NDX)=1                           !STORE LO-LIMIT
   40 CONTINUE                              !
      NMAPL=NMAPL+2*LEN                     !NEW MAP-LENGTH
  100 CONTINUE
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP MESSAGE
C     **************************************************************
C
  510 JJ=1
      GO TO 600
  520 JJ=2
      GO TO 600
  530 JJ=3
      GO TO 600
  540 JJ=4
      GO TO 600
  550 JJ=5
      GO TO 600
  560 JJ=6
C
  600 IERR=JJ
      DO 610 I=1,10
      MSER(I)=MESS(I,JJ)
  610 CONTINUE
      RETURN
      END
