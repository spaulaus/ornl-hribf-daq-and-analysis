C$PROG XIAASS
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/24/2000
C     ******************************************************************
C
      SUBROUTINE XIAASS(IWD)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
      DATA         NXIA,MXXIA/0,64/
C     ------------------------------------------------------------------
C
      INTEGER*4    IWD(20)
C
      INTEGER*4    JWD(20),LWD(2,40),ITYP(40),NF,NTER,XIANDX
C
      INTEGER*4    NDX(4),IVAL(4),CRAT,SLOT,VSN,GRP
C
      CHARACTER*1  CLWD(8,4),CODE(4)
C
      EQUIVALENCE (CLWD,LWD)
C
      EQUIVALENCE (CRAT,IVAL(1)),
     &            (SLOT,IVAL(2)),
     &            (VSN, IVAL(3)),
     &            (GRP, IVAL(4))
C
      INTEGER*4    BLANK
      character*4  cBLANK
      equivalence  (cBLANK, BLANK)
      DATA         cBLANK/'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
C
      IF(NTER.NE.0) GO TO 500
C
      IF(NF.LT.3)   GO TO 500
C
      IF(NF.GT.4)   GO TO 500
C
      DO 20 I=1,NF
      CODE(I)=CLWD(1,I)
      NDX(I)=XIANDX(CODE(I))
      IF(NDX(I).EQ.0) GO TO 500
   20 CONTINUE
C
      DO 30 I=1,20
      JWD(I)=BLANK
   30 CONTINUE
C
      DO 40 I=1,NF
      CLWD(1,I)=' '
   40 CONTINUE
C
      N=0
      DO 60 J=1,NF
      DO 50 I=1,2
      N=N+1
      JWD(N)=LWD(I,J)
   50 CONTINUE
   60 CONTINUE
C
      CALL GREAD(JWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(NTER.NE.0) GO TO 500
C
      DO 70 I=1,4
      IVAL(I)=0
   70 CONTINUE
C
      DO 80 I=1,NF
      CALL MILV(LWD(1,I),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 500
      IVAL(NDX(I))=IV
   80 CONTINUE
C
      IF(CRAT.LT.0.OR.CRAT.GT.7)   GO TO 510
      IF(SLOT.LT.1.OR.SLOT.GT.31)  GO TO 520
      IF(VSN .LT.0.OR.VSN .GT.255) GO TO 530
      IF(GRP .LT.0.OR.GRP .GT.255) GO TO 540
C
      CALL XIACHEK(IVAL,IERR)
C
      NXIA=NXIA+1
C
      IF(NXIA.GT.MXXIA) GO TO 550
C
      XIAC(NXIA)  =IVAL(1)
      XIAN(NXIA)  =IVAL(2)
      XIAVSN(NXIA)=IVAL(3)
      XIAGRP(NXIA)=IVAL(4)
C
C
      RETURN
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error or illegal command')
C
      GO TO 1000
C
  510 WRITE(CMSSG,515)CRAT
  515 FORMAT('Illegal CRATE number =',I8)
      GO TO 1000
C
  520 WRITE(CMSSG,525)SLOT
  525 FORMAT('Illegal SLOT number =',I8)
      GO TO 1000
C
  530 WRITE(CMSSG,535)VSN
  535 FORMAT('Illegal Virtual Station# =',I8,' - legal# is 0-255')
      GO TO 1000
C
  540 WRITE(CMSSG,545)GRP
  545 FORMAT('Illegal GROUP number =',I8,' - legal# is 0-255')
      GO TO 1000
C
  550 WRITE(CMSSG,555)NXIA
  555 FORMAT('/PACX/ overflow at NXIA =',I8)
      NXIA=MXXIA
      GO TO 1000
C
 1000 CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
