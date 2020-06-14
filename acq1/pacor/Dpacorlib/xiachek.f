C$PROG XIACHEK
C
      SUBROUTINE XIACHEK(IVAL,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
C     ------------------------------------------------------------------
C
      INTEGER*4    IVAL(4)
C
      SAVE
C
      IERR=0
C
      CTST=IVAL(1)
      NTST=IVAL(2)
      VTST=IVAL(3)
C
      DO 20 N=1,NUMT
      IF(MOTY(1,N).EQ.28) GO TO 20
      IF(CTST.NE.CRAT(N)) GO TO 20
      IF(NTST.NE.SLOT(N)) GO TO 20
      WRITE(CMSSG,10)CTST,NTST
   10 FORMAT('MULTIPLE DEFINITION FOR: TYPE, C,N = ',2I8)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
   20 CONTINUE
C
      DO 50 N=1,NXIA
      IF(CTST.NE.XIAC(N)) GO TO 50
      IF(NTST.NE.XIAN(N)) GO TO 50
      WRITE(CMSSG,40)CTST,NTST
   40 FORMAT('MULTIPLE DEFINITION FOR: TYPE, C,N = ',2I8)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
   50 CONTINUE
C
      DO 80 N=1,NXIA
      IF(VTST.NE.XIAVSN(N))GO TO 80
      WRITE(CMSSG,70)VTST
   70 FORMAT('MULTIPLE DEFINITION FOR: TYPE, VSN = ',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
   80 CONTINUE
C
      RETURN
C
      END
