C$PROG GATMSK
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE GATMSK(NAM,IDX,PWN,MSK)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXG=100)
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      INTEGER*4 NAM(3),NAME(4)
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
      DO 10 I=1,3
      NAME(I)=NAM(I)
   10 CONTINUE
      NAME(4)=IDX
C
      CALL NAMLOC(GNAM,NAME,NGAT,NDX)
C
      IF(NDX.LE.0) GO TO 100
C
      PWN=PATN(NDX)
      MSK=GMSK(NDX)
      RETURN
C
  100 PWN=1
      MSK=0
C
      WRITE(CMSSG,105)
  105 FORMAT('GATE-NAME NOT FOUND = ',3A4,I6)
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
