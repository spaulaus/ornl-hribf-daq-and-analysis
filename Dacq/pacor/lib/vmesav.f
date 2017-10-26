C$PROG VMESAV
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/22/2000
C     ************************************************************
C
      SUBROUTINE VMESAV(MODNAM,MODKOD)
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
      INTEGER*4    MODNAM(3),MODKOD
C
      INTEGER*4    MODLIS(100),NMOD
C
      DATA                   NMOD/0/
C
      CHARACTER*4  KIMO,USED
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 I=1,NMOD
      IF(MODLIS(I).EQ.MODKOD) RETURN
   10 CONTINUE
C
      NMOD=NMOD+1
C
      MODLIS(NMOD)=MODKOD
C
      N=NUMT+1
C
      IF(N.GT.TMX) THEN
      WRITE(CMSSG,20)N
   20 FORMAT('/PAC2/ ARRAY OVERFLOW AT N =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      N=TMX
      ENDIF
C
      NAMO(1,N)=MODNAM(1)
      NAMO(2,N)=MODNAM(2)
      NAMO(3,N)=MODNAM(3)
C
      KIMO(N)='$VME'
C
      MOTY(1,N)=MODKOD
C
      USED(N)='NO  '
C
      NUMT=N
C
      RETURN
C
      END
