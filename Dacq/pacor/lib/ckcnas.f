C$PROG CKCNAS
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKCNAS(NDX)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      CHARACTER*4  KIMO
C
      INTEGER*4    FLO
C
      SAVE
C
C     ************************************************************
C     CHECK CAMAC, FASTBUS & FERA PARAMETERS FOR LEGAL IN CONTEXT
C     ************************************************************
C
      IF(KIMO(NDX).EQ.'$LAT') GO TO 100
      IF(KIMO(NDX).EQ.'$CAM') GO TO 100
      IF(KIMO(NDX).EQ.'$FER') GO TO 200
      IF(KIMO(NDX).EQ.'$FAS') GO TO 300
C
      RETURN
C
  100 FLO=0
C
      IF(MOTY(1,NDX).GT.0) THEN  !If Module-type is specified
      FLO=-1                     !Allow   F-read to be undefined
      FRED(NDX)=-1               !and set F-read to be undefined
      ENDIF
C     
      CALL CKCAM(CRAT(NDX),SLOT(NDX),SUBA(NDX),FRED(NDX),FLO,7,ERR)
C
      IF(FCLR(NDX).LT.0) RETURN
C
      CALL CKCAM(CRAT(NDX),SLOT(NDX),ACLR(NDX),FCLR(NDX),8,31,ERR)
C
      RETURN
C
  200 CALL CKFER(CRAT(NDX),SLOT(NDX),SUBA(NDX),ERR)
      RETURN
C
  300 CALL CKFAS(CRAT(NDX),SLOT(NDX),SUBA(NDX),ERR)
      RETURN
C
      END
