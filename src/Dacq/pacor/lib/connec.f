C$PROG CONNEC
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CONNEC(NAME,IDX,JC,JN,JA,JF,JI)
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
      INTEGER*4 NAME(3)
C
      CHARACTER*4  KIMO,USED
C
      SAVE
C
C     ************************************************************
C     MAKES THE CONNECTION BETWEEN MODULE NAME(INDX) & C,N,A,F
C     ************************************************************
C
      DO 20 J=1,NUMT
      DO 10 I=1,3
      IF(NAME(I).NE.NAMO(I,J))                    GO TO 20
   10 CONTINUE
      IF(IDX.NE.NAMO(4,J))                        GO TO 20
      IF(KIMO(J).NE.'$CAM'.AND.KIMO(J).NE.'$LAT') GO TO 20
      GO TO 50
   20 CONTINUE
      GO TO 100
C
   50 JC=CRAT(J)
      JN=SLOT(J)
      JA=SUBA(J)
      JF=FRED(J)
      JI=IDNM(J)
      USED(J)='YES '
      RETURN
C
  100 WRITE(CMSSG,105)NAME,IDX
  105 FORMAT('MODULE NAME UNDEFINED = ',3A4,'(',I4,')')
      CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
