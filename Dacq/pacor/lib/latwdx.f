C$PROG LATWDX
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      FUNCTION LATWDX(NAME,INDX)
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
      CHARACTER*4  KIMO
C
      SAVE
C
C     ************************************************************
C     RETURNS LATCH-WORD-INDEX CORRESPONDING TO NAME(INDX)
C     ************************************************************
C
      LATN=0
      DO 20 K=1,NUMT                      !LOOP ON FULL TABLE
      IF(KIMO(K).NE.'$LAT')    GO TO 20   !LOOK FOR $LAT TYPE
      LATN=LATN+1
      DO 10 I=1,3                         !CHECK FOR NAME  MATCH
      IF(NAME(I).NE.NAMO(I,K)) GO TO 20
   10 CONTINUE
      IF(INDX.NE.NAMO(4,K))    GO TO 20   !CHECK FOR INDEX MATCH
      GO TO 50
   20 CONTINUE
      GO TO 100
C
   50 LATWDX=LATN                         !LATCH-WORD INDEX 
      RETURN
C
  100 WRITE(CMSSG,105)NAME,INDX
  105 FORMAT('UNDEFINED LATCH-WORD-NAME,INDEX = ',3A4,I6)
      CALL ERRLOG(LOGUT,LOGUP)
      LATWDX=1  
      RETURN
      END
