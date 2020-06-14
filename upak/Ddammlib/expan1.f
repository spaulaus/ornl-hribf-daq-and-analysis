C$PROG EXPAN1    - Expands 1-D display
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE EXPAN1(IDW)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/PL01/ ILOX(20),IHIX(20),ILOF(20),IHIF(20),      !/PL01
     &             FLOX(20),FHIX(20),FLOF(20),FHIF(20),      !/PL01
     &             GWID,MOPL,ECAL(3)                         !/PL01
C     ------------------------------------------------------------------
C
      CHARACTER*4  KDSP
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(ILOX(IDW).LT.0)         GO TO 530
      IF(IHIX(IDW).LE.ILOX(IDW)) GO TO 530
      ILO=ILOX(IDW)+1
      IHI=IHIX(IDW)+1
      IF(IHI.GT.16384) IHI=16384
      IF(ILO.GE.IHI)   GO TO 540
      NCH=IHI-ILO+1
      IFI=ILO
      KDSP=WINFLC(4,IDW)
C
      CALL PLOTUM1(IDW,IFI,NCH,KDSP)
C
      RETURN
C   
C     ------------------------------------------------------------------
C     RETURN ERROR MESSAGES
C     ------------------------------------------------------------------
C   
  530 WRITE(CMSSG,535)
  535 FORMAT('ILLEGAL OR UNSPECIFIED EXPAND REGION')
      GO TO 600
C
  540 WRITE(CMSSG,545)
  545 FORMAT('DISPLAY RANGE SPECIFIED BY "DL" DOESN,T MATCH DATA')
C   
  600 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
