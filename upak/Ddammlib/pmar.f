C$PROG PMAR      - Routine to compliment (1toggle) peak-markers
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE PMAR(IDW,IP)
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
C   
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM07/ WINL(4),KSCAL,LASIDW
C
      INTEGER*4 LAT(500,15),RED,BLUE
      character*4 cred, cblue
      equivalence (cred,red), (cblue,blue)
C   
      EQUIVALENCE (LAT(1,1),PAT(1,1))
C   
      DATA cRED,cBLUE/'ORED','OBLU'/
C
      SAVE
C     ==================================================================
C   
      X=PAT(IP,1)
C   
      IF(X.LT.WINL(1).OR.X.GT.WINL(2)) RETURN
C
      MAXY=WINDAT(4,1)-16
C   
      JYB=MAXY
      INC=MAXY/5
      DO 220 I=1,5
      JYA=JYB
      JYB=JYA-INC
      KOLR=RED
      JDX=I+5
      IF(I.NE.5.AND.LAT(IP,JDX).NE.0) KOLR=BLUE
      IF(I.EQ.5.AND.LAT(IP,JDX).EQ.0) KOLR=BLUE
C   
      CALL MARKITS(IDW,KOLR,X,JYA,JYB)
  220 CONTINUE
      RETURN
      END
