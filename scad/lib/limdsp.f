C$PROG LIMDSP    - Displays rate limits
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE LIMDSP(MODE)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
C     ------------------------------------------------------------------
      COMMON/SD14/ MXBEEP,MXGOOD
      INTEGER*4    MXBEEP,MXGOOD
C     ------------------------------------------------------------------
      COMMON/SD15/ LUAL,THUSH,TNOW,ALFLG
      INTEGER*4    LUAL,THUSH,TNOW
      CHARACTER*4                  ALFLG
C     ------------------------------------------------------------------
      CHARACTER*4  MODE,FLAG,STAT
C
      CHARACTER*4  FLGX(10)
C
      INTEGER*4    AVD(3,10),ALO(3,10),AHI(3,10)
C
      INTEGER*4    BELL,BEEP,TOGL,NDX,N,I
C
      INTEGER*4    NGOOD,NBEEP,BELLV
C
      REAL*4       VDF(512)
      EQUIVALENCE (VDF,VD)
C
      DATA         BELL,BELLV,BEEP/'07070707'X,'07070707'X,0/
C
      DATA         STAT/'GOOD'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      FLAG='   '
C
      IF(MODE.EQ.'INIT') GO TO 100
      IF(MODE.EQ.'CHEK') GO TO 200
      RETURN
C
C     ------------------------------------------------------------------
C     Executes this part subsequent to scaler read
C     ------------------------------------------------------------------
C
  100 ALFLG='OFF '
C
      IF(NLIM.LE.0) RETURN
C
      STAT='GOOD'                            !Init status to GOOD
C
      CALL SENDBUF(6,HOMLOC,8)
C
      WRITE(6,105)
  105 FORMAT(5('******'),'*')
  106 FORMAT(5('------'),'-')
C
      DO 120 N=1,NLIM
      NDX=SCNDX(N)
      FLAG='    '
      FLGX(N)='    '
C
      IF(VDF(NDX).LT.LIMLO(N)) FLGX(N)='LO  '
      IF(VDF(NDX).GT.LIMHI(N)) FLGX(N)='HI  '
C
      CALL FLO6(VDF(NDX),AVD(1,N))
      CALL FLO6(LIMLO(N),ALO(1,N))
      CALL FLO6(LIMHI(N),AHI(1,N))
C
      WRITE(6,110)(LA(I,NDX),I=1,3),
     &             (AVD(I,N),I=1,2),
     &             (ALO(I,N),I=1,2),
     &             (AHI(I,N),I=1,2),
     &              FLGX(N)
C
  110 FORMAT(2A4,A3,3(A4,A2),1X,A1)
C
      IF(FLGX(N).NE.'    ') STAT='BAD '      !Tst for any out of limits
C
  120 CONTINUE
      WRITE(6,105)
      TOGL=1
C
      IF(STAT.EQ.'BAD ') NGOOD=0             !Reset  GOOD cntr ?
C
      IF(STAT.EQ.'BAD ') ALFLG='ON  '        !Set Alarm flag ?
C
      RETURN
C
C     ------------------------------------------------------------------
C     Executes this part for display (blink/beep) only
C     ------------------------------------------------------------------
C
  200 IF(NLIM.LE.0) RETURN
C
      IF(STAT.NE.'GOOD') GO TO 205           !Tsfr blink/beep needed
C
      NGOOD=NGOOD+1                          !Otherwise, inc GOOD cntr?
      IF(NGOOD.GE.MXGOOD) THEN               !Tsfr beep enable
      BELLV=BELL
      NBEEP=0
      ENDIF
      RETURN
C
C
  205 CALL SENDBUF(6,HOMLOC,8)
C
      TOGL=3-TOGL
C
      IF(TOGL.EQ.2) GO TO 300
C
C     ------------------------------------------------------------------
C     This part writes the data and limit-flag
C     ------------------------------------------------------------------
C
      WRITE(6,105)
      DO 220 N=1,NLIM
      NDX=SCNDX(N)
      BEEP=BELLV
C
      WRITE(6,210)(LA(I,NDX),I=1,3),
     &             (AVD(I,N),I=1,2),
     &             (ALO(I,N),I=1,2),
     &             (AHI(I,N),I=1,2),
     &              FLGX(N),BEEP
C
  210 FORMAT(2A4,A3,3(A4,A2),1X,A1,A1)
  220 CONTINUE
C
      WRITE(6,106)
C
      IF(BEEP.EQ.BELL) THEN                  !Tst for beeping
      NBEEP=NBEEP+1                          !If yes, inc beep cntr
      NGOOD=0                                !and reset GOOD cntr
      ENDIF
      IF(NBEEP.GE.MXBEEP) BELLV=0            !Tsfr max & disable?
C
      RETURN
C
C     ------------------------------------------------------------------
C     This part writes the data and a blank limit-flag
C     ------------------------------------------------------------------
C
  300 WRITE(6,106)
      DO 320 N=1,NLIM
      FLAG='    '
      NDX=SCNDX(N)
      BEEP=BELLV
C
      WRITE(6,210)(LA(I,NDX),I=1,3),
     &             (AVD(I,N),I=1,2),
     &             (ALO(I,N),I=1,2),
     &             (AHI(I,N),I=1,2),
     &              FLAG,BEEP
  320 CONTINUE
C
      IF(BEEP.EQ.BELL) THEN                  !Tst for beeping
      NBEEP=NBEEP+1                          !If yes, inc beep cntr
      NGOOD=0                                !and reset GOOD cntr
      ENDIF
      IF(NBEEP.GE.MXBEEP) BELLV=0            !Tsfr max & disable?
C
      WRITE(6,105)
      RETURN
      END
