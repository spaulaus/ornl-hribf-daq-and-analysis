C$PROG PJUST2    - Computes pix/chan X & Y from #chans & space available
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PJUST2(KWN,NCHX,NCHY,KRX,KRY,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/PL05/ NFX,NFY,PPCX,PPCY,BPPCX,BPPCY,KRUNX,KRUNY
C     ------------------------------------------------------------------
      COMMON/PL06/ NPXX,NPXY,NUPX,NUPY
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     COMPUTES # PIXELS/CHANNEL (X & Y) FROM # CHANS TO DISPLAY
C                                       AND THE SPACE AVAILABLE
C     KWN   = WINDOW NUMBER FOR DISPLAY
C     NCHX  = # OF X-CHANNELS TO BE DISPLAYED
C     NCHY  = # OF Y-CHANNELS TO BE DISPLAYED
C     KRX   = USER REQUESTED X-CRUNCH FACTOR (0 SAME AS 1)
C     KRY   = USER SPECIFIED Y-CRUNCH FACTOR (0 SAME AS 1)
C
C     NFX   = # OF X-CHANS TO BE DISPLAYED (AFTER ANY CRUNCH)
C     NFY   = # OF Y-CHANS TO BE DISPLAYED (AFTER ANY CRUNCH)
C     PPCX  = # OF X-PIXELS/CHAN TO LIGHT (SET)
C     PPCY  = # OF Y-PIXELS/CHAN TO LIGHT (SET)
C     BPPCX = # OF X-PIXELS/CHAN TO LEAVE BLANK (SET TO 0)
C     BPPCY = # OF Y-PIXELS/CHAN TO LEAVE BLANK (SET TO 0)
C     KRUNX = FINAL X-CRUNCH (SPECIFIED OR REQUIRED)
C     KRUNY = FINAL Y-CRUNCH (SPECIFIED OR REQUIRED)
C     ------------------------------------------------------------------
C
      IERR=0                            !RESET ERROR FLAG
      IF(WINFLG(1,KWN).EQ.0) THEN       !TST FOR WINDOW OPEN
                             IERR=1     !IF NOT, SET ERROR FLAG
                             RETURN     !AND RETURN
                             ENDIF
C
      PXOF=50                           !NORMAL X-AXIS OFFSET
      PYOF=15                           !NORMAL Y-AXIS OFFSET
C
      IF(WINFLC(2,KWN).EQ.'OFF ') THEN  !TST FOR AXIS TURNED OFF
      PXOF=0
      PYOF=0
      ENDIF
C
      NPXX=WINDAT(3,KWN)-PXOF+0.5  !# X-PIXELS AVAILABLE
      NPXY=WINDAT(4,KWN)-PYOF+0.5  !# Y-PIXELS AVAILABLE
C
      KRUNX=KRX                    !SET REQUESTED X-CRUNCH
      KRUNY=KRY                    !SET REQUESTED Y-CRUNCH
      IF(KRUNX.LT.1) KRUNX=1       !IF 0, SET TO 1
      IF(KRUNY.LT.1) KRUNY=1       !IF 0, SET TO 1
C
      NX=(NCHX+KRUNX-1)/KRUNX      !# X-CHANS REQUIRED POST-CRUNCH
      NY=(NCHY+KRUNY-1)/KRUNY      !# Y-CHANS REQUIRED POST-CRUNCH
C
   10 IF(NX.LE.NPXX)  GO TO 20     !IS REQUIRED X-PIXELS AVAILABLE
      KRUNX=KRUNX+1                !INC CRUNCH IF TOO FEW X-PIXELS
      NX=(NCHX+KRUNX-1)/KRUNX      !# X-CHANS REQUIRED NOW
      GO TO 10                     !GO BACK & TST  FOR POSSIBLE
C
   20 IF(NY.LE.NPXY)  GO TO 30     !IS REQUIRED Y-PIXELS AVAILABLE
      KRUNY=KRUNY+1                !INC CRUNCH IF TOO FEW Y-PIXELS
      NY=(NCHY+KRUNY-1)/KRUNY      !# Y-CHANS REQUIRED NOW
      GO TO 20                     !GO BACK & TST FOR POSSIBLE
C
   30 PPCX=1                       !INIT LIT   X-PIXELS/CHAN
      PPCY=1                       !INIT LIT   Y-PIXELS/CHAN
      BPPCX=0                      !INIT BLANK X-PIXELS/CHAN
      BPPCY=0                      !INIT BLANK Y-PIXELS/CHAN
C
   50 NUPX=NX*(PPCX+BPPCX)         !# OF X-PIXELS     USED
      NLEFT=NPXX-NUPX              !# OF X-PIXELS NOT USED (LEFT)
      IF(NLEFT.LT.NX) GO TO 60     !IS LARGER CHAN-ZIZE POSSIBLE  
C
      IF(PPCX.LT.3)   GO TO 55     !IF YES, INC BPPCX OR PPCX
      IF(BPPCX.GT.0)  GO TO 55
      BPPCX=BPPCX+1                !INC BLANK X-PIXELS/CHAN
      GO TO 50                     !AND TST AGAIN
C
   55 PPCX=PPCX+1                  !INC LIT   X-PIXELS/CHAN
      GO TO 50                     !AND TST AGAIN
C
   60 NUPY=NY*(PPCY+BPPCY)         !# OF Y-PIXELS     USED
      NLEFT=NPXY-NUPY              !# OF Y-PIXELS NOT USED (LEFT)
      IF(NLEFT.LT.NY) GO TO 70     !IS LARGER CHAN-SIZE POSSIBLE
C
      IF(PPCY.LT.3)   GO TO 65     !IF YES, INC BPPCY OR PPCY
      IF(BPPCY.GT.0)  GO TO 65
      BPPCY=BPPCY+1                !INC BLANK Y-PIXELS/CHAN
      GO TO 60                     !AND TST AGAIN
C
   65 PPCY=PPCY+1                  !INC LIT   Y-PIXELS/CHAN
      GO TO 60                     !ANT TST AGAIN
C
   70 NFX=NX
      NFY=NY
      RETURN
      END
