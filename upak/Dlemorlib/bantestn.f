C$PROG BANTESTN  - Tests a free-form-gate by gate sequence number
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 07/20/2004
C     ******************************************************************
C
      LOGICAL FUNCTION BANTESTN(NG,IX,IY)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    NFFG,NNDX,MAXID
C     ------------------------------------------------------------------
      PARAMETER   (NFFG=3000)   !NUMBER OF FFG (MAX).
C
      PARAMETER   (NNDX=1024000)
C
      PARAMETER   (MAXID=8000)  !ID range is 1 to MAXID
C     ------------------------------------------------------------------
      COMMON/FFGA/ LFGL(NNDX),LFGH(NNDX),NFG
      INTEGER*2    LFGL,      LFGH
      INTEGER*4    NFG
C     ------------------------------------------------------------------
      COMMON/FFGB/ IPX(NFFG),IPY(NFFG),NBSX(NFFG),NBSY(NFFG),LFOF(NFFG)
      INTEGER*4    IPX,      IPY,      NBSX,      NBSY,      LFOF
C     ------------------------------------------------------------------
      COMMON/FFGC/ NUPM(NFFG),IAUX(NFFG),JAUX(NFFG)
      INTEGER*4    NUPM,      IAUX,      JAUX
C     ------------------------------------------------------------------
      COMMON/FFGD/ IDBAN(MAXID),IDIRBAN(NFFG)
      INTEGER*4    IDBAN,       IDIRBAN
C     ------------------------------------------------------------------
      COMMON/FFGE/ LXD(NFFG),LXG(NFFG),LYD(NFFG),LYG(NFFG)
      INTEGER*4    LXD,      LXG,      LYD,      LYG
C     ------------------------------------------------------------------
C
C     EXTERNAL     ISHFT
C
      INTEGER*4    NG,IX,IY,IDX,ISHFT
C
      INTEGER*2    JY
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      BANTESTN=.FALSE.
C
      IF(NG.GT.NFG)      RETURN  !Test for Gate#  out-of-range
C
      IF(NG.LT.1)        RETURN  !Test for Gate#  out-of-range
C
      JY=IY
C
      IDX=ISHFT(IX,NBSX(NG))     !Get X-index in gate array
C
      IF(IDX.LE.0)       RETURN  !Test for X-index out-of-range
C
      IF(IDX.GT.LXG(NG)) RETURN  !Test for X-index out-of-range
C
      IDX=IDX+LFOF(NG)           !Get index in multiple banana array
C
      IF(IDX.GT.NNDX)    RETURN  !Test for full index out-of-range
C
      IF(JY.GE.LFGL(IDX).AND.JY.LE.LFGH(IDX)) BANTESTN=.TRUE.
C
      RETURN
C
      END
