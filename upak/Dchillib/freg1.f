C$PROG FREG1
      SUBROUTINE FREG1(IXI,IY,NXY,LXD,LXG,LXR,LYD,LYG,LOC)
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      INTEGER*4 IXI(64),IX(64),IY(64)
C
      INTEGER*2 MIL(65536)
C
      EQUIVALENCE (MIL(1),MILF(1))
C
      SAVE
C
C     **************************************************************
C     NBSY    = # OF BITS TO SHIFT Y-COOR TO MATCH DATA
C     NBSX    = # OF BITS TO SHIFT X-COOR TO MATCH LXR
C
C     IXI(I)  = X-VALUE OF ITH X,Y-POINT IN CXY-LIST AS READ IN
C     IX(I)   = X-VALUE OF ITH X,Y-POINT IN CXY-LIST AFTER SHIFT
C     IY(I)   = Y-VALUE OF ITH X,Y-POINT IN CXY-LIST AS READ IN
C     NXY     = #OF ENTRIES IN X,Y-LIST
C
C     LXD     = X-PARM RANGE IN DATA LIST
C     LXG     = X-PARM RANGE IN  X,Y-LIST
C     LXR     = X-PARM RANGE REQUESTED BY BAN-STATEMENT
C     LYD     = Y-PARM RANGE IN DATA LIST
C     LYG     = Y-PARM RANGE IN  X,Y-LIST
C
C     LOC     = FIRST   HALF-WD IN MIL TO LOAD
C     MXNDXH  = HIGHEST HALF-WD IN MIL TO STORE HI-LIMIT
C     MXNDXL  = HIGHEST HALF-WD IN MIL TO STORE LO-LIMIT
C
C     **************************************************************
C     PROCESS X,Y-LIST (IX,IY) INTO MAP-REGION OF "MIL"
C     **************************************************************
C
      IERR=0
      MXNDXH=LOC+2*LXR-2                !MIN MIL-LOC TO SET
      MXNDXL=LOC+2*LXR-1                !MAX MIL-LOC TO SET
C
      NBSY=LOGRAT2(LYD,LYG)             !# BITS TO SHIFT Y-COOR
      NBSX=LOGRAT2(LXR,LXG)             !# BITS TO SHIFT X-COOR
C
      DO 100 I=1,NXY
      IX(I)=ISHFT(IXI(I),NBSX)          !SHIFT X-COOR TO MATCH LXR
  100 CONTINUE
C
      IX(NXY+1)=IX(1)                   !CLOSE THE BANANA
      IY(NXY+1)=IY(1)                   !CLOSE THE BANANA
C
C     SET LIMIT ARRAYS TO "IMPOSSIBLE"  ****************************
C
      IDX=LOC-1
      DO 150 I=1,LXR
      IDX=IDX+1
      MIL(IDX)=0
      IDX=IDX+1
      MIL(IDX)=1
  150 CONTINUE
C
C     **************************************************************
C     SET LIMITS IMPLIED BY X,Y-LIST
C     **************************************************************
C
      DO 200 I=1,NXY
      IF(IX(I).EQ.IX(I+1)) GO TO 200
      IF(IX(I).GT.IX(I+1)) GO TO 170
C
C     SET UP FOR POSITIVE-GOING DELTA-X  ***************************
C
      JLO=IX(I)
      JHI=IX(I+1)
      S=FLOAT(IY(I+1)-IY(I))/FLOAT(IX(I+1)-IX(I))
      Y0=IY(I)
      JDO=JHI-JLO+1
      X=0.0
      NDX=LOC+2*JLO-2
      DO 160 J=1,JDO
      NDX=NDX+2
      IF(NDX.GT.MXNDXH) NDX=MXNDXH
      LIMHI=Y0+S*X+0.5
      LIMHI=ISHFT(LIMHI+1,NBSY)-1
      IF(LIMHI.GT.LYD-1) LIMHI=LYD-1
      MIL(NDX)=LIMHI
      X=X+1.0
  160 CONTINUE
      GO TO 200
C
C     SET UP FOR NEGATIVE-GOING DELTA-X  ***************************
C
  170 JLO=IX(I+1)
      JHI=IX(I)
      S=FLOAT(IY(I)-IY(I+1))/FLOAT(IX(I)-IX(I+1))
      Y0=IY(I+1)
      JDO=JHI-JLO+1
      X=0.0
      NDX=LOC+2*JLO-1
      DO 180 J=1,JDO
      NDX=NDX+2
      IF(NDX.GT.MXNDXL) NDX=MXNDXL
      LIMLO=Y0+S*X+0.5
      LIMLO=ISHFT(LIMLO,NBSY)
      MIL(NDX)=LIMLO
      X=X+1.0
  180 CONTINUE
  200 CONTINUE
      RETURN
      END
