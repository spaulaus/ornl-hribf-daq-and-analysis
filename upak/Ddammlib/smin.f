C$PROG SMIN      - Gradient search routine for minimizing FOX(X)
C
C     ******************************************************************
C     From Sandia corp. - long ago
C     ******************************************************************
C
      SUBROUTINE SMIN(NV,KK,DEL,DELFAC,A,GUESS,X,FOFX)
C
      DIMENSION XNOW(176),XNEW(176),A(1),X(1),GUESS(1)
C
      SAVE
C
C     ------------------------------------------------------------------
C     NV=NO. OF VARIABLES (VALUES OF X(I))
C     X(I) ARE VARIABLES TO BE ADJUSTED
C     A(I) IS LOWER LIMIT FOR X(I)
C     A(I+NV) IS UPPER LIMIT FOR X(I)
C     GUESS(I) IS STARTING VALUE OF X(I)
C     DEL IS THE STARTING STEP INTERVAL (WORKS BEST IF GUESS(I)=1.0)
C     KK IS THE NO. OF TIMES DEL IS TO BE MULTIPLIED BY DELFAC
C     FOFX IS THE VALUE OF THE FUNCTION AT MIN
C     ------------------------------------------------------------------
C   
      NK=KK
      NX=NV
      DO 5 I=1,NX
      XNOW(I)=GUESS(I)
      XNEW(I)=XNOW(I)
    5 CONTINUE
      DELTA=DEL
      IF(DELTA.GT.0.0) GO TO 20
      DO 14 I=1,NX
      NA=NX+I
      T=A(NA)-A(I)
      IF(DELTA.LT.T) DELTA=T
   14 CONTINUE
      DELTA=DELTA*DELFAC
   20 FNOW=FOX(XNOW)
  201 FOLD=FNOW
  200 DO 40 I=1,NX
      XNEW(I)=XNEW(I)+DELTA
      NA=NX+I
      IF(XNEW(I).LE.A(NA)) GO TO 22
      XNEW(I)=A(NA)
   22 FNEW=FOX(XNEW)
      IF(FNEW.GE.FNOW) GO TO 26
   25 FNOW=FNEW
      GO TO 40
   26 XNEW(I)=XNOW(I)-DELTA
      IF(XNEW(I).GE.A(I)) GO TO 30
      XNEW(I)=A(I)
   30 FNEW=FOX(XNEW)
      IF(FNEW.LT.FNOW) GO TO 25
      XNEW(I)=XNOW(I)
   40 CONTINUE
      IF(FNOW.LT.FOLD) GO TO 50
      NK=NK-1
      IF(NK.LE.0) GO TO 46
      DELTA=DELTA*DELFAC
      GO TO 200
   50 DO 60 I=1,NX
      T=XNOW(I)
      XNOW(I)=XNEW(I)
      XNEW(I)=2.0*XNEW(I)-T
      NA=NX+I
      IF(XNEW(I)-A(NA)) 52,60,51
   51 XNEW(I)=A(NA)
      GO TO 60
   52 IF(XNEW(I).GE.A(I)) GO TO 60
      XNEW(I)=A(I)
   60 CONTINUE
      FNEW=FOX(XNEW)
      IF(FNEW.GE.FNOW) GO TO 70
      FNOW=FNEW
      GO TO 50
   70 DO 71 I=1,NX
   71 XNEW(I)=XNOW(I)
      GO TO 201
   46 FOFX=FNOW
      DO 80 I=1,NX
   80 X(I)=XNOW(I)
      RETURN
      END
