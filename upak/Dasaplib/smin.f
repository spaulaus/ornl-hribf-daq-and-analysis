C$PROG SMIN
      SUBROUTINE SMIN(NV,KK,DEL,DELFAC,A,GUESS,X,FOFX)
      DIMENSION XNOW(64),XNEW(64),A(*),X(*),GUESS(*)
C
      SAVE
C
C     **************************************************************
C     NV=NO. OF VARIABLES (VALUES OF X(I))
C     X(I) ARE VARIABLES TO BE ADJUSTED
C     A(I) IS LOWER LIMIT FOR X(I)
C     A(I+NV) IS UPPER LIMIT FOR X(I)
C     GUESS(I) IS STARTING VALUE OF X(I)
C     DEL IS THE STARTING STEP INTERVAL (WORKS BEST IF GUESS(I)=1.0)
C     KK IS THE NO. OF TIMES DEL IS TO BE MULTIPLIED BY DELFAC
C     FOFX IS THE VALUE OF THE FUNCTION AT MIN
C     **************************************************************
C
C     **************************************************************
C     SET UP INITIAL VALUES
C     **************************************************************
C
      NK=KK                             !
      NX=NV                             !
      DO 10 I=1,NX                      !
      XNOW(I)=GUESS(I)                  !
      XNEW(I)=XNOW(I)                   !
   10 CONTINUE                          !
      DELTA=DEL                         !
      IF(DELTA.GT.0.0)     GO TO 30     !TST FOR DELTA SPECIFIED
C                                       !
C     **************************************************************
C     COMPUTE DELTA IF NOT GIVEN (I.E. IF DELTA.LE.0)
C     **************************************************************
C                                       !
      DO 20 I=1,NX                      !LOOP ON # VARIABLES
      NA=NX+I                           !HI-LIMIT INDEX
      T=A(NA)-A(I)                      !HI-LIMIT - LO-LIMIT
      IF(DELTA.LT.T) DELTA=T            !SET DELTA TO DIFFERENCE?
   20 CONTINUE                          !
      DELTA=DELTA*DELFAC                !REDUCE DELTA BY DELFAC
C                                       !
C     **************************************************************
C     FIND A NEW "DIRECTION" FOR ALL VARIABLES
C     **************************************************************
C                                       !
   30 FNOW=FOX(XNOW)                    !COMPUTE QUALITY-OF-FIT
      FOLD=FNOW                         !SAVE IN FOLD
C                                       !
   50 DO 100 I=1,NX                     !LOOP ON ALL VARIABLES
      XNEW(I)=XNEW(I)+DELTA             !TRY + DELTA
      NA=NX+I                           !HI-LIMIT INDEX
      IF(XNEW(I).LE.A(NA)) GO TO 60     !TST AGAINST HI-LIMIT
      XNEW(I)=A(NA)                     !SET TO HI-LIMIT
   60 FNEW=FOX(XNEW)                    !COMPUTE NEW QFN
      IF(FNEW.GE.FNOW)     GO TO 80     !TST FOR WORSE FIT
   70 FNOW=FNEW                         !IF BETTER FIT, SAVE QFN
      GO TO 100                         !
   80 XNEW(I)=XNOW(I)-DELTA             !TRY - DELTA
      IF(XNEW(I).GE.A(I))  GO TO 90     !TST VS LO-LIMIT
      XNEW(I)=A(I)                      !SET TO LO-LIMIT
   90 FNEW=FOX(XNEW)                    !GET NEW QFN
      IF(FNEW.LT.FNOW)     GO TO 70     !SAVE QFN, IF BETTER FIT
      XNEW(I)=XNOW(I)                   !OTHERWISE, RESTORE VARIABLE
  100 CONTINUE                          !
C                                       !
      IF(FNOW.LT.FOLD)     GO TO 110    !TST FOR IMPROVED FIT
      NK=NK-1                           !IF NO, DEC REDUCTION CNTR
      IF(NK.LE.0)          GO TO 170    !REQUIRED # OF REDUCTIONS?
      DELTA=DELTA*DELFAC                !IF NO, REDUCE STEP SIZE
      GO TO 50                          !GO LOOK FOR NEW DIRECTION
C                                       !
C     **************************************************************
C     CHANGE ALL VARIABLES SIMULTANEOUSLY UNTIL FIT DOESN'T IMPROVE
C     **************************************************************
C                                       !
  110 DO 140 I=1,NX                     !LOOP ON ALL VARIABLES
      T=XNOW(I)                         !SAVE XNOW
      XNOW(I)=XNEW(I)                   !
      XNEW(I)=2.0*XNEW(I)-T             !MODIFY XNEW
      NA=NX+I                           !HI-LIMIT INDEX
      IF(XNEW(I).LT.A(NA)) GO TO 130    !TST FOR .LT. HI-LIMIT
      IF(XNEW(I).EQ.A(NA)) GO TO 140    !TST FOR .EQ. HI-LIMIT
      XNEW(I)=A(NA)                     !SET TO HI-LIMIT
      GO TO 140                         !
C
  130 IF(XNEW(I).GE.A(I))  GO TO 140    !TST FOR .GT. LO-LIMIT
      XNEW(I)=A(I)                      !SET TO LO-LIMIT
  140 CONTINUE                          !
      FNEW=FOX(XNEW)                    !
      IF(FNEW.GE.FNOW)     GO TO 150    !
      FNOW=FNEW                         !
      GO TO 110                         !
C                                       !
C     **************************************************************
C     FIT GOT WORSE - RESTORE PREVIOUS VALUES
C     **************************************************************
C                                       !
  150 DO 160 I=1,NX                     !
      XNEW(I)=XNOW(I)                   !
  160 CONTINUE                          !
      FOLD=FNOW
      GO TO 50                          !GO LOOK FOR NEW DIRECTION
C                                       !
C     **************************************************************
C     SAVE BEST VALUES IN X             !
C     **************************************************************
C                                       !
  170 FOFX=FNOW                         !
      DO 180 I=1,NX                     !
      X(I)=XNOW(I)                      !
  180 CONTINUE                          !
      RETURN                            !
      END                               !
