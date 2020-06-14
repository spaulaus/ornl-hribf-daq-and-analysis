C$PROG GFITT     - Gaussian peak fitting routine by M.J. Saltmarsh
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ------------------------------------------------------------------
C     From M.J. Saltmarsh & M.L. Halbert
C     ******************************************************************
C
      SUBROUTINE GFITT(NBC,KVAR,YINT,DYDX,ERR,FOFX,ISAV)
C
C     ------------------------------------------------------------------
C     M J SALMARSH'S PEAKFIT  FOR SEL 840A
C          ADAPTED FOR GAUSS-FIT VERSION OF SPASM   MLH 11-17-83
C       3-15-85  DON'T RESET INITIAL PARAMETERS IF ISAV=1
C                 (FLAG THAT "SAV" WAS DONE IN MAIN PROGRAM,
C                  SO USER INTENDS TO INVOKE RESULTS OF LAST FIT.)
C      10-23-86  CHANGE COMMON TO AGREE WITH NEW GASPT (SPASMT)
C      10-29-86  ADD COMMAND (IWD) TO OUTPUT LINE $1004
C      12-10-86  COMMON LABELS CHANGED TO BE COMPATIBLE WITH SPAT
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C   
      COMMON/SM02/COMP(2048,44),YCAL(2048),BGD(2048),DATA(2048),WT(2048)
C   
      COMMON/SM03/ XP(4,44),XPSAV(4,44),IPO(176),JPO(176)
C   
      COMMON/SM04/ KXF(4,44),KFUN(44),BETA(50),IVF(4)
C   
      COMMON/SM11/ MDYHMS(5),KINFD,MINSY,MAXSY
C   
C   
C     DIMENSION LOUT(12),KVAR(4)         !MLH  10-23-86
C     DIMENSION KVAR(4)                  !MLH  10-23-86
      CHARACTER*4  KVAR(4)               
C   
C     ------------------------------------------------------------------
C     MJS PKFT (JULY 1972) -- GAUSSIAN PEAK-FITTING -- PKFT
C     COMMON BLOCKS FROM PKFT:
C     ------------------------------------------------------------------
C   
      COMMON/GF01/ AM(180)
      COMMON/GF02/ YO(2048),SIGYO(2048),YC(2048),X(2048)
      COMMON/GF03/ P(18),KI(18),POLD(18),PD(18),NO,NP,IBOP,IPOP,IWOP
      COMMON/GF04/ PC(6,2048)
      DIMENSION SQSIG(2),DIAG(18),ERR(18),PK(6),D(18)
    
C     CONVERGENCE CRITERION
C
      DATA CONV/.0001/
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      FOFX=0.0
C   
C     ------------------------------------------------------------------
C     ONLY ABSOLUTE, NOT RELATIVE PEAK POSITIONS IN THIS VERSION
C     ------------------------------------------------------------------
C
      IPOP=0
    
C     NO MORE THAN 5 PEAKS
      IF(NPK.LT.6) GO TO 8
      WRITE(CMSSG,1000)
 1000 FORMAT('NO MORE THAN 5 PEAKS FOR GFIT - CMD IGNORED     ')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     ------------------------------------------------------------------
C     CONTINUE WITH LAST PARAMETER SET IF 'RFIT'
C     ------------------------------------------------------------------
C
    8 IF(ERR(18).LT.0.0) GO TO 35
    
C     ONLY LINEAR BKGD IN THIS VERSION --
      IBOP=0                   ! -- NOT LOGARITHMIC
      IF(NBC.LT.3) GO TO 20    ! -- NOT QUADRATIC
      WRITE(CMSSG,1002)
 1002 FORMAT('ONLY LINEAR BGD ALLOWED FOR GFIT - SET NBC=2    ')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     ------------------------------------------------------------------
C     INITIALIZE INPUT FOR LSQF BEFORE DEFINING THE PARAMETER SET.
C     ------------------------------------------------------------------
C
   20 DO 30 J=1,18
      P(J)=0.
   30 KI(J)=0
C
C     ------------------------------------------------------------------
C     INCORPORATE SUBR. SETUP HERE TO DEFINE PARAMETER SET
C     ------------------------------------------------------------------
C
   35 FL=0.001
C
C     ------------------------------------------------------------------
C     CHANGE NOTATION FROM SPASM TO PKFT
C     ------------------------------------------------------------------
C
      NO=I2
      NP=NPK
      IMESS=1
      DO 50 I=I1,I2
      X(I)=I
      YO(I)=DATA(I)
C
C     ------------------------------------------------------------------
C     USE SAME WEIGHTING AS SPASM DOES
C     ------------------------------------------------------------------
C
      SIGYO(I)=1.0
      IF(DATA(I).LE.0.) GO TO 40
      SIGYO(I)=SQRT(DATA(I))
      GO TO 50
   40 GO TO (42,50), IMESS
   42 WRITE(CMSSG,1003)
 1003 FORMAT('SOME CHANS .LE. ZERO! SET WEIGHT=1.0')
      CALL MESSLOG(LOGUT,LOGUP)
      IMESS=2
   50 CONTINUE
C
C     ------------------------------------------------------------------
C     CONTINUE WITH LAST PARAMETER SET IF 'RFIT'
C     ------------------------------------------------------------------
C
      IF(ERR(18).LT.0.0) GO TO 15
    
      DO 70 J=1,NPK
      P(J)=XP(1,J)
   70 P(J+10)=XP(2,J)*1.66478   ! SEE $210+1 IN GASP.FTN
C
C     ------------------------------------------------------------------
C     ESTIMATE BACKGROUND BY DRAWING A STRAIGHT LINE BETWEEN THE 1ST &
C     LAST CHANNELS.
C     IF BKGD FIXED, USE SLOPE AND INTERCEPT FROM SPASM
C     ------------------------------------------------------------------
C
      IF(IFBGD.EQ.0) GO TO 75
      P(17)=DYDX
      P(16)=YINT
      P(18)=0.0
      GO TO 78
    
   75 X1=X(1)
      X2=X(NO)
      Y1=YO(1)
      Y2=YO(NO)
      P(17)=(Y1-Y2)/(X1-X2)
      P(16)=Y1-X1*P(17)
      P(18)=0.
      KI(16)=1
      KI(17)=1
    
   78 DO 4 J=1,5
      JA=J+5
      JW=J+10
C
C     ------------------------------------------------------------------
C     SET UNUSED PEAK WIDTHS TO BE 1.
C     ------------------------------------------------------------------
C
      IF(P(J))5,6,5
    6 P(JW)=1.
      GO TO 4
    5 KI(J)=1
      KI(JW)=1
      KI(JA)=1
C
C     ------------------------------------------------------------------
C     FOLLOWING WIDTH ESTIMATE TO BE DONE ONLY ONCE
C     ------------------------------------------------------------------
C
      IF(J.GT.1) GO TO 100
C
C     ------------------------------------------------------------------
C     SKIP WIDTH ESTIMATE IF "SAV" WAS DONE        3-13-85
C     ------------------------------------------------------------------
C
      IF (ISAV.EQ.1) GO TO 105
      CALL WIDTH(P,YO,X,NO,NP,W)
C     IF W<0, AUTO ESTIMATE FAILED.  USE NORMAL PROCEDURE.
      IF(W.LE.0.0) GO TO 100
      P(11)=W
  100 P(JW)=P(11)
C   
  105 CONTINUE
C
C     ------------------------------------------------------------------
C     ESTIMATE THE PEAK AREA
C     ------------------------------------------------------------------
C
      POS=(P(J)-X(1))/(X(2)-X(1))+1.5  ! IF NO COMPR, POS=P(J)+0.5
      JP=POS
      BG=P(16)+P(17)*P(J)
      Y=YO(JP)-BG
      P(JA)=1.06446*Y*P(11)
    4 CONTINUE
C
C     ------------------------------------------------------------------
C     IWOP=0 MEANS PEAK WIDTHS ALL THE SAME AND VARY TOGETHER
C     TAKE VW 'ULOC' OR 'CLOC' TO MEAN THE SAME THING
C     ------------------------------------------------------------------
C
   15 IWOP=1
      IF(KVAR(2).EQ.'ULOC' .OR. KVAR(2).EQ.'CLOC') IWOP=0
      IF(IWOP)10,9,10
    9 IF(NP-1)10,10,11
   11 DO 12 J=2,NP
      JW=J+10
   12 KI(JW)=0
C
C     ------------------------------------------------------------------
C     FIX PARAMETERS IF REQUESTED BY USER
C     ------------------------------------------------------------------
C
   10 DO 80 J=1,NP
      IF(KXF(1,J).EQ.1) KI(J)=0
   80 IF(KXF(2,J).EQ.1) KI(J+10)=0
C     DEFINE THE NUMBER OF PARAMETERS TO BE VARIED
      NV=0
      DO 90 J=1,18
   90 NV=NV+KI(J)
      NM=(NV*(NV+1))/2
C
C     ------------------------------------------------------------------
C     READY TO START FITTING
C     ------------------------------------------------------------------
C
      IIT=0
   17 CALL LSQF(SQSIG,DIAG,NM,NV,FL)
      IIT=IIT+1
      SQ=SQSIG(2)**2
C   
      WRITE(CMSSG,1004)(IWD(I),I=1,7),SQ,IIT
 1004 FORMAT(1H>,7A4,'FITTING - SQ,ITER = ',F8.2,I4)
      CALL MESSLOG1(LOGUT,LOGUP)
C   
      IF(IIT.GE.25) GO TO 16
      IF(SQSIG(1)-SQSIG(2)-CONV) 16,16,17
    
   16 J=1
      DO 29 I=1,18
      IF(KI(I))32,31,32
   31 ERR(I)=0.   ! RESETS K(18) FLAG, BUT IT'S NO LONGER NEEDED.
      GO TO 29
   32 ERR(I)=SQRT(DIAG(J))*SQSIG(2)
      J=J+1
   29 CONTINUE
C
C     ------------------------------------------------------------------
C     CONVERT BACK TO SPASM NOTATION
C     ------------------------------------------------------------------
C
      DO 200 J=1,NPK
      XP(1,J)=P(J)
      XP(2,J)=P(J+10)/1.66478        ! SEE $210+1 IN GASP.FTN
      ERR(J+10)=ERR(J+10)/1.66478
  200 BETA(J)=P(J+5)/(1.7724539*XP(2,J)) ! SEE $562+5 IN GASP.FTN
C
C     ------------------------------------------------------------------
C     CALC BKGD FOR PLOT IF IT WAS VARIED! IF FIXED, SPASM DOES IT
C     ------------------------------------------------------------------
C
      IF(IFBGD.NE.0) GO TO 400
      DO 300 I=I1,I2
  300 BGD(I)=P(16)+P(17)*X(I)+P(18)*X(I)*X(I)
C
C     ------------------------------------------------------------------
C     CALCULATE SUM OF FITTED PEAKS (WITH BKGD)
C     AND PUT INTO YCAL ARRAY FOR DISPLAY OF FIT
C     ------------------------------------------------------------------
C
      DO 350 I=I1,I2
  350 CALL CALC(X(I),YCAL(I),P,D,PK,IBOP,IPOP,IWOP)
    
  400 FOFX=SQSIG(2)**2
      END
