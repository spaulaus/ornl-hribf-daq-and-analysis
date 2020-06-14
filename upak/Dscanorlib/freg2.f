C$PROG FREG2     - Sets up free-form-gates for Linux SCANOR & LEMOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 07/20/2004
C     ******************************************************************
C
      SUBROUTINE FREG2(LIN,NAME,IERR)
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
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4     LIN,IERR
      CHARACTER*(*) NAME
C     ------------------------------------------------------------------
C
      INTEGER*4    IX(64),IY(64)
C
      INTEGER*4    MESBUF(13,9), NMES0(13)
      CHARACTER*52 MEC(9), CMES0
      EQUIVALENCE (MESBUF,MEC)
C
      INTEGER*4    IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWDS(13)
      CHARACTER*4  KMI
      EQUIVALENCE (KMI,IWD(1))
C
      INTEGER*4    IDIR(880)
      INTEGER*4    JLO,JHI,Y0,JDO,NDX
C
      INTEGER*4    IOS,NERR,NDUP,NBIG,NB
C
      INTEGER*4    NXY,NBN,IRWTM,IIBAN,NRAT,LOGB2
C
      INTEGER*4    I,J,JJ,KK,NN,NDO,IDX,LGX
C
      INTEGER*4    ISHFT
C
      EXTERNAL     ISHFT
C
      INTEGER*4    EOFTST
C
      REAL*4       S,X
C
      EQUIVALENCE (IWDS(1),IWD(1)),(NMES0(1),CMES0)
C
      INTEGER*4    MAXNDX,MAXNG,ICALL,NG
      DATA         MAXNDX,MAXNG,ICALL,NG/NNDX,NFFG,0,0/
C
C     ------------------------------------------------------------------
      INTEGER*4    RECLVALU
C     ------------------------------------------------------------------
C
      DATA MEC/'* * * SYNTAX ERROR - PRECEDING LINE                ',
     2         '* * * ERRORS DETECTED - TRY AGAIN                  ',
     3         'FREE-FORM-GATES SET - GOOD LUCK                    ',
     4         'TOTAL FREE-FORM-GATE LENGTH TOO LONG               ',
     5         'MORE THAN 200 FREE-FORM-GATES                      ',
     6         'MORE THAN 63 X,Y POINTS                            ',
     7         'BAN-FILE EMPTY                                     ',
     8         '! ! ! WARNING, ADDING TO ALREADY EXISTING GATES    ',
     9         'ERROR DOING DIRECTORY I/O               '/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     ==================================================================
C     COMPATIBLE WITH STANDARD BAN-FILE (FEB 4, 1983)
C     GATES ARE STORED SEQUENTIALLY IN THE ORDER READ. THEY
C     ARE MOST EASILY ADDRESSED BY THIS SEQUENCE NUMBER,
C     IN WHICH CASE ID's ARE UNIMPORTANT AND MAY BE DUPLICATE.
C     AN ID# TO SEQUENCE# MAP (IDBAN) IS KEPT, AND WARNINGS OF
C     DUPLICATE ID's GIVEN. THE LAST OCCURENCE OF AN ID IS THE
C     ONE IN THE MAP. THE MAP FORMAT IS: SEQUENCE# = IDBAN(ID)
C     ------------------------------------------------------------------
C     TO CHANGE FREE-FORM-GATE CAPACITY, CHANGE NNDX, NFFG & MAXID
C     AND CORRESPONDING DIMENSIONS OF "LFGL", "LFGH" AND "IPX" THRU
C     "LYG", ETC.
C     ==================================================================
C     IPX(I)  = X-PARM FOR ITH GATE
C     IPY(I)  = Y-PARM FOR ITH GATE
C     NBSX(I) = # OF BITS TO SHIFT X-PARM BEFORE USING AS INDEX
C     NBSY(I) = # OF BITS TO SHIFT Y-PARM BEFORE TESTING AGAINST LIMITS
C     NUPM(I) = NEW PARM # TO SET (USUALLY TO 1) IF TEST PASSES
C     LFOF(I) - GIVES INDEX OF 1ST ELEMENT IN LFGL/LFGH CORRESPONDING TO 
C               GATE I
C
C     LFGL    = ARRAY CONTAINING LO-LIMITS
C     LFGH    = ARRAY CONTAINING HI-LIMITS
C     NFG     = # OF FREE-FORM GATES DEFINED
C
C     IX(I)   = X-VALUE OF ITH X,Y-POINT IN CLOCKWISE X,Y-LIST
C     IY(I)   = Y-VALUE OF ITH X,Y-POINT IN CLOCKWISE X,Y-LIST
C     NXY     = # OF ENTRIES CURRENTLY IN X,Y-LIST
C
C     LXD(I)  = X-PARM RANGE IN DATA LIST FOR ITH GATE
C     LXG(I)  = X-PARM RANGE IN X,Y-LIST  FOR ITH GATE
C     LYD(I)  = Y-PARM RANGE IN DATA LIST FOR ITH GATE
C     LYG(I)  = Y-PARM RANGE IN X,Y-LIST  FOR ITH GATE
C     ==================================================================
C
      IERR=0
C
      OPEN(UNIT   = LIN,
     &     FILE   = NAME,
     &     RECL   = RECLVALU(80),
     &     STATUS = 'OLD',
     &     FORM   = 'UNFORMATTED',
     &     IOSTAT = IOS,
     &     ACCESS = 'DIRECT')
C
      IF(IOS.NE.0)  THEN
                    CALL IOFERR(IOS)
                    IERR=IOS
                    RETURN
                    ENDIF
C
      IF(ICALL.GT.0) GO TO 30
C
      NG=0
      NFG=0
      ICALL=1
      DO 10 I=1,MAXID
      IDBAN(I)=0
   10 CONTINUE
C
   30 NERR=0
      NDUP=0
      NBIG=0
      NG=NFG
C
      IF(NG.NE.0) CALL UMESSO(1,MESBUF(1,8))
C   
C     ------------------------------------------------------------------
C     READ IN DIRECTORY - INITIALIZE - FOR IBN=0 ONLY
C     ------------------------------------------------------------------
C   
      DO  40 I=1,880
      IDIR(I)=0
   40 CONTINUE
      NN=0
      NBN=0
C
   50 DO 80 KK=1,5
      READ(LIN,REC=NBN+1,IOSTAT=IOS)IWD
      IF(EOFTST(IOS).NE.0) GO TO 85
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 485
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 485
      IF(NF.NE.16)  GO TO 485
C
      DO 60 J=1,16
      IF(ITYP(J).NE.2) GO TO 485
      NN=NN+1
      CALL LIMIV(LWD(1,J),-9999,99999,IDIR(NN),IERR)
      IF(IERR.NE.0) GO TO 485
   60 CONTINUE
      NBN=NBN+1
   80 CONTINUE
      NBN=NBN+960
      GO TO 50 
C
C     ------------------------------------------------------------------
C     SET UP CROSS REFERENCE LIST
C     Note that duplicate ID's, or ID out of range are not fatal
C     errors. Such gates can still be referred to by gate number,
C     but not by ID. 
C     ------------------------------------------------------------------
C
   85 NBN=0
C
      DO 90 I=1,880
C
      IF(IDIR(I).LE.0) GO TO 90
C
      IF(IDBAN(IDIR(I)).GT.0)THEN
      NDUP=NDUP+1
      CMES0=' '
      WRITE(CMES0,'(''BANANA ID DUPLICATED: ID ='',I5)')IDIR(I)
      CALL UMESSO(1,NMES0)
                             ENDIF
C
      IF(IDIR(I).LE.MAXID) THEN
      NBN=NBN+1
      IDBAN(IDIR(I))=NG+NBN
      IDIRBAN(NG+NBN)=IDIR(I)
                           ENDIF
C
      IF(IDIR(I).GT.MAXID) THEN
      NBIG=NBIG+1
      NBN=NBN+1
      CMES0=' '
      WRITE(CMES0,'(''BANANA ID TOO LARGE: ID ='',I5)')IDIR(I)
      CALL UMESSO(1,NMES0)
                           ENDIF
C
   90 CONTINUE
C   
C     ------------------------------------------------------------------
C     READ IN LINE IMAGE AND DETERMINE TYPE
C     ------------------------------------------------------------------
C
      NXY=0
      IRWTM=0
      IIBAN=0
  100 IF(NERR.GT.10)THEN
                    CLOSE(LIN)
                    RETURN
                    ENDIF
C
      IRWTM=IRWTM+1
      READ(LIN,REC=IRWTM,IOSTAT=IOS)IWD
      IF(EOFTST(IOS).NE.0) GO TO 120
      CALL IOFERR(IOS)
C
      IF(KMI.EQ.'GATE') THEN
                        IIBAN=IIBAN+1
                        WRITE(CMSSG,105)IWDS,IIBAN,IDIR(IIBAN)
  105                   FORMAT(13A4,'GATE#,ID# =',2I8)
                        CALL MESSLOG(LOGUT,LOGUP)
                        ENDIF
C
      IF(KMI.EQ.'GATE') GO TO 140
      IF(KMI.EQ.'END ') GO TO 140
      IF(KMI.EQ.'CXY ') GO TO 400
      GO TO 100
C
C     ------------------------------------------------------------------
C     PROCESS ANY PREVIOUSLY ACCUMULATED X,Y-LIST
C     ------------------------------------------------------------------
C
 120  KMI='END '
      IF(NG.LT.1) GO TO 485
C
 140  IF(NG.EQ.NFG) GO TO 300
      NRAT=LXD(NG)/LXG(NG)
      NBSX(NG)=-LOGB2(NRAT)
      NRAT=LYD(NG)/LYG(NG)
      NBSY(NG)=LOGB2(NRAT)
      IX(NXY+1)=IX(1)
      IY(NXY+1)=IY(1)
C
C     ------------------------------------------------------------------
C     SET LIMIT ARRAYS TO "IMPOSSIBLE"
C     ------------------------------------------------------------------
C
      IDX=LFOF(NG)
      LGX=LXG(NG)
      DO 150 I=1,LGX
      IF(IDX.GT.MAXNDX) GO TO 460
      LFGL(IDX)=2
      LFGH(IDX)=1
      IDX=IDX+1
  150 CONTINUE
C
C     ------------------------------------------------------------------
C     SET LIMITS IMPLIED BY X,Y-LIST
C     ------------------------------------------------------------------
C
      DO 200 I=1,NXY
      IF(IX(I).EQ.IX(I+1)) GO TO 200
      IF(IX(I).GT.IX(I+1)) GO TO 170
C
C     SET UP FOR POSITIVE-GOING DELTA-X
C
      JLO=IX(I)
      JHI=IX(I+1)
      S=FLOAT(IY(I+1)-IY(I))/FLOAT(IX(I+1)-IX(I))
      Y0=IY(I)
      JDO=JHI-JLO+1
      X=0.0
      NDX=LFOF(NG)+JLO
      DO 160 J=1,JDO
      IF(NDX.GT.MAXNDX) GO TO 460
      LFGH(NDX)=Y0+S*X+0.5
      LFGH(NDX)=ISHFT(LFGH(NDX)+1,NBSY(NG))-1
      IF(LFGH(NDX).GT.LYD(NG)-1) LFGH(NDX)=LYD(NG)-1
      NDX=NDX+1
      X=X+1.0
  160 CONTINUE
      GO TO 200
C
C     ------------------------------------------------------------------
C     SET UP FOR NEGATIVE-GOING DELTA-X
C     ------------------------------------------------------------------
C
  170 JLO=IX(I+1)
      JHI=IX(I)
      S=FLOAT(IY(I)-IY(I+1))/FLOAT(IX(I)-IX(I+1))
      Y0=IY(I+1)
      JDO=JHI-JLO+1
      X=0.0
      NDX=LFOF(NG)+JLO
      DO 180 J=1,JDO
      IF(NDX.GT.MAXNDX) GO TO 460
      LFGL(NDX)=Y0+S*X+0.5
      LFGL(NDX)=ISHFT(LFGL(NDX)+0,NBSY(NG))
      NDX=NDX+1
      X=X+1.0
  180 CONTINUE
  200 CONTINUE
      IF(KMI.EQ.'END ') GO TO 500
C
C     ------------------------------------------------------------------
C     PROCESS THE NEXT "GATE ENTRY"
C     ------------------------------------------------------------------
C
  300 CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0.OR.NF.LT.7) GO TO 320
      DO 310 I=1,NF
      IF(ITYP(I).NE.2) GO TO 320
  310 CONTINUE
      GO TO 340
  320 CALL UMESSO(1,MESBUF(1,1))
      NERR=NERR+1
      GO TO 100
  340 NG=NG+1
      IF(NG.GT.MAXNG) GO TO 470
      LFOF(NG)=1
      IF(NG.GT.1) LFOF(NG)=LFOF(NG-1)+LXG(NG-1)
      CALL IVALU(LWD(1,1),IPX(NG),IERR)
      IF(IERR.NE.0) GO TO 455
      CALL IVALU(LWD(1,2),IPY(NG),IERR)
      IF(IERR.NE.0) GO TO 455
      CALL IVALU(LWD(1,3),LXD(NG),IERR)
      IF(IERR.NE.0) GO TO 455
      CALL IVALU(LWD(1,4),LXG(NG),IERR)
      IF(IERR.NE.0) GO TO 455
      CALL IVALU(LWD(1,5),LYD(NG),IERR)
      IF(IERR.NE.0) GO TO 455
      CALL IVALU(LWD(1,6),LYG(NG),IERR)
      IF(IERR.NE.0) GO TO 455
      CALL IVALU(LWD(1,7),NUPM(NG),IERR)
      IF(IERR.NE.0) GO TO 455
      IF(NF.GE.8) CALL IVALU(LWD(1,8),IAUX(NG),IERR)
      IF(IERR.NE.0) GO TO 455
      IF(NF.GE.9) CALL IVALU(LWD(1,9),JAUX(NG),IERR)
      IF(IERR.NE.0) GO TO 455
C
      NXY=0
      GO TO 100
C
C     ------------------------------------------------------------------
C     ACCUMULATE INTO THE CURRENT X,Y-LISY
C     ------------------------------------------------------------------
C
  400 CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NF.LE.0) GO TO 100
      IF(2*(NF/2).NE.NF) GO TO 420
      IF(NTER.NE.0) GO TO 420
      DO 410 I=1,NF
      IF(ITYP(I).NE.2) GO TO 420
  410 CONTINUE
      GO TO 430
  420 CALL UMESSO(1,MESBUF(1,1))
      NERR=NERR+1
      GO TO 100
  430 NDO=NF/2
      J=1
      DO 450 I=1,NDO
      NXY=NXY+1
      IF(NXY.GT.63) GO TO 480
C
      CALL IVALU(LWD(1,J),IX(NXY),IERR)
      IF(IERR.NE.0) GO TO 455
      J=J+1
      CALL IVALU(LWD(1,J),IY(NXY),IERR)
      IF(IERR.NE.0) GO TO 455
      J=J+1
  450 CONTINUE
      GO TO 100
C
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES
C     ------------------------------------------------------------------
C
  455 NERR=NERR+1
      JJ=1
      GO TO 490
  460 NERR=NERR+1
      JJ=4
      GO TO 490
  470 NERR=NERR+1
      JJ=5
      GO TO 490
  480 NERR=NERR+1
      JJ=6
      GO TO 490
C
  485 NERR=NERR+1
      JJ=7
C
C     ==================================================================
C     SET # OF GATES TO ZERO IF WE HAD ANY ERRORS
C     ==================================================================
C
  490 CALL UMESSO(1,MESBUF(1,JJ))
  500 NFG=0
      IF(NERR.EQ.0) NFG=NG
      IF(NERR.NE.0) CALL UMESSO(1,MESBUF(1,2))
C
      CLOSE(LIN)
C
      IF(NERR.NE.0) RETURN
C
      WRITE(CMSSG,505)NFG
  505 FORMAT('NO. OF BANANA GATES SET - TOTAL =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,510)IIBAN
  510 FORMAT('NO. OF BANANA GATES SET - NOW   =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(NDUP.NE.0) THEN
                    CALL MESSLOG(LOGUT,LOGUP)
                    WRITE(CMSSG,515)NDUP
  515               FORMAT('WARNING!! NO. OF DUPLICATED IDs =',I8)
                    CALL MESSLOG(LOGUT,LOGUP)
                    ENDIF
C
      IF(NBIG.NE.0) THEN
                    WRITE(CMSSG,520)MAXID,NBIG
  520               FORMAT('WARNING!! #IDs.GT.MAXID=',I5,'  IS ',I6)
                    CALL MESSLOG(LOGUT,LOGUP)
                    ENDIF
      RETURN
C
C     ==================================================================
C
C$ENTR ZFREG2    - Zeros all internal bananas
C
C     ==================================================================
C
      ENTRY ZFREG2()
      NFG=0
      NG=0
      DO I=1,MAXID
         IDBAN(I)=0
      ENDDO
      DO I=1,NFFG
         IDIRBAN(I)=0
      ENDDO
      END
