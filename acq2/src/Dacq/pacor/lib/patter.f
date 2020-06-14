C$PROG PATTER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE PATTER(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PAC9/ NAMP(3,9),LATW(200,9),BITN(200,9),NPB(9),NPAT
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      INTEGER*4 NAMLAT(3,50),INDXL(50),NLAT
C
      INTEGER*4 PATNAM(3),LATNAM(3),LATORD(50),
     &          LOBIT(50),HIBIT(50),IV(3)
C
      DATA NCALL,NPAT,NLAT,MXPAT,MXBIT/0,0,0,9,200/
C
      character*4 cSETW
      equivalence (cSETW, SETW)
      DATA cSETW/'SETW'/
C
      CHARACTER*4  KIMO
C
      SAVE
C
      IF(NCALL.GT.0) GO TO 100
C
      DO 50 N=1,NUMT                        !LOOP ON FULL SOURCE TABLE
      IF(KIMO(N).NE.'$LAT') GO TO 50
      NLAT=NLAT+1
      NAMLAT(1,NLAT)=NAMO(1,N)                 !SAVE ALL LATCH-NAMES
      NAMLAT(2,NLAT)=NAMO(2,N)
      NAMLAT(3,NLAT)=NAMO(3,N)
      INDXL(NLAT)   =NAMO(4,N)                 !AND LATCH-NAME INDICES
   50 CONTINUE
      NCALL=1
C
  100 CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER) !SET RE-FMT WIDTH 10 12
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)    !RE-FORMAT
C
      DO 110 I=1,3                             !SAVE PAT-NAME
      PATNAM(I)=LWD(I,1)
  110 CONTINUE
      NFF=NF-1                                 !#FIELDS
      KDO=NFF/4                                !#LOOPS TO DO
      IF(4*KDO.NE.NFF) GO TO 500               !CHECK FOR MULT OF 4
C
      DO 130 N=1,NPAT                          !CHECK FOR PAT-NAME
      DO 120 I=1,3                             !ALREADY DEFINED
      IF(PATNAM(I).NE.NAMP(I,N)) GO TO 130
  120 CONTINUE
      GO TO 150                                !IF YES, SAVE INDEX
  130 CONTINUE
      NPAT=NPAT+1                              !IF NO,  ADD TO LIST
C
      IF(NPAT.GT.MXPAT) THEN
      WRITE(CMSSG,135)NPAT
  135 FORMAT('MAX# PAT DEFINITIONS EXCEEDED AT NPAT =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NPAT=MXPAT
                        ENDIF
C
      N=NPAT
      DO 140 I=1,3
      NAMP(I,N)=PATNAM(I)
  140 CONTINUE
C
  150 JPAT=N
      JJ=1
      DO 300 K=1,KDO                           !LOOP ON #FIELDS
      JJ=JJ+1
C
      DO 160 I=1,3
      LATNAM(I)=LWD(I,JJ)                      !LOAD LATCH-NAME
  160 CONTINUE
C
      DO 170 I=1,3
      JJ=JJ+1
      CALL MILV3(LWD(1,JJ),IV(I),XV,KIND,IERR) !DECODE NUMERIC FIELDS
  170 CONTINUE
C
      LNNDX    =IV(1)                          !LATCH-NAME INDEX
      LOBIT(K) =IV(2)                          !MIN BIT TO USE
      HIBIT(K) =IV(3)                          !MAX BIT TO USE
C
      DO 200 LL=1,NLAT                         !LOOK FOR LATNAM IN 
C                                              !"LIBRARY"
      DO 180 II=1,3                            !CHECK NAMES
      IF(LATNAM(II).NE.NAMLAT(II,LL))GOTO 200 
  180 CONTINUE
      IF(LNNDX.NE.INDXL(LL))         GOTO 200  !CHECK NAME INDEX
      LATORD(K)=LL                             !SAVE LATCH ORDINAL
      GO TO 300                                !IF ALL MATCH
  200 CONTINUE
      GO TO 510                                !ERROR IF NO MATCH
  300 CONTINUE
C
      II=NPB(JPAT)
      DO 320 NN=1,KDO
      IBL=LOBIT(NN)
      IBH=HIBIT(NN)
      DO 310 I=IBL,IBH
      II=II+1
C
      IF(II.GT.MXBIT) THEN
      WRITE(CMSSG,305)II
  305 FORMAT('MAX# PAT-BITS EXCEEDED AT II =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      II=MXBIT
                      ENDIF
C
      LATW(II,JPAT)=LATORD(NN)
      BITN(II,JPAT)=I
  310 CONTINUE
  320 CONTINUE
      NPB(JPAT)=II
      GO TO 1000
C
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR IN $pat ENTRY')
      CALL ERRLOG(LOGUT,LOGUP)
      GO TO 1000
C
  510 WRITE(CMSSG,515)LATNAM,LNNDX
  515 FORMAT('LATCH-NAME, INDEX NOT FOUND = ',3A4,I8)
      CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)
      RETURN
      END
