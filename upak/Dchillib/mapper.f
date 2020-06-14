C$PROG MAPPER
      SUBROUTINE MAPPER(IERR,MSER)
C
      INTEGER*2    GSID,GNOG,GLEN,MLEN,GAPO
C
      CHARACTER*2  GFOR,GSOR
C
      INTEGER*4 GLOC
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
      CHARACTER*4 CIWD(20)
      EQUIVALENCE (CIWD,IWD)
C
      COMMON/GGG/ GSID(128),GFOR(128),GSOR(128),GNOG(128),GLEN(128),
     &GLOC(128),MLEN(128),MLOC(128),GAPO(128),NGSET
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      INTEGER*2 MILH(524288)
C
      INTEGER*4 MESS(10,14),MSER(10),GATES(2,200),GATEL(400)
      CHARACTER*40 MSC(14)
C
      INTEGER*4 JWD(32)
C
      EQUIVALENCE (MILH(1),MILF(1))
      EQUIVALENCE (GATEL(1),GATES(1,1))
      EQUIVALENCE (MSC(1),MESS(1,1))
C
      DATA  MSC/'UN-RECOGNIZED DIRECTIVE                ',
     2          'SYNTAX ERROR                           ',
     3          'SYNTAX ERROR OR ILLEGAL GATE-SET #     ',
     4          'REFERENCED GATE-SET # NOT DEFINED      ',
     5          'REFERENCED GATE-SET PREVIOUSLY MAPPED  ',
     6          'GATE-SET NOT DEFINED VIA $GAF          ',
     7          'ILLEGAL GATE-SET LENGTH OR SYNTAX ERROR',
     8          'GATE-SET PREVIOUSLY DEFINED            ',
     9          'ILLEGAL GATE-LIMIT OR SYNTAX ERROR     ',
     A          '# OF GATE-LIMITS IS AN ODD             ',
     B          'TOO MANY GATE-SETS - MAX# = 128        ',
     C          'NOT ENOUGH SPACE TO STORE GATE-SET     ',
     D          'NOT ENOUGH SPACE TO MAP REQUESTED SET  ',
     E          'LINE NOT PROCESSED                     '/
C
      CHARACTER*4  KMD
C
      INTEGER*4    X26,X46,X4C,X54
      DATA         X26,X46,X4C,X54/z'26',z'46',z'4C',z'54'/
C
      SAVE
C
C     **************************************************************
C     PROCESS - $MAPF (LENG),ISN,ISN,ISN.......
C     **************************************************************
C
      IERR=0                                !RESET ERROR FLAG
      KMD=CIWD(1)                           !SAVE COMMAND-WORD
C
      CALL ILBYTE(IT,IWD,4)                 !GET 5TH BYTE
      IF(IT.EQ.X46) GO TO 100               !TST FOR "F" ($MAPF)
      IF(IT.EQ.X4C) GO TO 200               !TST FOR "L" ($MAPL)
      IF(IT.EQ.X54) GO TO 200               !TST FOR "T" ($GLST)
      GO TO 510                             !OTHERWISE, ERROR
C
  100 CALL REFOR(IWD,LWD,ITYP,NF,6,80,NTER) !REFORMAT LINE
      IF(NTER.NE.0) GO TO 520               !TST FOR ERROR
C
      CALL LIMIV(LWD(1,1),1,16384,LENG,IERR)!PICK UP LENGTH
      IF(IERR.NE.0)     GO TO 670           !TST FOR ERROR
      IT=LOGB2(LENG)                        !GET LOG-BASE-2
      IF(2**IT.NE.LENG) GO TO 670           !TST FOR PWR-OF-2 LENGTH
C
      DO 150 N=2,NF                         !LOOP ON SET# FIELDS
      CALL LIMIV(LWD(1,N),1,32767,ISN,IERR) !GET GATE-SET #
      IF(IERR.NE.0) GO TO 530               !TST FOR ERROR
C
      DO 110 I=1,NGSET                      !LOOP ON GATE-SET LIST
      IF(ISN.EQ.GSID(I)) GO TO 120          !TST FOR MATCH
  110 CONTINUE
      GO TO 540                             !ERROR IF NOT FOUND
C
  120 IS=I                                  !INDEX OF MATCHING SET
      IF(GFOR(IS).NE.'LI') GO TO 550        !FORM   MUST BE "LIST"
      IF(GSOR(IS).NE.'GF') GO TO 560        !SOURCE MUST BE "FILE"
      IF(NMAPL+LENG.LE.MXMAPL) GO TO 130    !TST FOR MAP SPACE
C
      WRITE(LU6,125)ISN
  125 FORMAT(1H ,'INSUFFICIENT SPACE TO MAP GATE-SET #',I6)
      GO TO 150
C
  130 NDX=IMAPOF+NMAPL                      !HALF-WD OFFSET IN MIL
      LOC=NDX+1                             !LOC OF 1ST NEW ENTRY
C
      DO 140 I=1,LENG                       !LOOP ON SET "LENGTH"
      NDX=NDX+1                             !INC MIL INDEX
      MILH(NDX)=-1                          !SET TO IMPOSSIBLE
  140 CONTINUE
C
      NMAPL=NMAPL+LENG                      !STOR NEW MAP-LENGTH
      GFOR(IS)='MA'                         !SET FORM TO "MAPPED"
      MLOC(IS)=LOC                          !POINT TO MAP LOCATION
      MLEN(IS)=LENG                         !SET MAP LENGTH BASIS
  150 CONTINUE
C
      CALL LODJWD(IWD,JWD)
      CALL SAVLIN(JWD,MSER,IERR,'DONE',NSOL,1)
      RETURN
C
C     **************************************************************
C     PROCESS - $MAPL LENG,ISN (LO,HI) (LO,HI) (LO,HI) .............
C                     &(LO,HI) (LO,HI) .............................
C
C     AND     - $GLST LENG,ISN (LO,HI) (LO,HI) (LO,HI) .............
C                     &(LO,HI) (LO,HI) .............................
C     **************************************************************
C     PICK UP LENG AND ISN FROM FIRST LINE ALWAYS
C     **************************************************************
C
  200 CALL REFOR(IWD,LWD,ITYP,NF,6,80,NTER) !RE-FORMAT 1ST LINE
      IF(NTER.NE.0) GO TO 620               !TST FOR ERROR
      IF(NF.LT.2) GO TO 620                 !REQUIRE 2 FIELDS MIN
      CALL LIMIV(LWD(1,1),2,16384,LENG,IERR) !GET SET "LENGTH"
      IF(IERR.NE.0) GO TO 670               !SIZE OR SYNTAX ERROR?
      IT=LOGB2(LENG)                        !GET LOG-BASE-2
      IF(2**IT.NE.LENG) GO TO 670           !REQUIRE PWR-OF-2 LENGTH
      IHIC=LENG-1                           !MAX CHANNEL #
      CALL LIMIV(LWD(1,2),1,32767,ISN,IERR) !GET GATE-SET #
      IF(IERR.NE.0) GO TO 630               !TST FOR ERROR
C
      DO 210 I=1,NGSET                      !LOOP ON CURRENT SET#'S
      IF(ISN.EQ.GSID(I)) GO TO 680          !ERROR IF EXISTS
  210 CONTINUE
      L=0                                   !RESET GATE-LIMIT CNTR
      IF(NF.LT.3) GO TO 240                 !TST FOR NO LIMITS
      NA=3                                  !FIRST FIELD TO PROCESS
C
C     **************************************************************
C     PROCESS LIMIT-FIELDS TO PRODUCE LIMIT-LIST "GATEL"
C     **************************************************************
C
  220 DO 230 N=NA,NF                        !LOOP ON # OF FIELDS
      L=L+1                                 !INC GATE-LIMIT CNTR
      CALL LIMIV(LWD(1,N),0,IHIC,GATEL(L),IERR) !GET GATE-LIMIT
      IF(IERR.NE.0) GO TO 690               !TST FOR ERROR
  230 CONTINUE
C
  240 CALL LODJWD(IWD,JWD)                  !SET-UP SOURCE LINE
C
      READ(LIN,250,END=305)IWD              !READ IN NEXT LINE
  250 FORMAT(20A4)
      CALL ILBYTE(IT,IWD,0)                 !GET 1ST BYTE
      IF(IT.NE.X26) GO TO 300               !TST FOR "&" (CONT)
C
      CALL SAVLIN(JWD,MSER,0,'DONE',NSOL,1) !SAVE PREVIOUS SRC LINE
C
      NSOL=NSOL+1                           !IF YES, INC LINE CNTR
C
      CALL REFOR(IWD,LWD,ITYP,NF,2,80,NTER) !RE-FORMAT SOURCE LINE
      IF(NTER.NE.0) GO TO 620               !TST FOR ERROR
      IF(NF.LT.1)   GO TO 620               !TST FOR NULL
      NA=1                                  !1ST FIELD TO PROCESS
      GO TO 220                             !GO DO IT
C
C     **************************************************************
C     STORE GATES IN THE GATE-LIST REGION OF MIL FIRST
C     **************************************************************
C
  300 BACKSPACE LIN                         !ALWAYS READ 1 TOO FAR
  305 IF(2*(L/2).NE.L) GO TO 700            !TST FOR EVEN # LIMITS
      NG=L/2                                !# OF GATES
      IF(NGATL+NG.GT.MXGATL) GO TO 720      !TST FOR LIST-SPACE
      CALL GASORT(GATES,NG,IERR,MSER)       !SORT GATES & TST FOR
C                                           !LO.GT.HI, ETC
      IF(IERR.NE.0) GO TO 815               !TST FOR ERROR
C
      NGSET=NGSET+1                         !INC # GATE-SETS
      IF(NGSET.GT.100) GO TO 710            !TST FOR TOO MANY
C
      CALL SAVLIN(JWD,MSER,0,'DONE',NSOL,1) !SAVE LAST SOURCE LINE
C
      GSID(NGSET)=ISN                       !GATE-SET ID
      GFOR(NGSET)='LI'                      !FORM IS "LIST"
      GSOR(NGSET)='IN'                      !SOURCE IS "INTERNAL"
      GNOG(NGSET)=NG                        !# OF GATES IN SET
      GLEN(NGSET)=LENG                      !GATE-SET "LENGTH"
      GLOC(NGSET)=IGATOF+NGATL+1            !LOC OF 1ST ENTRY IN MIL
C
      NDX=IGATOF+NGATL                      !INIT MIL INDEX
      DO 320 J=1,NG                         !LOOP ON # OF GATES
      NDX=NDX+1                             !INC MIL-INDEX
      MILH(NDX)=GATES(2,J)                  !STOR HI-LIMIT
      NDX=NDX+1                             !INC MIL-INDEX
      MILH(NDX)=GATES(1,J)                  !STOR LO-LIMIT
  320 CONTINUE
      NGATL=NGATL+2*NG                      !NEW GATE-LIST LENGTH
C
      IF(KMD.EQ.'$GLS') RETURN              !TST FOR MAP-REQUEST
C
C     **************************************************************
C     NOW MAP GATES IF YOU HAVE THE REQUIRED SPACE
C     **************************************************************
C
      IF(NMAPL+LENG.LE.MXMAPL) GO TO 325    !TST FOR MAP SPACE
C
      WRITE(LU6,125)ISN
      RETURN
C
  325 IA=IMAPOF+NMAPL+1                     !FIRST MAP LOCATION
      LOC=IA                                !FIRST MAP LOCATION
      IB=IA+LENG-1                          !LAST  MAP LOCATION
      DO 330 I=IA,IB                        !LOOP TO ZOT MAP
      MILH(I)=-1                            !SET TO IMPOSSIBLE
  330 CONTINUE
C
      DO 350 N=1,NG                         !LOOP ON # OF GATES
      IA=GATES(1,N)+LOC                     !LO-INDEX IN MIL
      IB=GATES(2,N)+LOC                     !HI-INDEX IN MIL
      DO 340 I=IA,IB                        !LOOP ON RANGE OF GATE
      MILH(I)=N-1                           !SET TO GATE#-1
  340 CONTINUE
  350 CONTINUE
      GFOR(NGSET)='MA'                      !FORM IS "MAPPED"
      MLOC(NGSET)=LOC                       !PNT TO 1ST LOC IN MIL
      MLEN(NGSET)=LENG                      !SET MAP LENGTH BASIS
      NMAPL=NMAPL+LENG                      !NEW MAP LENGTH
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP MESSAGE
C     **************************************************************
C
  510 JJ=1
      GO TO 800
  520 JJ=2
      GO TO 800
  530 JJ=3
      GO TO 800
  540 JJ=4
      GO TO 800
  550 JJ=5
      GO TO 800
  560 JJ=6
      GO TO 800
C
  620 JJ=2
      GO TO 900
  630 JJ=3
      GO TO 900
  670 JJ=7
      GO TO 900
  680 JJ=8
      GO TO 900
  690 JJ=9
      GO TO 900
  700 JJ=10
      GO TO 800
  710 JJ=11
      GO TO 800
  720 JJ=12
C
  800 IERR=JJ
      DO 810 I=1,10
      MSER(I)=MESS(I,JJ)
  810 CONTINUE
C
  815 CALL SAVLIN(JWD,MSER,IERR,'DONE',NSOL,1)
      RETURN
C
C     **************************************************************
C     LIST ERROR MESSAGE & READ THRU CONTINUATION LINES FOR $MAPL
C     **************************************************************
C
  900 DO 905 I=1,10
      MSER(I)=MESS(I,JJ)
  905 CONTINUE
C
C     ********************************** SAVE CURRENT LINE & ERROR
C
      CALL LODJWD(IWD,JWD)
      IERR=JJ
      CALL SAVLIN(JWD,MSER,IERR,'DONE',NSOL,1)
C
C     ********************************** READ & SAVE CONT LINES
C
      DO 910 I=1,10
      MSER(I)=MESS(I,14)
  910 CONTINUE
C
  920 READ(LIN,250,END=950)IWD
      CALL ILBYTE(IT,IWD,0)
      IF(IT.NE.X26) GO TO 940
      NSOL=NSOL+1
      CALL LODJWD(IWD,JWD)
      CALL SAVLIN(JWD,MSER,IERR,'DONE',NSOL,1)
      GO TO 920
C
  940 BACKSPACE LIN
      IERR=0
  950 RETURN
C
      END
