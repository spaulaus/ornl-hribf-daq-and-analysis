C$PROG GATLOC
      SUBROUTINE GATLOC(IP,ISN,NA,NB,ISHI,LENG,LOC,LOCG,IGT,IERR,MSER)
C
      INTEGER*2 GSID,GFOR,GSOR,GNOG,GLEN,MLEN,GAPO
C
      INTEGER*2 GXNX,GXLN
      INTEGER*2 ISH
C
      INTEGER*4 GLOC
C
      INTEGER*4 GXLOC
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      COMMON/GGG/ GSID(128),GFOR(128),GSOR(128),GNOG(128),GLEN(128),
     &GLOC(128),MLEN(128),MLOC(128),GAPO(128),NGSET
C
      COMMON/III/ GXNX(128),GXLN(128),GXLOC(128),NGXN
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      INTEGER*2 MILH(524288)
C
      INTEGER*4 MESS(10,3),MSER(10)
      CHARACTER*40 MSC(3)
C
      EQUIVALENCE (MSC(1),MESS(1,1))
      EQUIVALENCE (MILH(1),MILF(1))
C
      DATA  MSC/'REQUESTED GATE-LIST SET# NOT FOUND     ',
     2          'ILLEGAL GATE NUMBERS REQUESTED         ',
     3          'INSUFFICIENT SPACE FOR AUX GATE-LIST   '/
C
      CHARACTER*4  IGT
C
      INTEGER*2    MA
      character*2  cma
      equivalence  (cma,ma)
      DATA        cMA/'MA'/
C
      SAVE
C
C     **************************************************************
C     IP   = PARM # ON WHICH GATE IS TO BE DONE
C     ISN  = GATE-SET # OF INTEREST
C     NA   = FIRST GATE-ORDINAL OF INTEREST
C     NB   = LAST  GATE-ORDINAL OF INTEREST
C     ISHI = # OF BITS TO SHIFT TST PARM BEFORE GATING
C     LENG = LENGTH BASIS OF GATE SET
C     LOC  = MIL-INDEX (FULL-WD) OF FIRST GATE OF INTEREST
C     IGT  = GATE-FORM ('LIST' FOR NOW ANYWAY)
C     **************************************************************
C     GAPO(I)  - GIVES INDEX IN AUXILLARY LISTS (GXLOC, GXLN, GXNX)
C                ASSOCIATED WITH ITH PRIMARY GATE-LIST
C
C     GXLOC(K) - GIVES MIL-INDEX OF AUX GATE-LIST (DIFFERS FROM
C              - PARENT LIST ONLY IN THE LENGTH ON WHICH IT IS BASED)
C
C     GXLN(K)  - GIVES "LENGTH" ASSOCIATED WITH GXLOC(K)
C
C     GXNX(K)  - GIVES INDEX OF NEXT ASSOCIATED ENTRY IN GX-ARRAYS
C
C     NGXN     = # OF ELEMENTS OF GX-ARRAY/S USED
C     **************************************************************
C
      IERR=0                            !RESET ERROR FLAG
      ISHI=0                            !SET SHIFT CNT TO ZERO
      IGT='LIST'                        !GATE-LIST "FORM"
C
      DO 10 I=1,NGSET                   !LOOP ON GATE-SET ID'S
      IF(ISN.EQ.GSID(I)) GO TO 20       !TST FOR MATCH
   10 CONTINUE
      GO TO 510                         !ERROR IF NOT FOUND
C
   20 IS=I                              !INDEX OF REQUIRED SET
C
C     **************************************************************
C     NA=1 & NB=GNOG(IS) SAYS DO FULL SET           - MAP IF POSSIBLE
C     NA=0               SAYS DO FULL SET           - MAP IF POSSIBLE
C     NA=K & NB=0        SAYS DO GATE-K             - NO MAPPING
C     NA=K & NB=L        SAYS DO GATE-K THRU GATE-L - NO MAPPING
C     **************************************************************
C
      NG=GNOG(IS)                       !# OF GATES
      LENP=LPAR(IP)                     !PARM-LENGTH
      IF(GFOR(IS).NE.MA)      GO TO 25  !TST FOR MAPPING POSSIBLE
      IF(NA.EQ.0)             GO TO 300 !TST FOR FULL SET REQUEST
      IF(NA.EQ.1.AND.NB.EQ.NG)GO TO 300 !TST FOR FULL SET REQUEST
C
   25 IF(NA.GT.0)             GO TO 30  !NO MAPPING, FULL SET?
      NA=1                              !IF YES, RESET NA
      NB=NG                             !AND     RESET NB
      GO TO 40                          !GO PROCESS
C
   30 IF(NB.LE.0) NB=NA                 !NOT FULL SET, RESET NB?
      IF(NB.GT.NG)            GO TO 520 !TST FOR LEGAL REQUEST
      IF(NA.GT.NB)            GO TO 520 !TST FOR LEGAL REQUEST
      IF(NA.LT.0)             GO TO 520 !TST FOR LEGAL REQUEST
C
   40 LENG=GLEN(IS)                     !GATE-LENGTH
      IF(LENG.GT.0) GO TO 45            !TST FOR GATES UN-SET AS YET
      LENG=LENP                         !IF NOT, SET TO REQUESTED
      GLEN(IS)=LENP                     !LENGTH
C
   45 IF(LENP.NE.LENG) GO TO 50         !TST FOR REQUESTED LENGTH
C                                       !IF YES, RETURN LOCATION
      LOCG=GLOC(IS)+2*(NA-1)            !HALF-WD LOC OF 1ST GATE
      LOC=(LOCG+1)/2                    !FULL-WD LOC OF 1ST GATE
      RETURN
C
C     **************************************************************
C     PRIMARY GATE-LIST NOT OF REQUIRED LENGTH
C     CHECK AUXILLARY LISTS FOR REQUIRED LENGTH
C     **************************************************************
C
   50 IDX=GAPO(IS)                      !INDEX IN AUX-DIRECTORY
      LDX=IDX                           !PNTS TO LAST DIR ENTRY
      IF(IDX.LE.0) GO TO 100            !TST FOR NO AUX LIST
C
   60 IF(GXLN(IDX).EQ.LENP) GO TO 80    !TST FOR CORRECT LENGTH
      IDX=GXNX(IDX)                     !GET NEXT AUX-DIR INDEX
      IF(IDX.LE.0) GO TO 100            !TST FOR EXIST
      GO TO 60                          !IF YES, GO TST IT
C
   80 LOCG=GXLOC(IDX)+2*(NA-1)          !HALF-WD LOC OF 1ST GATE
      LOC=(LOCG+1)/2                    !FULL-WD LOC OF 1ST GATE
      RETURN
C
C     **************************************************************
C     COPY GATE-LIST OF CORRECT LENGTH INTO GATE-REGION OF MIL
C     **************************************************************
C
  100 IF(NGATL+2*NG.GT.MXGATL) GO TO 530!TST FOR SUFFICIENT SPACE
      NGXN=NGXN+1                       !NEW AUX-DIR INDEX
      ISH=LOGB2(LENP)-LOGB2(LENG)       !SHIFT CNT FOR NEW GATES
      MC=0                              !INIT HI-LIMIT CHAN MOD
      MFAC=2**ISH                       !EXPAND/COMPRESS FACTOR
      IF(MFAC.GT.1) MC=MFAC-1           !HI-LIMIT CHAN MODIFICATION
      GAPO(IS)=NGXN                     !PNT TO LAST AUX-DIR ENTRY
      GXNX(NGXN)=LDX                    !PNT TO PREVIOUS DIR ENTRY
      GXLN(NGXN)=LENP                   !SET PARM-LENGTH
C
      NDX=IGATOF+NGATL                  !INIT NEW MIL-INDEX
      MDX=GLOC(IS)-1                    !INIT OLD MIL-INDEX
      GXLOC(NGXN)=NDX+1                 !LOC OF NEW GATE-LIST
C
      DO 120 I=1,NG                     !LOOP ON # OG GATES
      NDX=NDX+1                         !INC NEW MIL-INDEX
      MDX=MDX+1                         !INC OLD MIL-INDEX
      MILH(NDX)=ISHFT(MILH(MDX),ISH)+MC !STOR NEW LIMIT (SHIFTED)
      NDX=NDX+1                         !INC NEW MIL-INDEX
      MDX=MDX+1                         !INC OLD MIL-INDEX
      MILH(NDX)=ISHFT(MILH(MDX),ISH)    !STOR NEW LIMIT
  120 CONTINUE
C
      LOCG=GXLOC(NGXN)+2*(NA-1)         !HALF-WD MIL-INDEX
      LOC=(LOCG+1)/2                    !FULL-WD MIL-INDEX
      NGATL=NGATL+2*NG                  !NEW GATE-LIST LENGTH
      RETURN
C
C     **************************************************************
C     REQUEST IS FOR A MAPPED GATE-LIST
C     **************************************************************
C
  300 LOCH=MLOC(IS)                     !HALF-WD LOC OF MAP IN MIL
      LOCG=GLOC(IS)                     !HALF-WD LOC OF GATE-LIST
      LOC=(LOCH+1)/2                    !FULL-WD LOC OF MAP IN MIL
      LENM=MLEN(IS)                     !LENGTH BASIS FOR MAP
      ISHI=LOGB2(LENP)-LOGB2(LENM)      !SHIFT CNT FOR TST-PARM
      LENG=MLEN(IS)-1                   !SIZE AFTER SHIFT
      IGT='MAP '                        !GATE-LIST FORM "MAPPED"
      NA=1                              !FIRST GATE#
      NB=GNOG(IS)                       !LAST  GATE#
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP MESSAGE
C     **************************************************************
C
  510 JJ=1
      GO TO 600
  520 JJ=2
      GO TO 600
  530 JJ=3
C
  600 DO 610 I=1,10
      MSER(I)=MESS(I,JJ)
  610 CONTINUE
      IERR=JJ
      RETURN
      END
