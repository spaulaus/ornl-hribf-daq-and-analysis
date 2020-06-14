C$PROG GAFFER
      SUBROUTINE GAFFER(IERR,MSER)
C
      INTEGER*2 GSID,GFOR,GSOR,GNOG,GLEN,MLEN,GAPO
C
      INTEGER*4 GLOC
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
C
      COMMON/GGG/ GSID(128),GFOR(128),GSOR(128),GNOG(128),GLEN(128),
     &GLOC(128),MLEN(128),MLOC(128),GAPO(128),NGSET
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      INTEGER*2 MILH(524288)
C
      INTEGER*4 MESS(10,7),MSER(10)
      CHARACTER*40 MSC(7)
C
      EQUIVALENCE (MSC(1),MESS(1,1))
      EQUIVALENCE (MILH(1),MILF(1))
C
      DATA  MSC/'SYMTAX ERROR IN $GAF SPECIFICATION     ',
     2          'ILLEGAL GATE-SET NUMBER SPECIFICATION  ',
     3          'GATE-SET NUMBER ALREADY EXISTS         ',
     4          'ILLEGAL "LENGTH" IN GATE-SET SPEC      ',
     5          'ILLEGAL # OF GATES SPECIFIED IN $GAF   ',
     6          'TOO MANY GATE-SETS - MAX# = 128        ',
     7          'GATE LIST REGION OVERFLO - MAX# = 4096 '/
C
      INTEGER*4  GF,LI
      CHARACTER*4 CGF, CLI
      EQUIVALENCE (CGF,GF), (CLI,LI)
      DATA       cGF,cLI/'GF  ','LI  '/
C
      SAVE
C
C
C     **************************************************************
C     GSID(I) = ID# FOR     ITH GATE SET
C     GFOR(I) = FORM OF     ITH GATE-SET ('LI'/'MA' = LIST/MAP)
C     GSOR(I) = SOURCE OF   ITH GATE-SET ('IN'/'GF' = CHIL/FILE)
C     GNOG(I) = # GATES IN  ITH GATE-SET
C     GLEN(I) = LENGTH FOR  ITH GATE-SET (BASIS ON WHICH DEFINED)
C     GLOC(I) = HALF-WD INDEX OF ITH GATE-SET IN MIL (UN-MAPPED)
C     MLOC(I) = HALF-WD INDEX OF ITH GATE-SET IN MIL (MAPPED)
C     MLEN(I) = MAP LENGTH (BASIS) FOR ITH GATE-SET
C     NGSET   = # OF GATE-SETS SPECIFIED
C
C     (MILF(I),I=1    ,16384) - IS THE INS -REGION OF MIL
C     (MILF(I),I=16385,24576) - IS THE GATE-REGION OF MIL
C     (MILF(I),I=24577,65536) - IS THE MAP -REGION OF MIL
C
C     IGATOF  = GATE-REGION OFFSET IN MIL (IN HALF-WDS) = 32768
C     IMAPOF  = MAP -REGION OFFSET IN MIL (IN HALF-WDS) = 49152
C
C     NGATL   = # OF HALF-WDS LOADED INTO GATE-REGION OF MIL
C     NMAPL   = # OF HALF-WDS LOADED INTO MAP -REGION OF MIL
C
C     MXGATL  = MAX # HALF-WDS IN GATE-REGION
C     MXMAPL  = MAX # HALF-WDS IN MAP -REGION
C     **************************************************************
C     PROCESS DIRECTIVES OF FORM - $GAF (ISET,#GATES) ........
C     **************************************************************
C
      IERR=0                                !RESET ERROF FLAG
C
      CALL REFOR(IWD,LWD,ITYP,NF,5,80,NTER) !RE-FORMAT LINE
      IF(NTER.NE.0)  GO TO 510              !TST FOR ERROR
      NDO=NF/2                              !REQUIRE #-FIELDS TO BE
      IF(2*NDO.NE.NF) GO TO 510             !MULTIPLE OF 2
      N=0                                   !RESET FIELD CNTR
C
      DO 100 KK=1,NDO                       !LOOP ON # GATE-SETS
      N=N+1                                 !INC FIELD CNTR
      CALL LIMIV(LWD(1,N),1,32768,IV,IERR)  !GET GATE-SET #
      IF(IERR.NE.0) GO TO 520               !TST FOR ERROR
C
      DO 20 I=1,NGSET                       !LOOP ON CURRENT SET#'S
      IF(IV.EQ.GSID(I)) GO TO 530           !TST FOR ALREADY EXISTS
   20 CONTINUE
C
      N=N+1                                 !INC FIELD CNTR
      CALL LIMIV(LWD(1,N),1,400,JV,IERR)    !GET # OF GATES IN SET
      IF(IERR.NE.0) GO TO 550               !TST FOR ERROR
      IF(NGSET.GE.128) GO TO 560            !TST FOR TOO MANY SETS
C
      NGSET=NGSET+1                         !INC # GATE-SETS
      GSID(NGSET)=IV                        !STORE GATE-SET ID#
      GLEN(NGSET)=0                         !SET LENGTH=0 (UNSET)
      GNOG(NGSET)=JV                        !STORE # OF GATES IN SET
      GSOR(NGSET)=GF                        !STORE SOURCE CODE (FILE
      GFOR(NGSET)=LI                        !STORE FORM CODE (LIST)
C
      IF(NGATL+2*JV.GT.MXGATL) GO TO 570    !TST FOR GATE-REG OVFLO
      NDX=IGATOF+NGATL                      !GET LAST HALF-WD LOADED
      GLOC(NGSET)=NDX+1                     !LOC OF NEW SET IN MIL
C
      DO 40 I=1,JV                          !LOOP ON # GATES IN SET
      NDX=NDX+1                             !INC MILH INDEX
      MILH(NDX)=0                           !STORE HI-LIMIT
      NDX=NDX+1                             !INC MILH INDEX
      MILH(NDX)=1                           !STORE LO-LIMIT
   40 CONTINUE
      NGATL=NGATL+2*JV                      !# GATE-WDS STORED
C
  100 CONTINUE
      RETURN
C
C     **************************************************************
C     RETURN ERROR CODE AND SET UP ERROR MESSAGE
C     **************************************************************
C
  510 JJ=1
      GO TO 600
  520 JJ=2
      GO TO 600
  530 JJ=3
      GO TO 600
  550 JJ=5
      GO TO 600
  560 JJ=6
      GO TO 600
  570 JJ=7
C
  600 DO 610 I=1,10
      MSER(I)=MESS(I,JJ)
  610 CONTINUE
      IERR=JJ
      RETURN
      END
