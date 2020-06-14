C$PROG DIMEN
      SUBROUTINE DIMEN(KPF,IERR,MSER)
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
      integer*4 isyn, isyt, isyd, isyp,ispf, isyv, nsym, nsyv
C
      INTEGER*4 MSG(10,7),MSER(10)
      CHARACTER*40 MSC(7)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/
     1'SYNTAX ERROR - SYM/DIM MORE THAN 8 CHAR ',
     2'SYNTAX ERROR - INCORRECT # OF FIELDS    ',
     3'SYNTAX ERROR - INCORRECT FIELD TYPE     ',
     4'CONTANS PREVIOUSLY DEFINED SYMBOL       ',
     5'SYMBOL TABLE OVERFLOW (MAX # IS 100)    ',
     6'DIMENSION OUT OF RANGE (1 TO 8192)      ',
     7'TOTAL DIMENSIONALITY .GT. 16384         '/
C
      DATA MAXNS,MAXNV/100,16384/
C
      SAVE
C
C     **************************************************************
C     PROCESS ENTRIES OF THE FORM:
C
C     $DIM SYMA(NUM),SYMB(NUM),.....
C     **************************************************************
C     ISYN(1,J),ISYN(2,J) - CONTAINS NAME OF JTH SYMBOL
C     ISYT(J) - CONTAINS TYPE OF JTH SYMBOL
C     ISYD(J) - CONTAINS DIMENSION OF JTH SYMBOL
C     ISPF(J) = '$DIM' SAYS JTH SYMBOL IS NOT A "PARAMETER"
C     ISPF(J) = '$DIP' SAYS JTH SYMBOL IS     A "PARAMETER"
C     ISYP(J) - GIVES INDEX IN "ISYV" FOR FIRST SYMBOL VALUE
C     ISYV(ISYP(J)+K-1) - CONTAINS KTH VALUE OF JTH SYMBOL
C
C     NSYM  = NUMBER OF SYMBOLS DEFINED
C     NSYV  = NUMBER OF SYMBOL VALUES DEFINED (SUM OF DIMENSIONS)
C
C     MAXNS = MAXIMUM NUMBER OF SYMBOLS ALLOWED
C     MAXNV = NAXIMUM NUMBER OF SYMBOL VALUES ALLOWED
C     **************************************************************
C
      IERR=0
      CALL REFOR(IWD,LWD,ITYP,NF,5,80,NTER) !RE-FORMAT THE LINE
C
      IF(NTER.NE.0)    GO TO 210            !TST FOR ERROR
      NDO=NF/2                              !GET # SYMBOLS TO DO
      IF(2*NDO.NE.NF)  GO TO 220            !REQUIRE EVEN # FIELDS
C
      N=0                                   !INIT FIELD COUNTER
      NS=NSYM                               !NS = CURRENT # SYMBOLS
      NV=NSYV                               !NV = CURRENT # VALUES
C
      DO 100 I=1,NDO                        !LOOP ON # SYMBOLS
      N=N+1                                 !INC FIELD COUNTER
      IF(ITYP(N).NE.1) GO TO 230            !TST FOR ALPHABETIC
      IF(LOCSYM(LWD(1,N)).NE.0) GO TO 240   !TST FOR ALREADY EXIST
      NS=NS+1                               !INC # SYMBOLS COUNTER
      IF(NS.GT.MAXNS)  GO TO 250            !TST FOR TOO MANY
C
      ISYN(1,NS)=LWD(1,N)                   !PUT NAME IN SYMBOL TABLE
      ISYN(2,NS)=LWD(2,N)
C
      N=N+1                                 !INC FIELD COUNTER
      IF(ITYP(N).NE.2) GO TO 230            !TST FOR NUMERIC
      CALL LIMIV(LWD(1,N),1,8192,LEN,JERR)  !GET DIMENSION
      IF(JERR.NE.0)    GO TO 260            !TST FOR ERROR
      ISYT(NS)=2                            !SET TYPE = DIMENSIONED
      ISYD(NS)=LEN                          !SET THE LENGTH
      ISPF(NS)=KPF                          !SET "PARAMETER" FLAG
      ISYP(NS)=NV+1                         !SET PNTR TO 1ST VALUE
      NV=NV+LEN                             !GET NEW # VALUES
      IF(NV.GT.MAXNV)  GO TO 270            !TST FOR TOO MANY
C
      NN=ISYP(NS)-1                         !INIT ISYV-INDEX
      DO 50 L=1,LEN                         !SET VALUES UN-DEFINED
      NN=NN+1                               !INC INDEX
      ISYV(NN)=Z'7fffffff'                 !SET UN-DEFINED VALUE
      ISYV(NN)= sign(isyv(nn),-1)-1                 !SET UN-DEFINED VALUE
   50 CONTINUE
C
  100 CONTINUE
      NSYM=NS                               !SAVE NEW # SYMBOLS
      NSYV=NV                               !SAVE NEW # VALUES
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP ERROR MESSAGE BUFFER
C     **************************************************************
C
  210 JJ=1
      GO TO 300
  220 JJ=2
      GO TO 300
  230 JJ=3
      GO TO 300
  240 JJ=4
      GO TO 300
  250 JJ=5
      GO TO 300
  260 JJ=6
      GO TO 300
  270 JJ=7
C
  300 DO 310 I=1,10
      MSER(I)=MSG(I,JJ)
  310 CONTINUE
      IERR=JJ
      RETURN
      END
