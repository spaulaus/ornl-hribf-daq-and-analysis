C$PROG TABLOG
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ******************************************************************
C
      SUBROUTINE TABLOG(LU)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXI=2000)
      PARAMETER (MXC=500)
      PARAMETER (MXG=100)
      PARAMETER (MXD=100)
      PARAMETER (MXK=50)
      PARAMETER (MXR=10)
C
      COMMON/PAC1/ KINMOD,MODKOD,NAMOD(3),MODATA(2,12)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PAC4/ LATN(MXI),MASK(MXI),KINS(MXI),
     &             TBLI(2,MXI),TBLN(2,MXI),NXTI(2,MXI),
     &             NUNDX(MXI),NTS
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C
      COMMON/PAC6/ LABLIS(2,MXI),LABVAL(MXI),NLA 
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PAC8/ KLASID(50),KLASND(50),KLASMU(50),NKLAS,LKIND
C
      COMMON/PAC9/ NAMP(3,9),LATW(200,9),BITN(200,9),NPB(9),NPAT
C
      COMMON/PACA/ JCRA(MXI),JSLO(MXI),JSUB(MXI),JFRE(MXI),
     &             JIDN(MXI),NNAF
C
      COMMON/PACB/ CNAFI(200),DATAI(200),NNAFI,
     &             CNAFQ(200),DATAQ(200),NNAFQ,
     &             CNAFR(200),DATAR(200),NNAFR
C
      COMMON/PACC/ RIFLATI(MXR),RIFMASK(MXR),RIFMOTY(2,MXR),NRIF
C
      COMMON/PACD/ FERID(32,576),FERC(576),FERN(576),FERT(576),NFER,
     &             FERERID
C
      COMMON/PACE/ FASID(256,32),FASC(32), FASN(32), FAST(32), NFAS,
     &             FASERID
C
      COMMON/PACF/ CLRC(200),CLRN(200),CLRA(200),CLRF(200),NCLR
C
      COMMON/PACG/ CRATLST(50),NCRAT
C
      COMMON/PACH/ KILATI(MXK),KILMSK(MXK),KILTYP(MXK),NKIL
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/PACK/ BOOLIST(MXC),NBOO
C
      COMMON/PACL/ CDPAT(MXD),CDMSK(MXD),CDCNT(MXD),NCDN
C
      COMMON/PACM/ NAMPAC(20)
C
      COMMON/PACN/ CAMID(32,32,8),CAMC(256),CAMN(256),CAMT(256),NCAM,
     &             CAMERID
C     ------------------------------------------------------------------
      COMMON/PACP/ DELAZ(30),NDELAZ
      INTEGER*4    DELAZ,    NDELAZ
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
C     ------------------------------------------------------------------
      COMMON/PACV/ VADCMAP(12),VTDCMAP(12),VQDCMAP(12),
     .             VADCID(408),VTDCID(408),VQDCID(408)
      INTEGER*4    VADCMAP,    VTDCMAP,    VQDCMAP,
     .             VADCID,     VTDCID,     VQDCID
C     ------------------------------------------------------------------
      INTEGER*4    ADCID(34,12),TDCID(34,12),QDCID(34,12)
C
      EQUIVALENCE (ADCID,VADCID),(TDCID,VTDCID),(QDCID,VQDCID)
C     ------------------------------------------------------------------
C
      INTEGER*4 IT(1000),JT(1000)
C
      SAVE
C     
C     ******************************************************************
C     /PAC1/ - DEFINITIONS - FROM ONE HARDWARE TABLE ENTRY
C     ******************************************************************
C     KINMOD      = MODULE TYPE ($LAT, $CAM, $FER, $FAS...)
C     NAMOD       = MODULE NAME (FOR EXAMPLE - ADC1) UP TO 12 BYTES
C     MODATA(1,1) = MODULE-NAME FIRST INDEX
C     MODATA(2,1) = MODULE-NAME INDEX INCREMENT
C     MODATA(1,2) = CRATE#
C     MODATA(1,3) = SLOT#
C     MODATA(1,4) = FIRST SUB-ADDRESS TO READ
C     MODATA(2,4) = LAST  SUB-ADDRESS TO READ
C     MODATA(1,5) = FUNCTION CODE FOR READ
C     MODATA(1,6) = CLASS#
C     MODATA(1,7) = DETECTOR#, FIRST VALUE
C     MODATA(2,7) = DETECTOR#, INCREMENT
C     MODATA(1,8) = ENTRY#
C     MODATA(1,9) = FUNCTION CODE FOR CLEAR
C     MODATA(1,10)= SUB-ADDRRESS FOR CLEAR
C     MODATA(1,11)= DELAY TIME BEFORE READ (no longer used ******)
C     MODATA(1,12)= ID-NUMBER, FIRST
C     MODATA(2,12)= ID-NUMBER, INCREMENT
C
C     ******************************************************************
C     /PAC2/ - DEFINITIONS - LIBRARY OF HARDWARE DEFINITIONS
C     ******************************************************************
C     NAMO(I,J),I=1,3 = JTH MODULE-NAME (12 BYTES MAX)
C     NAMO(4,J)       = JTH MODULE-NAME INDEX
C     KIMO(J)         = JTH MODULE-TYPE ($LAT, $CAM, ...)
C     CRAT(J)         = JTH CRATE#
C     SLOT(J)         = JTH SLOT#
C     SUBA(J)         = JTH SUB-ADDRESS
C     FRED(J)         = JTH READ  FUNCTION-CODE
C     FCLR(J)         = JTH CLEAR FUNCTION-CODE
C     ACLR(J)         = JTH SUB-ADDRESS FOR CLEAR
C     DLAT(J)         = JTH DELAY-TIME BEFORE READOUT
C     CLAS(J)         = JTH CLASS# ******************NOT USED NOW
C     DETN(J)         = JTH DETECTOR# ***************NOT USED NOW
C     ENTR(J)         = JTH ENTRY# ******************NOT USED NOW
C     IDNM(J)         = JTH PARAMETER-ID
C     MOTY(1,J)       = MODULE TYPE CODE (SEE ROUTINE MODCOD)
C     MOTY(2,J)       = RANGE, GAIN, ETC FOR ASSOCIATED MODULE TYPE
C     USED(J)         = YES/NO SAYS USED/NOT-USED IN CONDITIONAL READOUT
C     NUMT            = NUMBER OF ENTRIES IN THESE TABLES
C     ******************************************************************
C     /PAC4/ - DEFINITIONS - CONDITIONAL READOUT DRIVER TABLES
C     ******************************************************************
C     LATN(J)    = LATCH-WORD INDEX FOR JTH TEST
C     MASK(J)    = MASK FOR USE IN      JTH TEST
C     KINS(J)    = INSTRUCTION TYPE (DIAGNOSTICS ONLY)
C     TBLI(I,J)  = POINTS TO NAF-LIST (/PACA/)- I=1,2 for TRUE/FALSE 
C     TBLN(I,J)  = # NAFs TO TAKE FROM TABLE  - I=1,2 for TRUE/FALSE
C     NXTI(I,J)  = NEXT TEST-TABLE INDEX      - I=1,2 for TRUE/FALSE
C     NTS        = NUMBER OF TEST-TABLE ENTRIES TO USE
C
C     ******************************************************************
C     /PAC5/ - DEFINITIONS - INTERMEDIATE CONDITIONAL READOUT TABLES
C     ******************************************************************
C     LABL(J)    = JTH STATEMENT LABEL 
C     INST(J)    = JTH INSTRUCTION - IFU,IFS,IFA,IFN,CONT,GOTO
C     NAMD(I,J)  = JTH DEVICE NAME (I=1,3) - ADC1, TDC2, ETC
C     INDX(J)    = JTH DEVICE NAME INDEX
C     MSKI(J)    = JTH MASK FOR IF-TESTS
C     IDES(J)    = JTH DESTINATION (LABEL)
C     NREA(J)    = JTH READOUT LIST POINTER  - ASSOCIATED WITH "READ"
C     ILOR(J)    = JTH READOUT LIST LO-LIMIT - ASSOCIATED WITH "CONDITION"
C     IHIR(J)    = JTH READOUT LIST HI-LIMIT - ASSOCIATED WITH "CONDITION"
C     NCI        = NUMBER OF ENTRIES IN THIS INTERMEDIATE READOUT TABLE
C
C     ******************************************************************
C     /PAC6/ - DEFINITIONS - STATEMENT LABEL TABLES
C     ******************************************************************
C     LABLIS(1,J) = 1ST 4 BYTES OF JTH STATEMENT LABEL (ASCII)
C     LABLIS(2,J) = 2ND 4 BYTES OF JTH STATEMENT LABEL (ASCII)
C     LABVAL(J)   = VALUE OF       JTH STATEMENT LABEL (INTEGER)
C     NLA         = # OF ENTRIES IN THESE TABLES
C
C     ******************************************************************
C     /PAC7/ - DEFINITIONS - LISTING DIRECTIVES
C     ******************************************************************
C     ISORL  =   SOURCE-LINE COUNTER
C     IEXPL  = EXPANDED-LINE COUNTER
C     LISTYP = 'NONE' SAYS LIST NONE
C     LISTYP = 'SOR ' SAYS LIST SOURCE-LINES ONLY
C     LISTYP = 'ALL ' SAYS LIST SOURCE & GENERATED LINES
C     NERR   = NUMBER OF COMPILATION ERRORS
C
C     ******************************************************************
C     /PAC8/ - DEFINITIONS - CLASS TABLES
C     ******************************************************************
C     KLASID(J) = JTH CLASS-ID
C     KLASND(J) = #      OF DETECTORS FOR JTH CLASS
C     KLASMU(J) = # ENTRIES/DETECTOR  FOR JTH CLASS
C     NKLAS     = # OF ENTRIES IN THESE TABLES
C
C     ******************************************************************
C     /PAC9/ - DEFINITIONS - MULTI-WORD PATTERN-WORD TABLES
C     ******************************************************************
C     NAMP(I,K),I=1,3 = KTH PATTERN-WORD NAME
C     LATW(J,K)       = JTH LATCH-WD ASSOCIATED WITH KTH PATTERN-WD
C     BITN(J,K)       = JTH LATCH-WD-BIT        FOR  KTH PATTERN-WD
C     NBP(K)          = # OF BITS    ASSOCIATED WITH KTH PATTERN-WD
C     NPAT            = # OF PATTERN-WDS DEFINED
C
C     ******************************************************************
C     /PACA/ - DEFINITIONS - C,N,A,F,ID TABLE FOR CONDITIONAL CAMAC
C                                                 READOUT
C     ******************************************************************
C     JCRA(I) - CRATE#    FOR ITH ENTRY
C     JSLO(I) - SLOT#     FOR ITH ENTRY
C     JSUB(I) - SUB-ADDR  FOR ITH ENTRY
C     JFRE(I) - FUNC-CODE FOR ITH ENTRY (FOR READ)
C     JIDN(I) - ID-NUMBER FOR ITH ENTRY
C     NNAF    = NUMBER OF TABLE ENTRIES
C
C     ******************************************************************
C     /PACB/ - DEFINITIONS - INITIALIZING CNAF'S
C     ******************************************************************
C     CNAFI(I) - ITH PACKED INITIALIZING CNAF
C     DATAI(I) - ITH ASSOCIATED DATA WORD IF ANY
C     NNAFI    = NUMBER OF INITIALIZING CNAF'S
C     CNAFQ(I) - ITH PACKED WRAPUP CNAF (DONE AT END OF EACH EVENT)
C     DATAQ(I) - ITH ASSOCIATED DATA WORD IF ANY
C     NNAFQ    = NUMBER OF WRAPUP CNAF'S
C     CNAFR(I) - ITH PACKED RUN CNAF (DONE AT RUN COMMAND)
C     DATAR(I) - ITH ASSOCIATED DATA WORD IF ANY
C     NNAFR    = NUMBER OF RUN CNAF'S
C 
C     ******************************************************************
C     /PACC/ - DEFINITIONS - FERRA & FASTBUS CONDITIONAL READOUT
C     ******************************************************************
C     RIFLATI(I)   - LATCH-WORD  FOR ITH CONDITIONAL READOUT
C     RIFMASK(I)   - MASK        FOR ITH CONDITIONAL READOUT
C     RIFMOTY(1,I) - MODULE TYPE FOR ITH CONDITIONAL READOUT
C     RIFMOTY(2,I) - RANGE, ETC  FOR ASSOCIATED MODULE
C     NRIF         - NUMBER OF           CONDITIONAL READOUTS
C
C     ******************************************************************
C     /PACD/ - DEFINITIONS - FERRA   ID AND MODULE C,N,TYPE TABLES
C     ******************************************************************
C     FERC(I)    - ITH CRATE NUMBER
C     FERN(I)    - ITH SLOT  NUMBER
C     FERT(I)    - ITH MODULE TYPE CODE
C     FERID(I,J) - ID TABLE (ITH SUB-ADDRESS, JTH SLOT#
C     FERERID    - DEFAULT "ERROR-ID" TO LOAD IN ID-TABLE
C
C     ******************************************************************
C     /PACE/ - DEFINITIONS - FASTBUS ID AND MODULE C,N,TYPE TABLES
C     ******************************************************************
C     FASC(I)    - ITH CRATE NUMBER
C     FASN(I)    - ITH SLOT  NUMBER
C     FAST(I)    - ITH MODULE TYPE CODE
C     FASID(I,J) - ID TABLE (ITH SUB-ADDRESS, JTH SLOT#)
C     FASERID    - DEFAULT "ERROR-ID" TO LOAD IN ID-TABLE
C
C     ******************************************************************
C     /PACF/ - DEFINITIONS - CAMAC C,N,A,F TABLES FOR CLEAR
C     ******************************************************************
C     CLRC(I) - CRATE NUMBER FOR ITH CLEAR
C     CLRN(I) - SLOT  NUMBER FOR ITH CLEAR
C     CLRA(I) - SUB-ADDRESS  FOR ITH CLEAR
C     CLRF(I) - FUNCTION     FOR ITH CLEAR
C     NCLR    - NUMBER OF TANLE ENTRIES
C
C     ******************************************************************
C     /PACG/ - DEFINITIONS - CAMAC CRATE LIST
C     ******************************************************************
C     CRATLST(I) - ITH CAMAC CRATE NUMBER
C     NCRAT      = NUMBER OF ENTRIES
C
C     ******************************************************************
C     /PACH/ - DEFINITIONS - CONDITIONAL KILL-TABLE FROM $KIL
C     ******************************************************************
C     KILATI(I) - LATCH-WORD INDEX FOR ITH KILL TEST
C     KILMSK(I) - MASK-WORD        FOR ITH KILL TEST
C     KILTYP(I) - KILL-TYPE        FOR ITH KILL TEST 
C                                  1/0 SAYS KILL IF TRUE/FALSE
C     NKIL      = NUMBER OF ENTRIES
C
C     ******************************************************************
C     /PACJ/ - DEFINITIONS FOR FOR ITH GATE
C     ******************************************************************
C     GNAM(M,I),M=1,3 = GATE NAME (ASCII) (RAW OR CALCULATED)
C     GNAM(4,I)       = GATE NAME INDEX
C     GTYP(I)         = GATE TYPE = 'RAW' OR 'CAL'
C     PATN(I)         = ASSOCIATED PATTERN WORD# 
C     GMSK(I)         = ASSOCIATED MASK
C
C     GLO(J)          = GATE LO-LIMIT
C     GHI(J)          = GATE HI-LIMIT
C     LPTR(I)         = POINTER TO AUX LIST, COMMON/PACK/ FOR CAL 
C     RPTR(I)         = POINTER TO READ   LIST (INDEX IN GCNAF)
C
C     MPTR(I)         = POINTER TO MODULE LIST (INDEX IN /PACJ/)
C     GCNAF(M,I),M=1,5= RAW-GATE READ-LIST (C,N,A,F,MOTY)        
C     NENT(I)         = # OF ENTRIES FOR /PACK/ DATA
C
C     NGAT            = TOTAL# (RAW + CAL) GATE ENTRIES
C     NGRED           = # GATE-PARMS TO READ (# ENTRIES IN GCNAF)
C
C     PATNO           = CURRENT PATTERN WORD# 
C     MSKNO           = CURRENT MASK (SPECIFIES BIT TO SET)
C
C     ******************************************************************
C     /PACK/ - DEFINITIONS FOR CALCULATED GATES 
C     ******************************************************************
C     BOOLIST - CONTAINS A BOOLEAN LIST FOR CALCULATED GATES
C     THE FORM IS:
C
C     PATNDX, MASK, NOTCODE, GATNDX, OPCODE, NOTCODE, GATNDX......
C     ENDCODE, PATNDX, MASK, ....
C
C     NBOO            = NO. OF WORDS STORED IN BOOLIST 
C
C     ******************************************************************
C     /PACL/ - DEFINITIONS FOR COUNTDOWN LIST (KTH ENTRY)
C     ******************************************************************
C     CDPAT(K) = ASSOTIATED PATTERN WORD#
C     CDMSK(K) = ASSOCIATED MASK
C     CDCNT(K) = COUNT VALUE
C     NCDN     = NO. OF ENTRIES
C
C     ******************************************************************
C     /PACM/ - DEFINITION OF PAC SOURCE FILE-NAME
C     ******************************************************************
C     NAMPAC(I),I=1,20 - CONTAINS FULL PATH-NAME
C
C     ******************************************************************
C     /PACN/ - DEFINITIONS FOR CAMAC ID & MODULE C,N,TYPE TABLES
C     ******************************************************************
C     CAMC(I)      - ITH CRATE NUMBER
C     CAMN(I)      - ITH SLOT  NUMBER
C     CAMT(I)      - ITH MODULE TYPE CODE
C     CAMID(I,J,K) - ID TABLE FOR (ITH SUBA, JTH SLOT, KTH CRATE)
C     CAMERID      - DEFAULT ERROR ID TO LOAD IN TABLE
C     NCAM         - NUMBER OF ENTRIES
C
C     ******************************************************************
C     /PACP/ - DEFINITIONS FOR DELAY SPECIFICATIONS
C     ******************************************************************
C     DELAZ(I)     - DELAY (MICROSECONDS) FOR MODULE TYPE-I
C     NDELAZ       - NUMBER OF DELAYS SPECIFIED
C
C     ******************************************************************
C     /PACX/ - DEFINITIONS FOR XIA RELATED PARAMETERS
C     ******************************************************************
C     XIAC(I)      - ITH XIA CRATE NUMBER
C     XIAN(I)      - ITH XIA SLOT  NUMBER
C     XIAVSN(I)    - ITH XIA VIRTUAL STATION NUMBER
C     XIAGRP(I)    - ITH XIA GROUP NUMBER
C     NXIA         - NUMBER OF XIA ENTRIES
C     MXXIA        - MAX NUMBER OF XIA ENTRIES ALLOWED
C     ******************************************************************
C     /PACV/ - DEFINITIONS FOR VME (CAEN) RELATED PARAMETERS
C     ******************************************************************
C     VADCMAP(J)   = 1/0 for ADC-J EXISTS/NON-EXISTS
C     VTDCMAP(J)   = 1/0 for TDC-J EXISTS/NON-EXISTS
C     ADCID(I,J)   - CONTAINS ID ASSOCIATED WITH CHANNEL-I & ADC-J
C     TDCID(I,J)   - CONTAINS ID ASSOCIATED WITH CHANNEL-I & TDC-J
C     ******************************************************************
C
      WRITE(LU,90)
   90 FORMAT(1H )
      WRITE(LU,100)
  100 FORMAT(1H ,'COMMON/PAC1/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,90)
      WRITE(LU,200)
  200 FORMAT(1H ,'COMMON/PAC2/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,90)
      WRITE(LU,205)
  205 FORMAT(1H ,'  N  NAMO         NAMI KIMO CRAT SLOT SUBA FRED',
     &                  ' FCLR ACLR DLAT    IDNM MOTY GAIN USED'/)
C
      DO 220 N=1,NUMT
      WRITE(LU,210)N,(NAMO(I,N),I=1,4),KIMO(N),CRAT(N),
     &              SLOT(N),SUBA(N),FRED(N),FCLR(N),ACLR(N),
     &              DLAT(N),IDNM(N),MOTY(1,N),MOTY(2,N),USED(N)
C
  210 FORMAT(1H ,I3,2X,3A4,I5,1X,A4,7I5,Z8,2I5,2X,A3)
  220 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,400)
  400 FORMAT(1H ,'COMMON/PAC4/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,405)
  405 FORMAT(1H ,'    N  LATN      MASK    KINS  TBLI-T  TBLI-F',
     &                        '  TBLN-T  TBLN-F  NXTI-T  NXTI-F',
     &                        '   NUNDX'/)
C
      DO 420 N=1,NTS
      WRITE(LU,410)N,LATN(N),MASK(N),KINS(N),TBLI(1,N),TBLI(2,N),
     &               TBLN(1,N),TBLN(2,N),NXTI(1,N),NXTI(2,N),
     &               NUNDX(N)
C
  410 FORMAT(1H ,I5,I6,Z10,4X,A4,7I8)
  420 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,500)
  500 FORMAT(1H ,'COMMON/PAC5/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,505)
  505 FORMAT(1H ,'     N  LABL  INST  NAMD          INDX      MSKI',
     &                 '  IDES  NREA  ILOR  IHIR'/)
      DO 520 N=1,NCI
      WRITE(LU,510)N,LABL(N),INST(N),(NAMD(I,N),I=1,3),INDX(N),
     &               MSKI(N),IDES(N),NREA(N),ILOR(N),IHIR(N)
C
  510 FORMAT(1H ,I6,2(2X,A4),2X,3A4,2X,I4,Z10,2X,A4,3I6)
  520 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,600)
  600 FORMAT(1H ,'COMMON/PAC6/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,605)
  605 FORMAT(1H ,'     N    LABLIS    LABVAL'/)
C
      DO 620 N=1,NLA
      WRITE(LU,610)N,LABLIS(1,N),LABLIS(2,N),LABVAL(N)
  610 FORMAT(1H ,I6,4X,2A4,I8)
  620 CONTINUE
C
      WRITE(LU,700)
  700 FORMAT(1H ,'COMMON/PAC7/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,705)
  705 FORMAT(1H ,'   ISORL   IEXPL  LISTYP    NERR'/)
      WRITE(LU,710)ISORL,IEXPL,LISTYP,NERR
  710 FORMAT(1H ,2I8,4X,A4,I8)
C
      WRITE(LU,90)
      WRITE(LU,800)
  800 FORMAT(1H ,'COMMON/PAC8/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,805)
  805 FORMAT(1H ,'     N  KLASID  KLASND  KLASMU'/)
      DO 820 N=1,NKLAS
      WRITE(LU,810)N,KLASID(N),KLASND(N),KLASMU(N)
  810 FORMAT(1H ,I6,3I8)
  820 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,900)
  900 FORMAT(1H ,'COMMON/PAC9/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,905)
  905 FORMAT(1H ,'NAMP        (NDX)    LATW    BITN     NBP',
     &                              '       N'/)
      DO 940 N=1,NPAT
      NDX=1
      WRITE(LU,910)(NAMP(I,N),I=1,3),NDX,LATW(1,N),BITN(1,N),NPB(N),N
  910 FORMAT(1H ,3A4,'(',I3,')',4I8)
      JDO=NPB(N)
      DO 930 J=2,JDO
      NDX=NDX+1
      WRITE(LU,910)(NAMP(I,N),I=1,3),NDX,LATW(J,N),BITN(J,N)
  930 CONTINUE
  940 CONTINUE 
C
      WRITE(LU,90)
      WRITE(LU,1000)
 1000 FORMAT(1H ,'COMMON/PACA/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1010)
 1010 FORMAT(1H ,'       I    JCRA    JSLO    JSUB    JFRE    JIDN'/)
      DO 1020 I=1,NNAF
      WRITE(LU,1015)I,JCRA(I),JSLO(I),JSUB(I),JFRE(I),JIDN(I)
 1015 FORMAT(1H ,5I8,Z8)
 1020 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1030)
 1030 FORMAT(1H ,'COMMON/PACB/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
      WRITE(LU,1035)
 1035 FORMAT(1H ,'     CNAFI     DATAI     CNAFQ     DATAQ',
     &           '     CNAFR     DATAR'/)
      NDO=NNAFI
      IF(NNAFQ.GT.NDO) NDO=NNAFQ
      IF(NNAFR.GT.NDO) NDO=NNAFR
      DO 1045 I=1,NDO
      WRITE(LU,1040)CNAFI(I),DATAI(I),CNAFQ(I),DATAQ(I),
     &              CNAFR(I),DATAR(I)
 1040 FORMAT(1H ,6Z10)
 1045 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1050)
 1050 FORMAT(1H ,'COMMON/PACC/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1055)
 1055 FORMAT(1H ,'   RIFLATI   RIFMASK   RIFMOTY     RANGE'/)
      DO 1070 I=1,NRIF
      WRITE(LU,1060)RIFLATI(I),RIFMASK(I),(RIFMOTY(J,I),J=1,2)
 1060 FORMAT(1H ,I10,Z10,2I10)
 1070 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1080)
 1080 FORMAT(1H ,'COMMON/PACD/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1085)
 1085 FORMAT(1H ,'      FERC      FERN      FERT'/)
      DO 1100 I=1,NFER
      WRITE(LU,1090)FERC(I),FERN(I),FERT(I)
 1090 FORMAT(1H ,3I10)
 1100 CONTINUE
      WRITE(LU,90)
      WRITE(LU,1105)
 1105 FORMAT(1H ,'FERID-TABLE (A VS N) (32,576)'/)
      WRITE(LU,1110)FERID
 1110 FORMAT(1H ,16Z6)

C
      WRITE(LU,90)
      WRITE(LU,1120)
 1120 FORMAT(1H ,'COMMON/PACE/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1125)
 1125 FORMAT(1H ,'      FASC      FASN      FAST'/)
      DO 1140 I=1,NFAS
      WRITE(LU,1130)FASC(I),FASN(I),FAST(I)
 1130 FORMAT(1H ,3I10)
 1140 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1150)
 1150 FORMAT(1H ,'FASID TABLE (A VS N) (256,32)'/)
C
      DO 1180 N=1,32
      WRITE(LU,1160)(FASID(I,N),I=1,256)
 1160 FORMAT(1H ,16Z6)
      WRITE(LU,90)
 1180 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1190)
 1190 FORMAT(1H ,'COMMON/PACF/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1200)
 1200 FORMAT(1H ,'      CLRC      CLRN      CLRA      CLRF'/)
      DO 1210 I=1,NCLR
      WRITE(LU,1205)CLRC(I),CLRN(I),CLRA(I),CLRF(I)
 1205 FORMAT(1H ,4I10)
 1210 CONTINUE
C
C
      WRITE(LU,90)
      WRITE(LU,1220)
 1220 FORMAT(1H ,'COMMON/PACG/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1225)
 1225 FORMAT(1H ,'   CRATLST'/)
      DO 1240 I=1,NCRAT
      WRITE(LU,1230)CRATLST(I)
 1230 FORMAT(1H ,I10)
 1240 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1245)
 1245 FORMAT(1H ,'COMMON/PACH/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1250)
 1250 FORMAT(1H ,'    KILATI    KILMSK    KILTYP'/)
      DO 1260 I=1,NKIL
      WRITE(LU,1255)KILATI(I),KILMSK(I),KILTYP(I)
 1255 FORMAT(1H ,I10,Z10,I10)
 1260 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1265)
 1265 FORMAT(1H ,'COMMON/PACJ/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1270)
 1270 FORMAT(1H ,'GNAM          GNDX  GTYP  PATN     GMSK   GLO',
     &           '   GHI  LPTR  RPTR  MPTR  NENT'/)
C
      DO 1280 J=1,NGAT
      WRITE(LU,1275)(GNAM(I,J),I=1,4),GTYP(J),PATN(J),GMSK(J),GLO(J),
     &               GHI(J),LPTR(J),RPTR(J),MPTR(J),NENT(J)
 1275 FORMAT(1H ,3A4,I6,3X,A3,I6,Z9,6I6)
 1280 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1285)
 1285 FORMAT(1H ,'  CRAT  SLOT  SUBA  FRED  MTYP'/)
      DO 1295 J=1,NGRED
      WRITE(LU,1290)(GCNAF(I,J),I=1,5)
 1290 FORMAT(1H ,5I6)
 1295 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1300)NGAT,NGRED
 1300 FORMAT(1H ,'NGAT,NGRED =',2I6)
C
      WRITE(LU,90)
      WRITE(LU,1305)
 1305 FORMAT(1H ,'COMMON/PACK/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      J=0
 1310 J=J+1
      IF(J.GT.NBOO) GO TO 1400
      WRITE(LU,1315)BOOLIST(J)
 1315 FORMAT(1H ,'PATNDX =',I8)
      J=J+1
      WRITE(LU,1320)BOOLIST(J)
 1320 FORMAT(1H ,'MASK   =',Z8)
      J=J+1
      WRITE(LU,1325)BOOLIST(J)
 1325 FORMAT(1H ,'NOTCOD =',I8)
      J=J+1
      WRITE(LU,1330)BOOLIST(J)
 1330 FORMAT(1H ,'GATNDX =',I8)
C
 1350 J=J+1
      IF(BOOLIST(J).EQ.-1) THEN
      WRITE(LU,1355)BOOLIST(J)
 1355 FORMAT(1H ,'ENDCODE=',I8)
      GO TO 1310
      ENDIF
C
      WRITE(LU,1360)BOOLIST(J)
 1360 FORMAT(1H ,'OPCODE =',I8)
      J=J+1
      WRITE(LU,1365)BOOLIST(J)
 1365 FORMAT(1H ,'NOTCOD =',I8)
      J=J+1
      WRITE(LU,1370)BOOLIST(J)
 1370 FORMAT(1H ,'GATNDX =',I8)
      GO TO 1350
C
 1400 WRITE(LU,90)
      WRITE(LU,1405)
 1405 FORMAT(1H ,'COMMON/PACL/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1410)
 1410 FORMAT(1H ,'   CDPAT   CDMSK   CDCNT'/)
      DO 1420 J=1,NCDN
      WRITE(LU,1415)CDPAT(J),CDMSK(J),CDCNT(J)
 1415 FORMAT(1H ,I8,Z8,I8)
 1420 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1425)
 1425 FORMAT(1H ,'COMMON/PACM/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1430)
 1430 FORMAT(1H ,'SOURCE FILE NAME IN ALL ITS GLORY:'/)
      WRITE(LU,1435)NAMPAC
 1435 FORMAT(1H ,20A4)
C
      WRITE(LU,90)
      WRITE(LU,1440)
 1440 FORMAT(1H ,'COMMON/PACN/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1445)
 1445 FORMAT(1H ,'      CAMC      CAMN      CAMT'/)
      DO 1460 I=1,NCAM
      WRITE(LU,1450)CAMC(I),CAMN(I),CAMT(I)
 1450 FORMAT(1H ,3I10)
 1460 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1465)
 1465 FORMAT(1H ,'CAMID TABLE (A VS N VS C) (32,32,8)'/)
C
      WRITE(LU,1470)CAMID
 1470 FORMAT(1H ,16Z6)
C
      WRITE(LU,90)
      WRITE(LU,1475)
 1475 FORMAT(1H ,'COMMON/PACP/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1480)
 1480 FORMAT(1H ,' DIR-ENTRY     DELAY'/)
      DO 1490 I=1,NDELAZ
      IF(DELAZ(I).EQ.0) GO TO 1490
      WRITE(LU,1485)I,DELAZ(I)
 1485 FORMAT(1H ,2I10)
 1490 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1505)
 1505 FORMAT(1H ,'COMMON/PACX/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1510)
 1510 FORMAT(1H ,'      XIAC      XIAN    XIAVSN    XIAGRP'/)
      DO 1520 I=1,NXIA
      WRITE(LU,1515)XIAC(I),XIAN(I),XIAVSN(I),XIAGRP(I)
 1515 FORMAT(1H ,4I10)
 1520 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1605)
 1605 FORMAT(1H ,'COMMON/PACV/ DEFINITIONS *****************',
     &                       '******************************',
     &                       '*****'/)
C
      WRITE(LU,1610)
 1610 FORMAT(1H ,'    NUMBER   VADCMAP   VTDCMAP   VQDCMAP'/)
      DO 1620 I=1,12
      WRITE(LU,1615)I,VADCMAP(I),VTDCMAP(I),VQDCMAP(I)
 1615 FORMAT(1H ,4I10)
 1620 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1630)
      WRITE(LU,1635)
 1630 FORMAT(1H ,'ADC#=      1      2      3      4      5      6',
     &                '      7      8      9     10     11     12')
 1635 FORMAT(1H ,'CHAN#')
      DO 1650 I=1,34
      WRITE(LU,1640)I,(ADCID(I,J),J=1,12)
 1640 FORMAT(1H ,I5,12Z7)
 1650 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1660)
      WRITE(LU,1665)
 1660 FORMAT(1H ,'TDC#=      1      2      3      4      5      6',
     &                '      7      8      9     10     11     12')
 1665 FORMAT(1H ,'CHAN#')
      DO 1680 I=1,34
      WRITE(LU,1670)I,(TDCID(I,J),J=1,12)
 1670 FORMAT(1H ,I5,12Z7)
 1680 CONTINUE
C
      WRITE(LU,90)
      WRITE(LU,1690)
      WRITE(LU,1695)
 1690 FORMAT(1H ,'QDC#=      1      2      3      4      5      6',
     &                '      7      8      9     10     11     12')
 1695 FORMAT(1H ,'CHAN#')
      DO 1699 I=1,34
      WRITE(LU,1698)I,(QDCID(I,J),J=1,12)
 1698 FORMAT(1H ,I5,12Z7)
 1699 CONTINUE
C
      RETURN
      END
