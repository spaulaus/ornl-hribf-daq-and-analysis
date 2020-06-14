C$PROG POBGEN
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ************************************************************
C
      SUBROUTINE POBGEN(LU,KLIS)
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
      COMMON/PACI/ POB(65536),NPOB
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
C
      COMMON/PACO/ KODLO(4),KODHI(4)
C
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
      COMMON/KLOC/ KLOCID(2)
      INTEGER*4    KLOCID
C     ------------------------------------------------------------------
C
      INTEGER*4 MOLIST(20)
C
      INTEGER*4 DIR(3,33)
C
      BYTE BITE(4)
C
      EQUIVALENCE (WORD,BITE)
C
      EQUIVALENCE (DIR,POB)
C
      CHARACTER*4  KIMO,GTYP,USED,KLIS
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     ******************************************************************
C     BUILD THE FINAL TABLES FOR FRONT-END (OBJECT TABLES) 
C     ******************************************************************
C
C     ******************************************************************
C     DIR(I,1)  - LOCATES PAC SOURCE FILENAME
C     DIR(I,2)  - LOCATES CAMAC CRATE TABLE
C     DIR(I,3)  - LOCATES CNAF-INIT LIST
C     DIR(I,4)  - LOCATES CAMAC   MODULE TABLE
C     DIR(I,5)  - LOCATES FASTBUS MODULE TABLE
C     DIR(I,6)  - LOCATES FERA    MODULE TABLE
C     DIR(I,7)  - LOCATES GATED LATCH   TABLE
C     DIR(I,8)  - LOCATES GATE  READ    TABLE
C     DIR(I,9)  - LOCATES RAW GATE SPEC TABLE
C     DIR(I,10) - LOCATES CAL GATE SPEC TABLE
C     DIR(I,11) - LOCATES COUNT DOWN    TABLE
C     DIR(I,12) - LOCATES CONDITIIONAL  KILL TABLE
C     DIR(I,13) - LOCATES UNCONDITIONAL READOUT TABLE
C     DIR(I,14) - LOCATES CONDITIOINAL  READOUT PROGRAM
C     DIR(I,15) - LOCATES CNAF-LIST FOR CONDITIONAL READOUT
C     DIR(I,16) - LOCATES ID-LIST   FOR CONDITIONAL READOUT
C     DIR(I,17) - LOCATES CAMAC   MODULE-TYPE READOUT LIST
C     DIR(I,18) - LOCATES FASTBUS MODULE-TYPE READOUT LIST
C     DIR(I,19) - LOCATES FERA    MODULE-TYPE READOUT LIST
C     DIR(I,20) - LOCATES CAMAC   ID-TABLE
C     DIR(I,21) - LOCATES FASTBUS ID-TABLE
C     DIR(I,22) - LOCATES FERA    ID-TABLE
C     DIR(I,23) - LOCATES WINDUP  CNAF LIST
C     DIR(I,24) - LOCATES RUN     CNAF LIST
C     DIR(I,25) - LOCATES XIA     MODULE TABLE
C     DIR(I,26) - LOCATES VME CONDITIONAL READOUT TABLE
C     DIR(I,27) - LOCATES CAEN ADC HARDWARE MAP
C     DIR(I,28) - LOCATES CAEN TDC HARDWARE MAP
C     DIR(I,29) - LOCATES CAEN QDC HARDWARE MAP
C     DIR(I,30) - LOCATES CAEN ADC ID-TABLE
C     DIR(I,31) - LOCATES CAEN TDC ID-TABLE
C     DIR(I,32) - LOCATES CAEN QDC ID-TABLE
C     DIR(I,33) - LOCATES 100HZ clock IDs (2 required)
C     *************************************************************
C
      DO 20 J=1,33
      DO 10 I=1,3
      DIR(I,J)=0
   10 CONTINUE
   20 CONTINUE
C
      IOF=100
C
C     01-***************************(01-03)**!PAC FILENAME*********
C
  100 JD=1                                   !DIRECTORY#
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=20                           !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 110 I=1,20
      POB(IOF+I)=NAMPAC(I)                   !SOURCE FILE NAME
  110 CONTINUE
      IOF=IOF+20
C
C     02-***************************(04-06)**!CRATE-LIST***********
C
  200 IF(NCRAT.LE.0) GO TO 300
C
      JD=2
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NCRAT                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 210 I=1,NCRAT
      POB(IOF+I)=CRATLST(I)
  210 CONTINUE
      IOF=IOF+NCRAT
C
C     03-***************************(07-09)**!CNAF-INIT LIST*******
C
  300 IF(NNAFI.LE.0) GO TO 400
C
      JD=3
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAFI                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 310 I=1,NNAFI
      POB(IOF+N+1)=CNAFI(I)
      POB(IOF+N+2)=DATAI(I)
      N=N+2
  310 CONTINUE
      IOF=IOF+2*NNAFI
C
C     04-***************************(10-12)**!CAMAC   MODULE TABLE
C
  400 IF(NCAM.LE.0) GO TO 500
C
      JD=4
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NCAM                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 410 I=1,NCAM
      BITE(1)=CAMT(I)                        !MODULE-TYPE
      BITE(2)=0                              !DUMMY
      BITE(3)=CAMN(I)                        !SLOT#
      BITE(4)=CAMC(I)                        !CRATE#
      POB(IOF+I)=WORD
  410 CONTINUE
      IOF=IOF+NCAM
C
C     05-***************************(13-15)**!FASTBUS MODULE TABLE
C
  500 IF(NFAS.LE.0) GO TO 600
C
      JD=5
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NFAS                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 510 I=1,NFAS
      BITE(1)=FAST(I)                        !MODULE-TYPE
      BITE(2)=0                              !DUMMY
      BITE(3)=FASN(I)                        !SLOT#
      BITE(4)=FASC(I)                        !CRATE#
      POB(IOF+I)=WORD
  510 CONTINUE
      IOF=IOF+NFAS
C
C     06-***************************(16-18)**!FERA MODULE TABLE****
C
  600 IF(NFER.LE.0) GO TO 700
C
      JD=6
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NFER                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 610 I=1,NFER
      BITE(1)=FERT(I)                        !MODULE-TYPE
      BITE(2)=0                              !DUMMY
      BITE(3)=FERN(I)                        !SLOT#
      BITE(4)=FERC(I)                        !CRATE#
      POB(IOF+I)=WORD
  610 CONTINUE
      IOF=IOF+NFER
C
C     07-***************************(19-21)**!GATED-LATCH TABLE****
C
  700 NLAT=0                                 !INIT LATCH COUNTER
      DO 710 I=1,NUMT                        !LOOP ON HWD TABLE
      IF(KIMO(I).NE.'$LAT') GO TO 710
      NLAT=NLAT+1
      BITE(1)=FRED(I)
      BITE(2)=SUBA(I)
      BITE(3)=SLOT(I)
      BITE(4)=CRAT(I)
      POB(IOF+NLAT)=WORD
  710 CONTINUE
C
      IF(NLAT.LE.0) GO TO 800
C
      JD=7
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NLAT                         !#ENTRIES
      DIR(3,JD)=DELAZ(7)                     !DELAY
      IOF=IOF+NLAT
C
C     08-***************************(22-24)**!GATE READ TABLE
C
  800 IF(NGRED.LE.0) GO TO 900
C
      JD=8                                   !DIRECTORY#
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NGRED                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY?????????
      N=0
      DO 820 J=1,NGRED
      DO 810 I=1,5
      N=N+1
      POB(IOF+N)=GCNAF(I,J)
  810 CONTINUE
  820 CONTINUE
      IOF=IOF+N
C
C     09-***************************(25-27)**!RAW GATE SPEC TABLE
C
  900 IF(NGAT.LE.0) GO TO 1000
C
      JD=9                                   !DIRECTORY#
      DIR(1,JD)=IOF
      DIR(3,JD)=0                            !DELAY
      N=0
      NRAW=0
      DO 910 I=1,NUMT
      IF(GTYP(I).NE.'RAW ') GO TO 910
      POB(IOF+N+1)=RPTR(I)
      POB(IOF+N+2)=GLO(I)
      POB(IOF+N+3)=GHI(I)
      POB(IOF+N+4)=PATN(I)+1                 !START WITH LAT-NDX 2
      POB(IOF+N+5)=GMSK(I)
      N=N+5
      NRAW=NRAW+1
  910 CONTINUE
      DIR(2,JD)=NRAW                         !#ENTRIES
      IOF=IOF+N

C
C     10-***************************(28-30)**!CAL GATE SPEC TABLE
C
 1000 IF(NBOO.LE.0) GO TO 1100
C
      JD=10                                  !DIRECTORY#
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NBOO                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 1010 I=1,NBOO
      POB(IOF+I)=BOOLIST(I)
 1010 CONTINUE
      IOF=IOF+NBOO
C
C     11-***************************(31-33)**!COUNT DOWN TABLE
C
 1100 IF(NCDN.LE.0) GO TO 1200
C
      JD=11
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NCDN                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 1110 I=1,NCDN
      POB(IOF+N+1)=CDPAT(I)+1                !START WITH LAT-NDX 2
      POB(IOF+N+2)=CDMSK(I)
      POB(IOF+N+3)=CDCNT(I)
      N=N+3
 1110 CONTINUE
      IOF=IOF+3*NCDN
C
C     12-***************************(34-36)**!CONDITIONAL KILL TABL
C
 1200 IF(NKIL.LE.0) GO TO 1300
C
      JD=12
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NKIL                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 1210 I=1,NKIL
      POB(IOF+N+1)=KILATI(I)+1               !START WITH LAT-NDX 2
      POB(IOF+N+2)=KILMSK(I)
      POB(IOF+N+3)=KILTYP(I)
      N=N+3
 1210 CONTINUE
      IOF=IOF+3*NKIL
C
C     13-***************************(37-39)**!UN-COND READOUT TABLE
C
 1300 N=0                                    !INIT WORD COUNTER
      DO 1320 I=1,NUMT
C
      IF(USED(I).EQ.'YES ') GO TO 1320
      IF(IDNM(I).LE.0)      GO TO 1320
C
      IT=MOTY(1,I)
      IF(IT.GE.KODLO(3).AND.IT.LE.KODHI(3)) GO TO 1320
C
      IF(KIMO(I).EQ.'$CAM') GO TO 1310
      IF(KIMO(I).EQ.'$LAT') GO TO 1310
      GO TO 1320
C
 1310 BITE(1)=FRED(I)
      BITE(2)=SUBA(I)
      BITE(3)=SLOT(I)
      BITE(4)=CRAT(I)
      N=N+1
      POB(IOF+N)=WORD
      N=N+1
      POB(IOF+N)=IDNM(I)
 1320 CONTINUE
C
      IF(N.LE.0) GO TO 1400
C
      JD=13
      DIR(1,JD)=IOF
      DIR(2,JD)=N/2                          !#ENTRIES
      DIR(3,JD)=DELAZ(13)                    !DELAY
      IOF=IOF+N
C
C     14-***************************(40-42)**!COND-CAMAC PROGRAM***
C
 1400 IF(NTS.LE.0) GO TO 1500
C
      JD=14
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NTS+1                        !#ENTRIES
      DO 1410 I=1,NUMT                       !LOOP TO FIND MAX DELAY
      IF(KIMO(I).NE.'$CAM') GO TO 1410
      IF(USED(I).NE.'YES ') GO TO 1410
 1410 CONTINUE
      DIR(3,JD)=DELAZ(14)                    !DELAY
      N=0                                    !INIT WORD COUNTER
      DO 1420 I=1,NTS                        !LOOP ON # ENTRIES
      POB(IOF+N+1)=LATN(I)+1                 !START AT LAT-INDEX 2
      POB(IOF+N+2)=MASK(I)
      POB(IOF+N+3)=TBLI(1,I)
      POB(IOF+N+4)=TBLN(1,I)
      POB(IOF+N+5)=NXTI(1,I)
      POB(IOF+N+6)=TBLI(2,I)
      POB(IOF+N+7)=TBLN(2,I)
      POB(IOF+N+8)=NXTI(2,I)
      N=N+8
 1420 CONTINUE
      DO 1430 I=1,8                          !NULL ENTRY FOR END
      POB(IOF+N+I)=0
 1430 CONTINUE
      IOF=IOF+8*(NTS+1)
C
C     15-***************************(43-45)**!COND-CAMAC CNAF-LIST*
C
 1500 IF(NNAF.LE.0) GO TO 1600
C
      JD=15
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAF                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 1510 I=1,NNAF
      BITE(1)=JFRE(I)
      BITE(2)=JSUB(I)
      BITE(3)=JSLO(I)
      BITE(4)=JCRA(I)
      POB(IOF+I)=WORD
 1510 CONTINUE
      IOF=IOF+NNAF
C
C     16-***************************(46-48)**!COND-CAMAC ID-LIST***
C
 1600 IF(NNAF.LE.0) GO TO 1700
C
      JD=16
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAF                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 1610 I=1,NNAF
      POB(IOF+I)=JIDN(I)
 1610 CONTINUE
      IOF=IOF+NNAF
C
C     17-***************************(49-51)**!CAMAC   MODULE READ TABLE
C
 1700 N=0
      DO 1710 I=1,NRIF
      IT=RIFMOTY(1,I)
      IF(IT.LT.KODLO(3).OR.IT.GT.KODHI(3)) GO TO 1710
      POB(IOF+N+1)=RIFLATI(I)+1              !START WITH LAT-NDX 2
      POB(IOF+N+2)=RIFMASK(I)
      POB(IOF+N+3)=RIFMOTY(1,I)
      N=N+3
 1710 CONTINUE
      CALL MODULY('$CAM',MOLIST,NUM)
      DO 1720 I=1,NUM
      POB(IOF+N+1)=1                         !LAT-WD (ALL BITS SET)
      POB(IOF+N+2)=-1                        !MASK   (ALL BITS SET)
      POB(IOF+N+3)=MOLIST(I)
      N=N+3
 1720 CONTINUE
      NCAMA=N/3
C
      IF(N.LE.0) GO TO 1800
C
      JD=17
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N/3                          !#ENTRIES
      DIR(3,JD)=DELAZ(17)                    !DELAY
      IOF=IOF+N
C
C     18-***************************(52-54)**!FASTBUS MODULE READ TABLE
C
 1800 N=0
      DO 1810 I=1,NRIF
      IT=RIFMOTY(1,I)
      IF(IT.LT.KODLO(2).OR.IT.GT.KODHI(2)) GO TO 1810
      POB(IOF+N+1)=RIFLATI(I)+1              !START WITH LAT-NDX 2
      POB(IOF+N+2)=RIFMASK(I)
      POB(IOF+N+3)=RIFMOTY(1,I)
      N=N+3
 1810 CONTINUE
      CALL MODULY('$FAS',MOLIST,NUM)
      DO 1820 I=1,NUM
      POB(IOF+N+1)=1                         !LAT-WD (ALL BITS SET)
      POB(IOF+N+2)=-1                        !MASK   (ALL BITS SET)
      POB(IOF+N+3)=MOLIST(I)
      N=N+3
 1820 CONTINUE
      NFAST=N/3
C
      IF(N.LE.0) GO TO 1900
C
      JD=18
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N/3                          !#ENTRIES
      DIR(3,JD)=DELAZ(18)                    !DELAY
      IOF=IOF+N
C
C     19-***************************(55-57)**!FERA    MODULE READ TABLE
C
 1900 N=0
      DO 1910 I=1,NRIF
      IT=RIFMOTY(1,I)
      IF(IT.LT.KODLO(1).OR.IT.GT.KODHI(1)) GO TO 1910
      POB(IOF+N+1)=RIFLATI(I)+1              !START WITH LAT-NDX 2
      POB(IOF+N+2)=RIFMASK(I)
      POB(IOF+N+3)=RIFMOTY(1,I)
      N=N+3
 1910 CONTINUE
      CALL MODULY('$FER',MOLIST,NUM)
      DO 1920 I=1,NUM
      POB(IOF+N+1)=1                         !LAT-WD (ALL BITS SET)
      POB(IOF+N+2)=-1                        !MASK   (ALL BITS SET)
      POB(IOF+N+3)=MOLIST(I)
      N=N+3
 1920 CONTINUE
      NFERA=N/3
C
      IF(N.LE.0) GO TO 2000
C
      JD=19
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N/3                          !#ENTRIES
      DIR(3,JD)=DELAZ(19)                    !DELAY
      IOF=IOF+N
C
C     20-***************************(58-60)**!CAMAC   ID TABLE
C
 2000 IF(NCAMA.LE.0) GO TO 2100
C
      JD=20
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=8192                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2030 K=1,8
      DO 2020 J=1,32
      DO 2010 I=1,32
      N=N+1
      POB(IOF+N)=CAMID(I,J,K)    
 2010 CONTINUE
 2020 CONTINUE
 2030 CONTINUE
      IOF=IOF+N
C
C     21-***************************(61-63)**!FASTBUS ID-TABLE*****
C
 2100 IF(NFAST.LE.0) GO TO 2200
C
      JD=21
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=8192                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2120 J=1,32
      DO 2110 I=1,256
      N=N+1
      POB(IOF+N)=FASID(I,J)    
 2110 CONTINUE
 2120 CONTINUE
      IOF=IOF+N
C
C     22-***************************(64-66)**!FERA ID-TABLE********
C
 2200 IF(NFERA.LE.0) GO TO 2300
C
      JD=22
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=18432                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2220 J=1,576
      DO 2210 I=1,32
      N=N+1
      POB(IOF+N)=FERID(I,J)
 2210 CONTINUE
 2220 CONTINUE
      IOF=IOF+N
C
C     23-***************************(67-69)**!WINDUP/CLR CNAF TABLE
C
 2300 IF((NNAFQ+NCLR).LE.0) GO TO 2400
C
      JD=23
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAFQ+NCLR                   !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2310 I=1,NNAFQ
      N=N+1
      POB(IOF+N)=CNAFQ(I)
      N=N+1
      POB(IOF+N)=DATAQ(I)
 2310 CONTINUE
      DO 2320 I=1,NCLR
      N=N+1
      BITE(1)=CLRF(I)
      BITE(2)=CLRA(I)
      BITE(3)=CLRN(I)
      BITE(4)=CLRC(I)
      POB(IOF+N)=WORD
      N=N+1
      POB(IOF+N)=0
 2320 CONTINUE
      IOF=IOF+N
C
C     24-***************************(70-72)**!RUN CNAF TABLE
C
 2400 IF(NNAFR.LE.0) GO TO 2500
C
      JD=24
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NNAFR                        !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      N=0
      DO 2410 I=1,NNAFR
      N=N+1
      POB(IOF+N)=CNAFR(I)
      N=N+1
      POB(IOF+N)=DATAR(I)
 2410 CONTINUE
      IOF=IOF+N
C
C     25-***************************(73-75)**!XIA MODULE TABLE*****
C
 2500 IF(NXIA.LE.0) GO TO 2600
C
      JD=25
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=NXIA                         !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      DO 2510 I=1,NXIA
      BITE(1)=XIAGRP(I)                      !GROUP# (optinal)
      BITE(2)=XIAVSN(I)                      !VERTUAL STATION#
      BITE(3)=XIAN(I)                        !SLOT#
      BITE(4)=XIAC(I)                        !CRATE#
      POB(IOF+I)=WORD
 2510 CONTINUE
      IOF=IOF+NXIA
C
C     26-***************************(76-78)**!VME MODULE READ TABLE*****
C
 2600 N=0
      DO 2610 I=1,NRIF
      IT=RIFMOTY(1,I)
      IF(IT.LT.KODLO(4).OR.IT.GT.KODHI(4)) GO TO 2610
      POB(IOF+N+1)=RIFLATI(I)+1              !START WITH LAT-NDX 2
      POB(IOF+N+2)=RIFMASK(I)
      POB(IOF+N+3)=RIFMOTY(1,I)
      N=N+3
 2610 CONTINUE
      CALL MODULY('$VME',MOLIST,NUM)
      DO 2620 I=1,NUM
      POB(IOF+N+1)=1                         !LAT-WD (ALL BITS SET)
      POB(IOF+N+2)=-1                        !MASK   (ALL BITS SET)
      POB(IOF+N+3)=MOLIST(I)
      N=N+3
 2620 CONTINUE
      NVME=N/3
C
      IF(NVME.LE.0) GO TO 2700
C
      JD=26
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N/3                          !#ENTRIES
      DIR(3,JD)=DELAZ(26)                    !DELAY
      IOF=IOF+N
C
C     27-***************************(79-81)**!CAEN ADC HARDWARE MAP*****
C
 2700 N=0
      NADC=0
      DO 2710 I=1,12
      N=N+1
      POB(IOF+N)=VADCMAP(I)
      IF(VADCMAP(I).GT.0) NADC=NADC+1
 2710 CONTINUE
      IF(NADC.LE.0) GO TO 2800
C
      JD=27
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     28-***************************(82-84)**!CAEN TDC HARDWARE MAP*****
C
 2800 N=0
      NTDC=0
      DO 2810 I=1,12
      N=N+1
      POB(IOF+N)=VTDCMAP(I)
      IF(VTDCMAP(I).GT.0) NTDC=NTDC+1
 2810 CONTINUE
      IF(NTDC.LE.0) GO TO 2900
C
      JD=28
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     29-***************************(85-87)**!CAEN QDC HARDWARE MAP*****
C
 2900 N=0
      NQDC=0
      DO 2910 I=1,12
      N=N+1
      POB(IOF+N)=VQDCMAP(I)
      IF(VQDCMAP(I).GT.0) NQDC=NQDC+1
 2910 CONTINUE
      IF(NQDC.LE.0) GO TO 3000
C
      JD=29
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     30-***************************(88-90)**!CAEN ADC     ID-TABLE*****
C
 3000 IF(NADC.LE.0) GO TO 3100
      N=0
      DO 3010 I=1,408
      N=N+1
      POB(IOF+N)=VADCID(I)
 3010 CONTINUE
C
      JD=30
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     31-***************************(91-93)**!CAEN TDC     ID-TABLE*****
C
 3100 IF(NTDC.LE.0) GO TO 3200
      N=0
      DO 3110 I=1,408
      N=N+1
      POB(IOF+N)=VTDCID(I)
 3110 CONTINUE
C
      JD=31
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     32-***************************(94-96)**!CAEN QDC     ID-TABLE*****
C
 3200 IF(NQDC.LE.0) GO TO 3300
      N=0
      DO 3210 I=1,408
      N=N+1
      POB(IOF+N)=VQDCID(I)
 3210 CONTINUE
C
      JD=32
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=N                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C     33-***************************(97-99)**!100HZ CLOCK  ID-TABLE*****
C
 3300 IF(KLOCID(1).EQ.0) GO TO 5000
      N=0
      DO 3310 I=1,2
      N=N+1
      POB(IOF+N)=KLOCID(I)
 3310 CONTINUE
C
      JD=33
      DIR(1,JD)=IOF                          !POB OFFSET
      DIR(2,JD)=2                            !#ENTRIES
      DIR(3,JD)=0                            !DELAY
      IOF=IOF+N
C
C
 5000 NPOB=IOF
      IF(KLIS.EQ.'X   ') WRITE(LU,REC=1)(POB(I),I=1,IOF)
      NBWRIT=4*NPOB
      WRITE(6,5005)NBWRIT
 5005 FORMAT(1H ,'#BYTES OBJECT CODE GENERATED =',I8)
      RETURN
      END
