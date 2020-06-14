C$PROG CONTAB
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CONTAB
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXI=2000)
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C
      COMMON/PAC6/ LABLIS(2,MXI),LABVAL(MXI),NLA
C 
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PAC4/ LATN(MXI),MASK(MXI),KINS(MXI),
     &             TBLI(2,MXI),TBLN(2,MXI),NXTI(2,MXI),
     &             NUNDX(MXI),NTS
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      CHARACTER*4  INST,KINS,IT
C
      SAVE
C
C     ************************************************************
C     CONSTRUCTS FINAL "CONDITIONAL READOUT TABLES"
C     ************************************************************
C
C     ---------------------------------------------------------------------
C     LABL(J)    = JTH STATEMENT LABEL 
C     INST(J)    = JTH INSTRUCTION - IFU,IFS,IFA,IFN,CONT,GOTO
C     NAMD(J)    = JTH DEVICE NAME - ADC1, TDC2, ETC
C     INDX(J)    = JTH DEVICE NAME INDEX
C     MSKI(J)    = JTH MASK FOR IF-TESTS
C     IDES(J)    = JTH DESTINATION (LABEL)
C     NREA(J)    = JTH READOUT LIST POINTER  - ASSOCIATED WITH "READ"
C     ILOR(J)    = JTH READOUT LIST LO-LIMIT - ASSOCIATED WITH "CONDITION"
C     IHIR(J)    = JTH READOUT LIST HI-LIMIT - ASSOCIATED WITH "CONDITION"
C     NCI        = NUMBER OF ENTRIES IN THIS INTERMEDIATE READOUT TABLE
C     ---------------------------------------------------------------------
C     LABLIS(1,J) = 1ST 4 BYTES OF JTH STATEMENT LABEL (ASCII)
C     LABLIS(2,J) = 2ND 4 BYTES OF JTH STATEMENT LABEL (ASCII)
C     LABVAL(J)   = VALUE OF       JTH STATEMENT LABEL (INTEGER)
C     ---------------------------------------------------------------------
C     LATN(J)    = LATCH-WORD INDEX FOR JTH TEST
C     MASK(J)    = MASK FOR USE IN      JTH TEST
C
C     TBLI(I,J)  = POINTS TO NAF-LIST (/PACA/)- I=1,2 for TRUE/FALSE 
C     TBLN(I,J)  = # NAFs TO TAKE FROM TABLE  - I=1,2 for TRUE/FALSE
C     NXTI(I,J)  = NEXT TEST-TABLE INDEX      - I=1,2 for TRUE/FALSE
C
C     NTS        = NUMBER OF TEST-TABLE ENTRIES TO USE
C     --------------------------------------------------------------------- 
C
      NTS=0
      N=0
C
  100 N=N+1
      IF(N.GT.NCI) GO TO 1000
      ISORL=JSORL(N)
      IEXPL=JEXPL(N)
      IT=INST(N)
      KINS(NTS+1)=IT
C
      IF(NTS.GT.MXI-1) THEN
      WRITE(CMSSG,105)NTS+1
  105 FORMAT('/PAC4/ ARRAY OVERFLOW AT NTS =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NTS=MXI-1
                       ENDIF
C
      IF(IT.EQ.'CONT') GO TO 200
      IF(IT.EQ.'GOTO') GO TO 220
      IF(IT.EQ.'IFN ') GO TO 240
      IF(IT.EQ.'IFA ') GO TO 260
      IF(IT.EQ.'IFU ') GO TO 280
      IF(IT.EQ.'IFS ') GO TO 300
      IF(IT.EQ.'IFF ') GO TO 320
      IF(IT.EQ.'IFT ') GO TO 340
                       GO TO 100
C
  200 NTS=NTS+1                              !CONTINUE ***************
      LATN(NTS)=0                            !LATCH-WD-0 PRESET
      MASK(NTS)=-1                           !MASK=-1
      TBLI(1,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=ILOR(N)                    !LO CAMAC-LST-PRT - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS 
      TBLN(1,NTS)=NR                         !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=NR                         !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=NTS+1                      !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NTS+1                      !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  220 NTS=NTS+1                              !GOTO *******************
      LATN(NTS)=1                            !LATCH-WD-1 PRESET
      MASK(NTS)=0                            !MASK=0
      TBLI(1,NTS)=0                          !NO CAMAC LIST    - TRUE
      TBLI(2,NTS)=0                          !NO CAMAC LIST    - FALSE
      TBLN(1,NTS)=0                          !#  CAMAC OPS=0   - TRUE
      TBLN(2,NTS)=0                          !#  CAMAC OPS=0   - FALSE
      NXTI(1,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NXTI(1,NTS)                !NEST TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  240 NTS=NTS+1                              !IFN ********************
      LATN(NTS)=LATWDX(NAMD(1,N),INDX(N))    !LATWD# FROM NAMD(INDX)
      MASK(NTS)=MSKI(N)                      !MASK
      TBLI(1,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=0                          !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=NR                         !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=0                          !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=NTS+1                      !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  260 NTS=NTS+1                              !IFA ********************
      LATN(NTS)=LATWDX(NAMD(1,N),INDX(N))    !LATWD# FROM NAMD(INDX)
      MASK(NTS)=MSKI(N)                      !MASK
      TBLI(1,NTS)=0                          !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=0                          !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=NR                         !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NTS+1                      !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  280 NTS=NTS+1                              !IFU ********************
      CALL PATMSK(NAMD(1,N),INDX(N),         !FROM NAME & INDEX, GET
     &            LATN(NTS),MASK(NTS))       !LATCH-WD# & MASK
      TBLI(1,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=0                          !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=NR                         !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=0                          !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=NTS+1                      !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  300 NTS=NTS+1                              !IFS ********************
      CALL PATMSK(NAMD(1,N),INDX(N),         !FROM NAME & INDEX, GET
     &            LATN(NTS),MASK(NTS))       !LATCH-WD# & MASK
      TBLI(1,NTS)=0                          !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=0                          !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=NR                         !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NTS+1                      !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  320 NTS=NTS+1                              !IFF ********************
      CALL GATMSK(NAMD(1,N),INDX(N),         !FROM NAME & INDEX, GET
     &            LATN(NTS),MASK(NTS))       !LATCH-WD# & MASK
      TBLI(1,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=0                          !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=NR                         !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=0                          !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=NTS+1                      !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
  340 NTS=NTS+1                              !IFT ********************
      CALL GATMSK(NAMD(1,N),INDX(N),         !FROM NAME & INDEX, GET
     &            LATN(NTS),MASK(NTS))       !LATCH-WD# & MASK
      TBLI(1,NTS)=0                          !LO CAMAC-LST-PTR - TRUE
      TBLI(2,NTS)=ILOR(N)                    !LO CAMAC-LST-PTR - FALSE
      NR=0                                   !# CAMAC OPS
      IF(IHIR(N).GT.0) NR=IHIR(N)-ILOR(N)+1  !# CAMAC OPS
      TBLN(1,NTS)=0                          !# CAMAC OPS      - TRUE
      TBLN(2,NTS)=NR                         !# CAMAC OPS      - FALSE
      NXTI(1,NTS)=LABLV(IDES(N))             !NEXT TABLE INDEX - TRUE
      NXTI(2,NTS)=NTS+1                      !NEXT TABLE INDEX - FALSE
      CALL CKNXTI(NXTI(1,NTS),NTS)           !CHECK FOR LEGAL
      GO TO 100
C
 1000 DO 1010 I=1,NTS-1
C
      IF(KINS(I).EQ.'CONT'
     &   .AND.TBLI(1,I).EQ.0
     &   .AND.TBLI(2,I).EQ.0) KINS(I)='SKIP'
 1010 CONTINUE
C
      NUDX=1
      DO 1015 I=1,NTS
      NUNDX(I)=NUDX
      IF(KINS(I).NE.'SKIP') NUDX=NUDX+1
 1015 CONTINUE
C
      DO 1020 I=1,NTS
      IF(NXTI(1,I).GT.0) NXTI(1,I)=NUNDX(NXTI(1,I))
      IF(NXTI(2,I).GT.0) NXTI(2,I)=NUNDX(NXTI(2,I))
 1020 CONTINUE
C
      N=0
      DO 1030 I=1,NTS
      IF(KINS(I).EQ.'SKIP') GO TO 1030
      N=N+1
      LATN(N)=LATN(I)
      MASK(N)=MASK(I)
      KINS(N)=KINS(I)
      TBLI(1,N)=TBLI(1,I)
      TBLI(2,N)=TBLI(2,I)
      TBLN(1,N)=TBLN(1,I)
      TBLN(2,N)=TBLN(2,I)
      NXTI(1,N)=NXTI(1,I)
      NXTI(2,N)=NXTI(2,I)
 1030 CONTINUE
      NTS=N
      RETURN
      END
