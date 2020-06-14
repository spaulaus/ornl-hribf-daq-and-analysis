C$PROG COMPOZ
C
      SUBROUTINE COMPOZ(LI)
C
      COMMON/BBB/  IFMTA,IFMTB,IDATE(3),IFODX,JUSTON
      CHARACTER*4                             JUSTON
      CHARACTER*16 IFMTA,IFMTB
C
      COMMON/CCC/ ICHAP,NCHAP(10)
C
      COMMON/XXX/ BOLDFLG,UNDLFLG,BOLDON,UNDLON,BOLDOF(2),UNDLOF(2)
      CHARACTER*4 BOLDFLG,UNDLFLG,BOLDON,UNDLON,BOLDOF,   UNDLOF
C
      COMMON/YYY/ KLIS,KAUTOSP,KPAGLAB,KTOFLAG,NPAGSP
      CHARACTER*4 KLIS,KAUTOSP,KPAGLAB,KTOFLAG
C
      CHARACTER*4  BOLDSAV,UNDLSAV
C
      INTEGER*4 IWD30(30),IWD(20),LWD(60),ICH(120,2),LCH(240,2)
C
      INTEGER*4 KWD(30),KWD20(20),IWD21(21)
C
      CHARACTER*4  KMD,KMM,KMDTST,CWD1,CWD211
C
      CHARACTER*76  CIWD
C
      EQUIVALENCE  (CIWD,IWD(2)),(CWD1,IWD(1)),(CWD211,IWD21(1))
C
C
      BYTE      BBOLDON(4),BUNLDON(4),BBOLDOF(8),BUNDLOF(8)
C
      EQUIVALENCE (BBOLDON,BOLDON),
     &            (BUNDLON,UNDLON),
     &            (BBOLDOF,BOLDOF),
     &            (BUNDLOF,UNDLOF)
C
      EQUIVALENCE (KWD20,KWD),(IWD30,IWD)
C
      DATA NWID,NLPG,NDENT,ISHIF,ISPA,NLPR,IHI,NPAG/
     & 75,56,7,0,1,0,75,0/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     *************************************************************
C     READ IN A CARD IMAGE AND DETERMINE TYPE
C     *************************************************************
C   
   10 CALL GETIWD(LI,KLIS,IWD30)
C
      IF(KMDTST(IWD,KMD).NE.'YES ') GO TO 10
C   
   30 IF(KMD.EQ.'JON$') GO TO 44
      IF(KMD.EQ.'JOF$') GO TO 46
C   
      IF(KMD.EQ.'CHP$') GO TO 50
      IF(KMD.EQ.'TOF$') GO TO 60
      IF(KMD.EQ.'TPF$') GO TO 60
      IF(KMD.EQ.'QIT$') GO TO 65
C   
      IF(KMD.EQ.'SP1$') GO TO 70
      IF(KMD.EQ.'SP2$') GO TO 70
      IF(KMD.EQ.'SP3$') GO TO 70
      IF(KMD.EQ.'SP4$') GO TO 70
      IF(KMD.EQ.'SP5$') GO TO 70
C
      IF(KMD.EQ.'NAS$') GO TO 90
      IF(KMD.EQ.'ASP$') GO TO 92
      IF(KMD.EQ.'NPL$') GO TO 94
      IF(KMD.EQ.'PGL$') GO TO 96
      IF(KMD.EQ.'NOT$') GO TO 98
      IF(KMD.EQ.'TON$') GO TO 100
      IF(KMD.EQ.'NPS$') GO TO 102
C   
      IF(KMD.EQ.'BON$') GO TO 150
      IF(KMD.EQ.'BOF$') GO TO 150
      IF(KMD.EQ.'UON$') GO TO 150
      IF(KMD.EQ.'UOF$') GO TO 150
C
      IF(KMD.EQ.'SIN$') GO TO 160
      IF(KMD.EQ.'DBL$') GO TO 165
      IF(KMD.EQ.'RSH$') GO TO 170
      IF(KMD.EQ.'RND$') GO TO 190
      IF(KMD.EQ.'RPD$') GO TO 200
C   
      IF(KAUTOSP.EQ.'YES ') THEN
                            WRITE(7,85)
                            CALL PAGER(1,NLPG,NPAG,IHI)
                            ENDIF
C   
      IF(KMD.EQ.'CY1$') GO TO 300
      IF(KMD.EQ.'CYB$') GO TO 300
      IF(KMD.EQ.'COV$') GO TO 350
      IF(KMD.EQ.'CEN$') GO TO 400
      IF(KMD.EQ.'CEB$') GO TO 400
C   
      IF(KMD.EQ.'SEC$') GO TO 450
C   
      IF(KMD.EQ.'BLP$') GO TO 500
      IF(KMD.EQ.'INP$') GO TO 510
      IF(KMD.EQ.'IDP$') GO TO 510
      IF(KMD.EQ.'ODP$') GO TO 520
      IF(KMD.EQ.'FIP$') GO TO 530
C   
      GO TO 10
C   
C     *************************************************************
C     DO ANY PRINTER CONTROL OPERATIONS
C     *************************************************************
C   
   44 JUSTON='ON  '
      GO TO 10
   46 JUSTON='OFF '
      GO TO 10
C   
C     *************************************************************
C     RESET CHAPTER NO. ICHAP
C     *************************************************************
C   
   50 READ(CIWD,52)ICHAP,NCHAP
   52 FORMAT(11A4)
      GO TO 10
C
   60 IF(KTOFLAG.NE.'YES ') GO TO 10
      CALL PAGER(1000,NLPG,NPAG,IHI)
      GO TO 10
   65 RETURN
C   
C     *************************************************************
C     SKIP REQUIRED # OF LINES
C     *************************************************************
C   
   70 READ(CWD1,75)LSKIP
   75 FORMAT(2X,I1)
      DO 80 I=1,LSKIP
      WRITE(7,85)
      CALL PAGER(1,NLPG,NPAG,IHI)
   80 CONTINUE
   85 FORMAT(1H )
      GO TO 10
C   
C     *************************************************************
C     PROCESS NAS$, ASP$, NPL$, PGL$, NOT$, TON$, NPS$ 
C     *************************************************************
   90 KAUTOSP='NO  '
      GO TO 10
   92 KAUTOSP='YES '
      GO TO 10
   94 KPAGLAB='NO  '
      GO TO 10
   96 KPAGLAB='YES '
      GO TO 10
   98 KTOFLAG='NO  '
      GO TO 10
  100 KTOFLAG='YES '
      GO TO 10
  102 READ(CIWD,105)NPAGSP
  105 FORMAT(I2)
      GO TO 10
C   
C   
C     *************************************************************
C     PROCESS BON$, BOF$, UON$ & UOF$ TYPES
C     *************************************************************
C
  150 CALL FONTMO('SET ',KMD)
      GO TO 10
C   
C     *************************************************************
C     READ IN NEW VALUES OF ISHIF, NDENT, NWID ETC.
C     *************************************************************
C   
  160 ISPA=1
      GO TO 10
  165 ISPA=2
      GO TO 10
  170 READ(CIWD,175)LOF1,LOF2
  175 FORMAT(2I2)
      IF(LOF1.LT.0) LOF1=0
      IF(LOF2.LT.0) LOF2=0
      WRITE(IFMTA,180)LOF1
      WRITE(IFMTB,180)LOF2
  180 FORMAT('(',I2.2,'X,120A1)')
      IFODX=2
      GO TO 10
  190 READ(CIWD,175)NDENT
      GO TO 10
  200 READ(CIWD,205)NWID,NLPG,ITM
  205 FORMAT(3I3)
      IF(ITM.NE.0) NPAG=ITM-1
      IF(NWID.GT.119) NWID=119
      IHI=NWID
      GO TO 10
C   
C     *************************************************************
C     PROCESS THE CY1$ & CYB$ TYPES
C     *************************************************************
C   
  300 KMM=KMD
      BOLDSAV=BOLDFLG
      IF(KMM.NE.'CYB$')     GO TO 310
      IF(BOLDFLG.EQ.'BON$') GO TO 310
      CALL FONTMO('SET ','BON$')
C
  310 CALL GETIWD(LI,KLIS,IWD30)
C
      IF(KMDTST(IWD,KMD).NE.'YES ') GO TO 320
C
      IF(BOLDSAV.EQ.BOLDFLG) GO TO 30
      CALL FONTMO('SET ',BOLDSAV)
      GO TO 30
C
  320 CALL UNPAC120(IWD,ICH,100)
      CALL   PAC120(ICH,IWD,1,100)
C
      CALL TRIMOUT(IWD,100)
C
      IF(ISPA.NE.1)  WRITE(7,85)
      CALL PAGER(ISPA,NLPG,NPAG,IHI)
      GO TO 310
C
C     *************************************************************
C     PROCESS THE COV$ & COF$ TYPES
C     *************************************************************
C   
  350 KMM=KMD
C
  360 READ(LI,365)IWD
  365 FORMAT(20A4)
C
      IF(CWD1.EQ.'COF$') GO TO 10
C
      CALL TRIMOUT(IWD,80)
C
      IF(ISPA.NE.1)  WRITE(7,85)
      CALL PAGER(ISPA,NLPG,NPAG,IHI)
      GO TO 360
C   
C     *************************************************************
C     PROCESS THE CEN$ & CEB$ TYPES
C     *************************************************************
C   
  400 KMM=KMD
      BOLDSAV=BOLDFLG
      IF(KMM.NE.'CEB$')     GO TO 410
      IF(BOLDFLG.EQ.'BON$') GO TO 410
      CALL FONTMO('SET ','BON$')
C
  410 CALL GETIWD(LI,KLIS,IWD30)
C
      IF(KMDTST(IWD,KMD).NE.'YES ') GO TO 420
C
      IF(BOLDSAV.EQ.BOLDFLG) GO TO 30
      CALL FONTMO('SET ',BOLDSAV)
      GO TO 30
C
  420 CALL BLANKIT(KWD,30)
      CALL UNPAC120(IWD,ICH,80)
      LAC=LASTC(ICH,80)
      IF(LAC.EQ.0) GO TO 440
      JS=(IHI-LAC)/2
      IF(JS.LT.0) JS=0
      IF(LAC.LT.1) GO TO 440
C
      CALL PAC120(ICH,KWD,JS+1,80)
C
  440 IF(KMM.EQ.'CEB$') CALL FONTMO('DO  ','BON$')
C
      CALL TRIMOUT(KWD20,80)
C
      IF(KMM.EQ.'CEB$') CALL FONTMO('RES ','BON$')
      IF(ISPA.NE.1)  WRITE(7,85)
      CALL PAGER(ISPA,NLPG,NPAG,IHI)
      GO TO 410
C   
C     *************************************************************
C     PRINT OUT A SECTION HEADING (BOLD & UNDERLINED)
C     *************************************************************
C   
  450 JBEGT=LSNB(IWD,1,NWID)
      IBEGT=JBEGT+1
      IF(IBEGT.LE.2)    GO TO 470
      IF(IBEGT.GT.NWID) GO TO 470
C
      CALL BLANKC(IWD,IBEGT,NWID)
C
  470 IWD(1)=ICHAP
C
      IF(KLIS.NE.'LN03') GO TO 480
C
      CALL LODUP(UNDLOF,1,6,IWD,NWID+1)
C
      CWD211=UNDLON
      DO 475 I=1,20
      IWD21(I+1)=IWD(I)
  475 CONTINUE
C
      CALL FONTMO('DO  ','BON$')
C
      CALL TRIMOUT(IWD21,84)
C
      CALL FONTMO('RES ','BON$')
      CALL PAGER(1,NLPG,NPAG,IHI)
      GO TO 10
C
  480 CALL TRIMOUT(IWD,80)
C
      CALL PAGER(1,NLPG,NPAG,IHI)
      GO TO 10
C   
C     *************************************************************
C     PROCESS PARAGRAPHS - BLP$, INP$, IDP$, ODP$, FIP$
C     *************************************************************
C     SET UP STRING BOUNDRIES FOR SPECIFIC PARAGRAPH TYPE
C   
C     ILA # FIRST CHARACTER TO BE SET ON LINE 1
C     ILB # FIRST CHARACTER TO BE SET ON LINE 2 AND SUBSEQUENT LINES
C     ILL # FIRST CHARACTER TO BE SET ON CURRENT LINE
C     IHI # LAST  CHARACTER TO BE SET FOR ALL LINES
C   
C     IPO POINTS TO THE LAST CHARACTER SET IN INPUT BUFFER LCH(240)
C     *************************************************************
C   
  500 ILA=ISHIF+1
      ILB=ILA
      GO TO 600
  510 ILA=ISHIF+NDENT+1
      ILB=ILA-NDENT
      GO TO 600
  520 ILA=ISHIF+1
      ILB=ILA+NDENT
      GO TO 600
  530 ILA=ISHIF+NDENT+1
      ILB=ILA
  600 IHI=NWID
      IF(IHI.GT.119) IHI=119
      ILL=ILA
C   
C     SET LCH-BUFFER TO BLANK *************************************
C   
      CALL BLANKIT(LWD,60)
C
      CALL UNPAC240(LWD,LCH,240)
C   
C     READ LINE IMAGES (UNTIL YOU HIT A CONTROL) AND FILL LCH-BUFFER
C   
      IPO=ILA-1
  650 CALL GETIWD(LI,KLIS,IWD30)
C
      IF(KMDTST(IWD,KMD).EQ.'YES ') GO TO 700
C
      CALL UNPAC120(IWD,ICH,80)
      KDO=LASTC(ICH(1,1),80)
      IF(KDO.LT.1) GO TO 670
      DO 660 I=1,KDO
      IPO=IPO+1
      LCH(IPO,1)=ICH(I,1)
      LCH(IPO,2)=ICH(I,2)
  660 CONTINUE
  670 IPO=IPO+1
      LCH(IPO,1)='20'X
      LCH(IPO,2)=0
      IF(IPO.LE.IHI) GO TO 650
C   
C     *************************************************************
C     LINE HAS OVERFLOWED - GO PRINT IT OUT
C     *************************************************************
C   
      CALL  LINOUT(LCH,IPO,ILB,ILL,IHI,ISPA,NLPG,NPAG)
      ILL=ILB
      GO TO 650
C   
C     *************************************************************
C     PRINT OUT UNFILLED LINE
C     *************************************************************
C
  700 IF(IPO.LE.0) GO TO 30   
      CALL LINOUT(LCH,IPO,ILB,ILL,IHI,ISPA,NLPG,NPAG)
      GO TO 30
      END
