C$PROG HISPRO
      SUBROUTINE HISPRO(NOL,JWD,IERR,MSER)
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      COMMON/HHH/ KHP(4,20),LHP(4),KRA(2,4),KGS(2,4),KHT,NHP,NHS,
     &            ICTY(100),NORS(100),LOCC(100),LOCG(100),ITPR(2,100),
     &            LIMS(2,100),IFBID(100),NCON
      CHARACTER*4 ICTY
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      COMMON/YYY/ NAMHIL(20),LISKIN,LTA,LICO,NOHIS,ITRACE
      CHARACTER*4            LISKIN
C
      INTEGER*4    LVAL(100),MSG(10,5)
      CHARACTER*40 MSC(5)
      INTEGER*4    GATES(2,200)
C
      INTEGER*4 NWD(400),JWD(32)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/'ERROR PROCESSING HISTOGRAM SPEC         ',
     &         'ERROR FROM GATAD                        ',
     &         'ERROR FROM GATLOC                       ',
     &         'ERROR FROM BANLOC                       ',
     &         'OH ILLEGAL IMMEDIATELY AFTER $H32 & $H16'/
C
      CHARACTER*40 MSER
C
      CHARACTER*4  NAME,NUBPC,KHT,IGT
C
      INTEGER*4    END,LC
      character*4  cend, clc
      DATA         cEND,cLC/'END ','LC  '/
C
      SAVE
C
C     **************************************************************
C
C     KHT      = 'H   ' OR 'OH  ' - HISTOGRAM TYPE
C     NHP      = # OF HISTOGRAM PARAMETERS
C     NHS      = # OF HISTOGRAM SETS (TO OCCUPY SAME SPACE)
C
C     KHP(J,K) = JTH HISTOGRAM PARAMETER OF THE KTH SET
C     LHP(J)   = LENGTH (PWR OF 2)   OF JTH HISTOGRAM PARM (SHIFTED)
C     KRA(1,J) = RANGE LO-LIMIT      OF JTH HISTOGRAM PARM (SHIFTED)
C     KRA(2,J) = RANGE HI-LIMIT      OF JTH HISTOGRAM PARM (SHIFTED)
C     KGS(1,J) = SELF-GATE LO-LIMIT FOR JTH HISTOGRAM PARM (RAW)
C     KGS(2,J) = SELF-GATE HI-LIMIT FOR JTH HISTOGRAM PARM (RAW)
C     **************************************************************
C     NCON      = # OF CONDITIONS
C
C     ICTY(J)   = CONDITION TYPE               FOR JTH CONDITION
C               = 'SING' SAYS SINGLE GATE
C               = 'LIST' SAYS GATE LIST
C               = 'BAN ' SAYS BAN  LIST
C               = 'MAP ' SAYS MAPPED GATE-LIST
C
C     NORS(J)   = # OF ENTRIES (GATES OR BANS) FOR JTH CONDITION
C     ITPR(1,J) = 1ST TEST PARAMETER           FOR JTH CONDITION
C     ITPR(2,J) = 2ND TEST PARAMETER (IF BAN)  FOR JTH CONDITION
C     LIMS(1,J) = LO-LIMIT (IF SINGLE GATE)    FOR JTH CONDITION
C     LIMS(2,J) = HI-LIMIT (IF SINGLE GATE)    FOR JTH CONDITION
C     LOCC(J)   = FULL-WD MIL-LOC OF GATE-LIST, GATE-MAP, OR
C                 BAN-LIST FOR JTH CONDITION
C     LOCG(J)   = HALF-WD MIL-LOC OF GATE-LIST FOR JTH COND
C     **************************************************************
C
      NHP=0
      NHS=0
      NCON=0
      IERR=0
C
      DO 10 J=1,4
      LHP(J)=0
      DO 5 I=1,2
      KRA(I,J)=0
      KGS(I,J)=0
    5 CONTINUE
   10 CONTINUE
C
      N=0
   20 ILO=JWD(23)
      IHI=JWD(24)
C
      DO 30 I=ILO,IHI
      CALL ILBYTE(IT,JWD,I-1)
      N=N+1
      CALL ISBYTE(IT,NWD,N-1)
   30 CONTINUE
      N=N+1
      CALL ISBYTE(Z'20',NWD,N-1)
C
      READ(LOU,IOSTAT=IOS,END=40)JWD
      CALL IOERR(IOS)
   40 IF(IOS.NE.0) JWD(21)=END
      IF(IOS.NE.0) JWD(22)=IOS
      IF(JWD(21).NE.LC) GO TO 100
C
C     **************************************************************
C
      IF(LISKIN.EQ.'NULL') GO TO 20
C
      IF(LISKIN.EQ.'FULL') THEN
                           NOL=NOL+1
                           WRITE(LU6,45)NOL,(JWD(I),I=1,30)
                           GO TO 20
                           ENDIF
C
   45 FORMAT(1H ,I4,2X,20A4,A4,4I3,4(1X,A4),I4)
C
      IF(JWD(31).NE.1) GO TO 20
      NOL=NOL+1
      WRITE(LU6,50)JWD(30),(JWD(I),I=1,20)
   50 FORMAT(1H ,I4,2X,20A4)
      GO TO 20
C
C     **************************************************************
C
  100 IB=N
      IA=1
      NHP=0
C
  110 IF(IA.GE.IB) GO TO 700
C
      CALL GSPAN(NWD,IA,IB,IRP,NV,LVAL,NAME,IERR,MSER)
C
      IF(IERR.LT.0) GO TO 700
      IF(IERR.NE.0) RETURN
      IA=IRP+1
C
      IF(NAME.EQ.'H   ') GO TO 190
      IF(NAME.EQ.'OH  ') GO TO 195
      IF(NAME.EQ.'L   ') GO TO 250
      IF(NAME.EQ.'R   ') GO TO 300
C     **************************************************************
      NCT=NCON+1
      ICTY(NCT)='NULL'
      NORS(NCT)=0
      LOCC(NCT)=0
      DO 120 I=1,2
      ITPR(I,NCT)=0
      LIMS(I,NCT)=0
  120 CONTINUE
C     **************************************************************
      IF(NAME.EQ.'G   ') GO TO 400
      IF(NAME.EQ.'GS  ') GO TO 450
      IF(NAME.EQ.'B   ') GO TO 500
C
      GO TO 1010
C
C     **************************************************************
C     PROCESS  -  H(P1,P2..)  OR  OH(P1,P2,,)
C     ***************************************************************
C
  190 NUBPC='NO  '                      !RESET NEW BIT/CHAN FLAG
      GO TO 200
  195 IF(NUBPC.EQ.'YES ') GO TO 1550    !ERROR IF BIT/CHAN CHANGED
C
  200 IF(NV.GT.4) GO TO 1020
      NHS=NHS+1
      IF(NHS.EQ.1) KHT=NAME
      IF(NHS.EQ.1) NHP=NV
      IF(NV.NE.NHP) GO TO 1030
      IF(NAME.NE.KHT) GO TO 1040
C
      DO 210 I=1,NHP
      LV=LVAL(I)
      IF(LV.LE.0.OR.LV.GT.NPAR) GO TO 1050
C
      KHP(I,NHS)=LV                     !SET   HIS-PARM #
      LHP(I)=LPAR(LV)                   !DEFLT LENGTH TO PARM LENGTH
      KRA(1,I)=0                        !DEFLT RANGE  TO FULL RANGE
      KRA(2,I)=LHP(I)-1                 !DEFLT RANGE  TO FULL RANGE
C
      MV=KHP(I,1)
      IF(LPAR(LV).NE.LPAR(MV)) GO TO 1060
  210 CONTINUE
      IST=NHP+1
      DO 215 I=IST,4
      KHP(I,NHS)=0
  215 CONTINUE
      GO TO 110
C
C     **************************************************************
C     PROCESS  -  L(L1,L2...)  (HISTOGRAM LENGTHS)
C     **************************************************************
C
  250 IF(NV.NE.NHP) GO TO 1070
      DO 260 I=1,NV
      LV=LVAL(I)
      NDX=KHP(I,1)
      IF(LV.LE.0.OR.LV.GT.LPAR(NDX)) GO TO 1080
      IT=LOGB2(LV)
      IF(2**IT.NE.LV) GO TO 1080
      LHP(I)=LV                         !SET   LENGTH
      KRA(1,I)=0                        !DEFLT RANGE TO FULL RANGE
      KRA(2,I)=LV-1                     !DEFLT RANGE TO FULL RANGE
  260 CONTINUE
      GO TO 110
C
C     **************************************************************
C     PROCESS  -  R(LO1,HI1 LO2,HI2 ...)  (HISTOGRAM RANGES)
C     **************************************************************
C
  300 IF(NV.NE.2*NHP) GO TO 1090
      N=0
      NDO=NV/2
      DO 310 I=1,NDO
      N=N+1
      IV=LVAL(N)
      IF(IV.LT.0.OR.IV.GE.LHP(I)) GO TO 1100
      N=N+1
      JV=LVAL(N)
      IF(JV.LT.IV.OR.JV.GE.LHP(I)) GO TO 1110
      KRA(1,I)=IV
      KRA(2,I)=JV
  310 CONTINUE
      GO TO 110
C
C     **************************************************************
C     PROCESS  -  G(P LO,HI LO,HI ..)  (SPECIFIC GATE LIST)
C     **************************************************************
C
  400 IF(2*(NV/2).EQ.NV) GO TO 1150
      IP=LVAL(1)
      IF(IP.LT.1.OR.IP.GT.NPAR) GO TO 1160
      NCON=NCON+1
      NORS(NCON)=0
      ITPR(1,NCON)=IP
      ITPR(2,NCON)=0
      ICTY(NCON)='LIST'
      IF(NV.EQ.3) ICTY(NCON)='SING'
      NDO=(NV-1)/2
      N=1
      DO 420 I=1,NDO
      N=N+1
      IV=LVAL(N)
      N=N+1
      JV=LVAL(N)
      LPR=LPAR(IP)-1
      IF(IV.LT. 0.OR.IV.GT.LPR) GO TO 1170
      IF(JV.LT.IV.OR.JV.GT.LPR) GO TO 1180
C
      IF(NDO.GT.1) GO TO 410
C
      LIMS(1,NCON)=IV
      LIMS(2,NCON)=JV
      NORS(NCON)=1
      GO TO 420
C
  410 GATES(1,I)=IV
      GATES(2,I)=JV
C
  420 CONTINUE
      IF(NDO.EQ.1) GO TO 110
C
      NORS(NCON)=NDO
      CALL GATAD(GATES,NDO,LOCC(NCON),LOCG(NCON),IERR,MSER)
      IF(IERR.NE.0) RETURN
      GO TO 110
C
C     **************************************************************
C     PROCESS  -  GS(P,IS,NA,NB)  (GATE-LIST DEFINED BY SET#)
C     **************************************************************
C
  450 IF(NV.NE.4) GO TO 1190
      IP=LVAL(1)
      IF(IP.LT.1.OR.IP.GT.NPAR) GO TO 1160
      IS=LVAL(2)
      NA=LVAL(3)
      NB=LVAL(4)
      NG=NB-NA+1
      IF(NG.LE.0) GO TO 1200
C
      CALL GATLOC(IP,IS,NA,NB,ISHI,ISIZ,LOC,LCG,IGT,IERR,MSER)
C
      IF(IERR.NE.0) RETURN
C
      NCON=NCON+1
      NORS(NCON)=NG
      LOCC(NCON)=LOC
      LOCG(NCON)=LCG
      ITPR(1,NCON)=IP
      ITPR(2,NCON)=0
      LIMS(1,NCON)=ISHI
      LIMS(2,NCON)=ISIZ
      ICTY(NCON)=IGT
      GO TO 110
C
C     **************************************************************
C     PROCESS  -  B(IX,JY,IDA,IDB)  (BAN-LIST FROM BAN-FILE)
C     **************************************************************
C
  500 IF(NV.NE.4) GO TO 1210
      IPX=LVAL(1)
      IPY=LVAL(2)
      IF(IPX.LT.1.OR.IPX.GT.NPAR) GO TO 1220
      IF(IPY.LT.1.OR.IPY.GT.NPAR) GO TO 1230
      IDA=LVAL(3)
      IDB=LVAL(4)
      NB=IDB-IDA+1
      IF(NB.LE.0) GO TO 1240
C
      CALL BANLOC(IPX,IDA,IDB,NB,ISHI,ISIZ,LOC,IERR,MSER)
C
      IF(IERR.NE.0) RETURN
C
      NCON=NCON+1
      NORS(NCON)=NB
      IFBID(NCON)=IDA
      LOCC(NCON)=LOC
      ITPR(1,NCON)=IPX
      ITPR(2,NCON)=IPY
      LIMS(1,NCON)=ISHI
      LIMS(2,NCON)=ISIZ
      ICTY(NCON)='BAN '
      GO TO 110
C
C     **************************************************************
C     ASSEMBLE MIGHTY-INSRTUCTION-LIST FOR THIS H-STATEMENT
C     **************************************************************
C
  700 CALL HISMIL(IERR,MSER)
      IF(IERR.NE.0) RETURN
C
C*    DO 720 J=1,NHS
C*    WRITE(LU6,710)KHT,NHP,(KHP(I,J),I=1,4),LHP,KRA,KGS
C*710 FORMAT(1H ,A4,25I5)
C*720 CONTINUE
C*    WRITE(LU6,725)
C*725 FORMAT(1H )
C
C*    DO 740 J=1,NCON
C*    WRITE(LU6,710)ICTY(J),ITPR(1,J),ITPR(2,J),NORS(J),
C*   &LOCC(J),LIMS(1,J),LIMS(2,J)
C*740 CONTINUE
      IERR=0
      RETURN
C
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP MESSAGE
C     **************************************************************
C
 1010 WRITE(MSER,1012)NAME
 1012 FORMAT(A4,' IS AN ILLEGAL LIST IDENTIFIER      ')
      GO TO 1500
C
 1020 WRITE(MSER,1022)NV
 1022 FORMAT(I5,' IS MORE THAN 4 HIST-PARMS         ')
      GO TO 1500
C
 1030 WRITE(MSER,1032)NV
 1032 FORMAT(I5,' H-PARMS (NOT MATCH # IN 1ST SET   ')
      GO TO 1500
C
 1040 WRITE(MSER,1042)NAME
 1042 FORMAT(A4,' HIST-TYPE DOESN,T MATCH 1ST SET    ')
      GO TO 1500
C
 1050 WRITE(MSER,1052)LV
 1052 FORMAT(I5,' IS AN ILLEGAL H-PARM NUMBER       ')
      GO TO 1500
C
 1060 WRITE(MSER,1062)LPAR(LV)
 1062 FORMAT(I5,' = H-PARM LENG (NO MATCH 1ST SET)  ')
      GO TO 1500
C
 1070 WRITE(MSER,1072)NV
 1072 FORMAT(I5,' = # H-LENGTHS (NO MATCH # H-PARMS)')
      GO TO 1500
C
 1080 WRITE(MSER,1082)LV
 1082 FORMAT(I5,' IS AN ILLEGAL H-LENGTH VALUE      ')
      GO TO 1500
C
 1090 WRITE(MSER,1092)NV
 1092 FORMAT(I5,' IS AN ILLEGAL # OF RANGE VALUES   ')
      GO TO 1500
C
 1100 WRITE(MSER,1102)IV
 1102 FORMAT(I5,' IS AN ILLEGAL RANGE LO-LIMIT      ')
      GO TO 1500
C
 1110 WRITE(MSER,1112)JV
 1112 FORMAT(I5,' IS AN ILLEGAL RANGE HI-LIMIT      ')
      GO TO 1500
C
 1150 WRITE(MSER,1152)NV-1
 1152 FORMAT(I5,' IS AN ODD # OF GATE-LIMIT VALUES  ')
      GO TO 1500
C
 1160 WRITE(MSER,1162)IP
 1162 FORMAT(I5,' IS AN ILLEGAL GATE-PARM NUMBER    ')
      GO TO 1500
C
 1170 WRITE(MSER,1172)IV
 1172 FORMAT(I5,' IS AN ILLEGAL GATE LO-LIMIT       ')
      GO TO 1500
C
 1180 WRITE(MSER,1182)JV
 1182 FORMAT(I5,' IS AN ILLEGAL GATE HI-LIMIT       ')
      GO TO 1500
C
 1190 WRITE(MSER,1192)NV
 1192 FORMAT(I5,' IS ILLEGAL # OF GATE-FILE SPECS   ')
      GO TO 1500
C
 1200 WRITE(MSER,1202)NG
 1202 FORMAT(I5,' IS AN ILLEGAL # OF GATES          ')
      GO TO 1500
C
 1210 WRITE(MSER,1212)NV
 1212 FORMAT(I5,' IS AN ILLEGAL # OF BAN-SPECIFIERS ')
      GO TO 1500
C
 1220 WRITE(MSER,1222)IPX
 1222 FORMAT(I5,' IS AN ILLEGAL BAN X-PARM          ')
      GO TO 1500
C
 1230 WRITE(MSER,1232)IPY
 1232 FORMAT(I5,' IS AN ILLEGAL BAN Y-PARM          ')
      GO TO 1500
C
 1240 WRITE(MSER,1242)NB
 1242 FORMAT(I5,' IS AN ILLEGAL # OF BANANAS        ')
      GO TO 1500
C
 1500 IERR=1
      RETURN
C
 1550 JJ=4
C
      IERR=JJ
      MSER=MSC(JJ)
      RETURN
      END
