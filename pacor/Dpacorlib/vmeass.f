C$PROG VMEASS
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     03/03/08 CAEN 792 support, modeled on TDC
C     ******************************************************************
C
      SUBROUTINE VMEASS(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
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
      INTEGER*4 IWD(20),ILO(20),IHI(20),IT(5),NAME(3)
C
      INTEGER*4 CODES(6)
      character*4 cCODES(6)
      equivalence (cCODES, CODES)
C
      DATA cCODES/'ADC ','TDC ','QDC ','A   ','ID  ','MT  '/
C
      DATA NCODE/6/
C
      DATA MAXADC,MAXTDC,MAXQDC/12,12,12/
C
      CHARACTER*12 ADCNAM,TDCNAM,QDCNAM
C
      DATA         ADCNAM/'CAEN-785    '/
      DATA         TDCNAM/'CAEN-775    '/
      DATA         QDCNAM/'CAEN-792    '/
C
      CHARACTER*4  ICOD,JCODE,MODNAM
C
      INTEGER*4    X8000,BLANK
      character*4  cBLANK
      equivalence  (cBLANK, BLANK)
      DATA         X8000,cBLANK/'8000'X,'    '/
C
      SAVE
C
C     ******************************************************************
C     PROCESSES ONE HARDWARE ASSIGNMENT LINE OF THE FORM:
C
C     $vme  adc0N  a0A-0B  NAME:0N,0I  id0N,0I  mt=MOTYP
C     $vme  tdc0N  a0A-0B  NAME:0N,0I  id0N,0I  mt=MOTYP
C     $vme  qdc0N  a0A-0B  NAME:0N,0I  id0N,0I  mt=MOTYP
C     ******************************************************************
C
      CALL CASEUP(IWD)
C
      NF=0
      IB=5
  100 IA=NXNB(IWD,IB,80)
      IF(IA.LE.0) GO TO 200
      IB=NXBL(IWD,IA,80)
      IF(IB.LE.0) IB=81
      IB=IB-1
      NF=NF+1
      ILO(NF)=IA
      IHI(NF)=IB
      IB=IB+1
      IF(IB.GE.80) GO TO 200
      GO TO 100
C
  200 MODNAM='    '
      MODNUM=0
      MOCODE=0
      SUBAD1=0
      SUBAD2=0
      IDNUM=0
      IDINC=0
C
      DO 300 N=1,NF
C
      DO 205 I=1,5
      IT(I)=BLANK
  205 CONTINUE
C
      CALL LODUP(IWD,ILO(N),IHI(N),IT,1)
C
      CALL HARDECO(IT,ICOD,NAME,I1,I2)
      JCODE=ICOD
C
      MODKOD=0
      IF(JCODE.EQ.'MT  ')   THEN
      CALL MODCOD(NAME,MODKOD,IERR)
                            ENDIF
C
      IF(JCODE.EQ.'ADC ') THEN
      IF(I1.LT.1)      GO TO 1000
      IF(I1.GT.MAXADC) GO TO 1000
      MODNAM='ADC '
      MODNUM=I1
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'TDC ') THEN
      IF(I1.LT.1)      GO TO 1010
      IF(I1.GT.MAXTDC) GO TO 1010
      MODNAM='TDC '
      MODNUM=I1
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'QDC ') THEN
      IF(I1.LT.1)      GO TO 1010
      IF(I1.GT.MAXQDC) GO TO 1010
      MODNAM='QDC '
      MODNUM=I1
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'A   ') THEN
      IF(I1.LT.1)      GO TO 1020
      IF(I2.GT.34)     GO TO 1020
      IF(I1.GT.I2)     GO TO 1020
      SUBAD1=I1
      SUBAD2=I2
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'ID  ') THEN
      IF(I1.LE.0)      GO TO 1030
      IDNUM=I1
      IDINC=I2
      IF(IDINC.LE.0) IDINC=1
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'MT  ') THEN
      MOCODE=MODKOD
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'    ') THEN
      GO TO 300
      ENDIF
C
  300 CONTINUE
C
      IF(MODNAM.EQ.'    ') GO TO 1040
      IF(MOCODE.LE.0)      GO TO 1050
      IF(SUBAD1.LE.0)      GO TO 1060
      IF(IDNUM.LE.0)       GO TO 1070
C
      IF(MODNAM.EQ.'ADC '.AND.MOCODE.NE.42) GO TO 1080
      IF(MODNAM.EQ.'TDC '.AND.MOCODE.NE.41) GO TO 1080
      IF(MODNAM.EQ.'QDC '.AND.MOCODE.NE.43) GO TO 1080
C
      IF(MODNAM.EQ.'ADC ') GO TO 400
      IF(MODNAM.EQ.'TDC ') GO TO 500
      IF(MODNAM.EQ.'QDC ') GO TO 600
      GO TO 1040
C
  400 ID=IDNUM+X8000
      DO 410 I=SUBAD1,SUBAD2
      CALL CKVMEID(ID)
      ADCID(I,MODNUM)=ID
      ID=ID+IDINC
  410 CONTINUE
      VADCMAP(MODNUM)=1
      CALL VMESAV(ADCNAM,MOCODE)
      RETURN
C
  500 ID=IDNUM+X8000
      DO 510 I=SUBAD1,SUBAD2
      CALL CKVMEID(ID)
      TDCID(I,MODNUM)=ID
      ID=ID+IDINC
  510 CONTINUE
      VTDCMAP(MODNUM)=1
      CALL VMESAV(TDCNAM,MOCODE)
      RETURN
C
  600 ID=IDNUM+X8000
      DO 610 I=SUBAD1,SUBAD2
      CALL CKVMEID(ID)
      QDCID(I,MODNUM)=ID
      ID=ID+IDINC
  610 CONTINUE
      VQDCMAP(MODNUM)=1
      CALL VMESAV(QDCNAM,MOCODE)
      RETURN
C
C
      RETURN
C
 1000 WRITE(CMSSG,1005)MAXADC
 1005 FORMAT('Illegal ADC number - allowed range = 1 to ',I2)
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)MAXTDC
 1015 FORMAT('Illegal TDC number - allowed range = 1 to ',I2)
      GO TO 2000
C
 1016 WRITE(CMSSG,1017)MAXQDC
 1017 FORMAT('Illegal QDC number - allowed range = 1 to ',I2)
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Illegal subaddress specified - allowed range = 1,34')
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('Illegal parameter ID number specified')
      GO TO 2000
C
 1040 WRITE(CMSSG,1045)
 1045 FORMAT('Module Name not specified')
      GO TO 2000
C
 1050 WRITE(CMSSG,1055)
 1055 FORMAT('Module Type not specified')
      GO TO 2000
C
 1060 WRITE(CMSSG,1065)
 1065 FORMAT('Module subaddress not properly specified')
      GO TO 2000
C
 1070 WRITE(CMSSG,1075)
 1075 FORMAT('Parameter ID number/s not properly specified')
      GO TO 2000
C
 1080 WRITE(CMSSG,1085)
 1085 FORMAT('Module Name and Module Type do not match')
      GO TO 2000
C
C
 2000 CALL ERRLOG(LOGUT,LOGUP)
      RETURN
      END
