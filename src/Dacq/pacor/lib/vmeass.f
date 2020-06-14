C$PROG VMEASS
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     03/03/08 CAEN 792 support, modeled on TDC
C     10/16/2017 SIS scaler support, part of VME - 2 modules, 32 chan, 3
C     words each.
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
      COMMON/PACV/ VADCMAP(24),VTDCMAP(12),VQDCMAP(12), 
     .             VSISMAP(2), VMYRMAP(1),
     .             VADCID(816),VTDCID(408),VQDCID(408), 
     .             VSISID(192),VMYRID(3)
      INTEGER*4    VADCMAP,    VTDCMAP,    VQDCMAP,
     .             VSISMAP,    VMYRMAP,
     .             VADCID,     VTDCID,     VQDCID,
     .             VSISID,     VMYRID
C     ------------------------------------------------------------------
      INTEGER*4    ADCID(34,24),TDCID(34,12),QDCID(34,12),
     .             SISID(3,32,2), MYRID(3,1)
C
      EQUIVALENCE (ADCID,VADCID),(TDCID,VTDCID),(QDCID,VQDCID),
     .            (SISID,VSISID), (MYRID, VMYRID)
C     ------------------------------------------------------------------
C 
      INTEGER*4 IWD(20),ILO(20),IHI(20),IT(5),NAME(3)
C
      INTEGER*4 CODES(8)
C
      character*4 cCODES(8)
      equivalence (cCODES, CODES)
      DATA cCODES/'ADC ','TDC ','QDC ',
     .            'SIS ','MYR ', 'A   ','ID  ','MT  '/
C
      DATA NCODE/8/
C
      DATA MAXADC,MAXTDC,MAXQDC,MAXSIS,MAXMYR/24,12,12,2,1/
C
      CHARACTER*12 ADCNAM,TDCNAM,QDCNAM,SISNAM,MYRNAM
C
      DATA         ADCNAM/'CAEN-785    '/
      DATA         TDCNAM/'CAEN-775    '/
      DATA         QDCNAM/'CAEN-792    '/
      DATA         SISNAM/'SIS_3820    '/
      DATA         MYRNAM/'MYRIAD      '/
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
C     $vme  sis0N  a0A-0B  NAME:0N,0I  id0N,0I  mt=MOTYP
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
      IF(I1.LT.1)      GO TO 1012
      IF(I1.GT.MAXQDC) GO TO 1012
      MODNAM='QDC '
      MODNUM=I1
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'SIS ') THEN
      IF(I1.LT.1)      GO TO 1014
      IF(I1.GT.MAXSIS) GO TO 1014
      if (I2 .eq. 0) I2 = I1
      MODNAM='SIS '
      MODNUM=I1
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'MYR ') THEN
      IF(I1.LT.1)      GO TO 1016
      IF(I1.GT.MAXMYR) GO TO 1016
      MODNAM='MYR '
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
      IF(MODNAM.EQ.'SIS '.AND.MOCODE.NE.44) GO TO 1080
      IF(MODNAM.EQ.'MYR '.AND.MOCODE.NE.45) GO TO 1080
C
      IF(MODNAM.EQ.'ADC ') GO TO 400
      IF(MODNAM.EQ.'TDC ') GO TO 500
      IF(MODNAM.EQ.'QDC ') GO TO 600
      IF(MODNAM.EQ.'SIS ') GO TO 700
      IF(MODNAM.EQ.'MYR ') GO TO 800
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
C     This code forces IDINC to be a multiple of 3.  
C     The SIS3820 scaler puts out 2 or 3 ints per channel because
C     channels 1 and 17 have extended precision
  700 ID=IDNUM+X8000
C     If IDINC is not a multiple of 3, bailout
      if ((idinc/3)*3 .ne. idinc) then
         goto 1070
      endif
      DO 720 I=SUBAD1,SUBAD2
      CALL CKVMEID(ID)
      SISID(1,I,MODNUM)=ID
      CALL CKVMEID(ID+1)
      SISID(2,I,MODNUM)=ID+1
      CALL CKVMEID(ID+2)
      SISID(3,I,MODNUM)=ID+2
      ID=ID+IDINC
  720 CONTINUE
      VSISMAP(MODNUM)=1
      CALL VMESAV(SISNAM,MOCODE)
      RETURN
C
C     This code forces IDINC to be a multiple of 3.  
C     MYRIAD puts out exactly three 16-bit words
  800 ID=IDNUM+X8000
C     There is only ever one myriad module
      if (subad1 .ne. subad2) then
         goto 1070
      endif

      CALL CKVMEID(ID)
      MYRID(1,MODNUM)=ID
      CALL CKVMEID(ID+1)
      MYRID(2,MODNUM)=ID+1
      CALL CKVMEID(ID+2)
      MYRID(3,MODNUM)=ID+2
      VMYRMAP(MODNUM)=1
      CALL VMESAV(MYRNAM,MOCODE)
      RETURN
C
C
C
      RETURN
C
 1000 WRITE(CMSSG,1005)MAXADC
 1005 FORMAT('Illegal ADC number - allowed range = 1 to ',I2)
      GO TO 2000
C
 1010 WRITE(CMSSG,1011)MAXTDC
 1011 FORMAT('Illegal TDC number - allowed range = 1 to ',I2)
      GO TO 2000
C
 1012 WRITE(CMSSG,1013)MAXQDC
 1013 FORMAT('Illegal QDC number - allowed range = 1 to ',I2)
      GO TO 2000
C
 1014 WRITE(CMSSG,1015)MAXSIS
 1015 FORMAT('Illegal SIS number - allowed range = 1 to ',I2)
      GO TO 2000
C
 1016 WRITE(CMSSG,1017)MAXMYR
 1017 FORMAT('Illegal MYR number - allowed range = 1 to ',I2)
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
