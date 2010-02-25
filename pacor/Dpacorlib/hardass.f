C$PROG HARDASS
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE HARDASS(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      COMMON/PAC1/ KINMOD,MODKOD,NAMOD(3),MODATA(2,12)
C 
      INTEGER*4    IWD(20),ILO(20),IHI(20),NAME(3)
C
      CHARACTER*4  IT(5)
C
      CHARACTER*4  CODES(11)
C
      DATA CODES/'C   ','N   ','A   ','F   ','K   ',
     &           'D   ','E   ','FC  ','AC  ','DT  ','ID  '/
C
      DATA NCODE/11/
C
      CHARACTER*4   ICOD,JCODE
C
      INTEGER*4     BLANK
      character*4   cBLANK
      equivalence (cBLANK,BLANK)
      DATA          cBLANK/'    '/
C
      SAVE
C
C     ************************************************************
C     PROCESSES ONE HARDWARE ASSIGNMENT LINE OF THE TYPE:
C     $LAT, $CAM, $FER, $FAS & SAVES DATA VIA CALL TO MODSAV
C     ************************************************************
C
      DO 20 J=1,12
      MODATA(1,J)=-1
      MODATA(2,J)=-1
   20 CONTINUE
      NAMOD(1)=BLANK
      NAMOD(2)=BLANK
      NAMOD(3)=BLANK
C
      KINMOD=IWD(1)
      MODKOD=0
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
  200 DO 300 N=1,NF
      DO 205 I=1,5
      IT(I)='    '
  205 CONTINUE
C
      CALL LODUP(IWD,ILO(N),IHI(N),IT,1)
C
      CALL HARDECO(IT,ICOD,NAME,I1,I2)
      JCODE=ICOD
      CALL KASUP4(JCODE)
C
      IF(JCODE.EQ.'DT  ') THEN
      WRITE(CMSSG,210)
  210 FORMAT('DT form of delay-time specification not supported')
      CALL ERRLOG(LOGUT,LOGUP)
      GO TO 300
      ENDIF
C
      IF(JCODE.EQ.'MT  ')   THEN
      CALL MODCOD(NAME,MODKOD,IERR)
      GO TO 300
                            ENDIF
C
      NDX=0
      DO 220 I=1,NCODE
      IF(JCODE.EQ.CODES(I)) THEN
                            NDX=I+1
                            MODATA(1,NDX)=I1
                            MODATA(2,NDX)=I2
                            GO TO 225
                            ENDIF
  220 CONTINUE
C
  225 IF(NDX.GT.0) GO TO 300
C
      NAMOD(1)=NAME(1)
      NAMOD(2)=NAME(2)
      NAMOD(3)=NAME(3)
      MODATA(1,1)=I1
      MODATA(2,1)=I2
C
  300 CONTINUE
C
      IF(MODATA(2,4).LT.MODATA(1,4)) MODATA(2,4)=MODATA(1,4)
C
      CALL MODSAV
C
      RETURN
      END
