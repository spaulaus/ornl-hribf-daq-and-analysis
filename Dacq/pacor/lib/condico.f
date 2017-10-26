C$PROG CONDICO
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CONDICO
C
      PARAMETER (MXI=2000)
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C 
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PACA/ JCRA(MXI),JSLO(MXI),JSUB(MXI),JFRE(MXI),
     &             JIDN(MXI),NNAF
C     
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 KIN(13)
C
      INTEGER*4 CC(200),NN(200),AA(200),FF(200),DD(200)
C
      CHARACTER*4  INS,INST,CKIN(2)
C
      EQUIVALENCE (INS,KIN(2)),(CKIN,KIN)
C
      DATA NCI,ITY,NCNAF/0,0,0/
C
      INTEGER*4   BLANK
      character*4 cBLANK
      equivalence (cBLANK, BLANK)
      DATA        cBLANK/'    '/
C
      SAVE
C
C     ************************************************************
C     READS TMP-FILE WRITTEN BY CONDX (RESULTING FROM LOOP
C     EXPANSION, ETC) AND GENERATES FINAL CONDITIONAL READOUT 
C     TABLES 
C     ************************************************************
C
      DO 10 I=1,MXI
      LABL(I)  =BLANK
      INST(I)  ='    '
      NAMD(1,I)=BLANK
      NAMD(2,I)=BLANK
      NAMD(3,I)=BLANK
      IDES(I)  =BLANK
      INDX(I)=0
      MSKI(I)=0
      NREA(I)=0
      ILOR(I)=0
      IHIR(I)=0
   10 CONTINUE
C
      REWIND 8
C
   50 IF(NCI.GE.MXI) THEN
      WRITE(CMSSG,55)NCI
   55 FORMAT('/PAC5/ ARRAY OVERFLOW AT NCI =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NCI=MXI-1
                      ENDIF
C
      READ(8,60,END=1000)KIN,ISORL,IEXPL
   60 FORMAT(2(A4,2X),3A4,2X,4(2A4,2X),2I6)
C
      IF(INS.EQ.'IFN ') GO TO 200
      IF(INS.EQ.'IFA ') GO TO 200
C
      IF(INS.EQ.'IFU ') GO TO 300
      IF(INS.EQ.'IFS ') GO TO 300
      IF(INS.EQ.'IFT ') GO TO 300
      IF(INS.EQ.'IFF ') GO TO 300
C
      IF(INS.EQ.'GOTO') GO TO 400
      IF(INS.EQ.'CONT') GO TO 500
      IF(INS.EQ.'READ') GO TO 600
      IF(INS.EQ.'CNAF') GO TO 700
                        GO TO 50
C
  200 NCI=NCI+1               !IFN,  IFA
      JSORL(NCI) =ISORL
      JEXPL(NCI) =IEXPL
      LABL(NCI)  =KIN(1)
      INST(NCI)  =CKIN(2)
      NAMD(1,NCI)=KIN(3)
      NAMD(2,NCI)=KIN(4)
      NAMD(3,NCI)=KIN(5)
      INDX(NCI)  =ISVAL(KIN(6),ITY,IERR)
      MSKI(NCI)  =ISVAL(KIN(8),ITY,IERR)
      IDES(NCI)  =KIN(12)
      GO TO 50
C
  300 NCI=NCI+1               !IFU,  IFS
      JSORL(NCI) =ISORL
      JEXPL(NCI) =IEXPL
      LABL(NCI)  =KIN(1)
      INST(NCI)  =CKIN(2)
      NAMD(1,NCI)=KIN(3)
      NAMD(2,NCI)=KIN(4)
      NAMD(3,NCI)=KIN(5)
      INDX(NCI)  =ISVAL(KIN(6),ITY,IERR)
      IDES(NCI)  =KIN(10)
      GO TO 50
C
  400 NCI=NCI+1               !GOTO
      JSORL(NCI)=ISORL
      JEXPL(NCI)=IEXPL
      LABL(NCI)=KIN(1)
      INST(NCI)=CKIN(2)
      IDES(NCI)=KIN(3)
      GO TO 50
C
  500 NCI=NCI+1               !CONT
      JSORL(NCI)=ISORL
      JEXPL(NCI)=IEXPL
      LABL(NCI)=KIN(1)
      INST(NCI)=CKIN(2)
      GO TO 50
C
  600 NCI=NCI+1               !READ
      JSORL(NCI)=ISORL
      JEXPL(NCI)=IEXPL
      INST(NCI) =CKIN(2)
      NAMD(1,NCI)=KIN(3)
      NAMD(2,NCI)=KIN(4)
      NAMD(3,NCI)=KIN(5)
      INDX(NCI)  =ISVAL(KIN(6),ITY,IERR)
      GO TO 50
C
  700 NCI=NCI+1               !CNAF
      JSORL(NCI)=ISORL
      JEXPL(NCI)=IEXPL
      INST(NCI)=CKIN(2)
      NCNAF=NCNAF+1
C
      IF(NCNAF.GT.200) THEN
      WRITE(CMSSG,705)NCNAF
  705 FORMAT('MORE THAN 200 CNAF ENTRIES ENTERED - NOT SUPPORTED')
      CALL ERRLOG(LOGUT,LOGUP)
      NCNAF=200
                       ENDIF
C 
      INDX(NCI)=NCNAF
      CC(NCNAF)=ISVAL(KIN(3), ITY,IERR)
      NN(NCNAF)=ISVAL(KIN(6), ITY,IERR)
      AA(NCNAF)=ISVAL(KIN(8), ITY,IERR)
      FF(NCNAF)=ISVAL(KIN(10),ITY,IERR)
      DD(NCNAF)=ISVAL(KIN(12),ITY,IERR)
      M =NCNAF
      CALL CKFRITE(CC(M),NN(M),AA(M),FF(M),IERR)
      GO TO 50
C
 1000 NR=0
      DO 1010 I=1,NCI
      NREA(I)=0
C
      IF(INST(I).EQ.'READ') THEN
      NR=NR+1
      NREA(I)=NR
      CALL CONNEC(NAMD(1,I),INDX(I),JC,JN,JA,JF,JI)
C
      JCRA(NR)=JC
      JSLO(NR)=JN
      JSUB(NR)=JA
      JFRE(NR)=JF
      JIDN(NR)=JI
                            ENDIF
C
      IF(INST(I).EQ.'CNAF') THEN
      NR=NR+1
      NREA(I)=NR
      NDX=INDX(I)
      JCRA(NR)=CC(NDX)
      JSLO(NR)=NN(NDX)
      JSUB(NR)=AA(NDX)
      JFRE(NR)=FF(NDX)
      JIDN(NR)=DD(NDX)
                            ENDIF
C
 1010 CONTINUE
      NNAF=NR
C
      N=0
 1020 N=N+1
      IF(N.GT.NCI) GO TO 1200
      ILOR(N)=0
      IHIR(N)=0
C
      IF(INST(N).EQ.'READ'.OR.INST(N).EQ.'CNAF')    GO TO 1020
C
      NT=N+1
      IF(INST(NT).NE.'READ'.AND.INST(NT).NE.'CNAF') GO TO 1020
      ILOR(N)=NREA(NT)
      IHIR(N)=NREA(NT)
C
 1030 NT=NT+1
      IF(INST(NT).NE.'READ'.AND.INST(NT).NE.'CNAF') THEN
                                                    N=NT-1
                                                    GO TO 1020
                                                    ENDIF
      IHIR(N)=NREA(NT)
      GO TO 1030
C
 1200 CALL LABLMAN('SAVE',IDUM,IDUM,IERR)
C
      CALL CONTAB
C
      RETURN
      END
