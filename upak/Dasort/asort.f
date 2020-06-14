C$PROG ASORT
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/AAA/ IWD(20)
C   
      COMMON/BBB/ KEY(10,20000),NK,MAX
C     ------------------------------------------------------------------
      INTEGER*4  IDAT(20,20000)
C   
      CHARACTER*4  KMD,IWD1
      EQUIVALENCE (KMD,IWD),(IWD1,IWD(1))
C
      CHARACTER*4 CNAMPROG(2)
      EQUIVALENCE (CNAMPROG, NAMPROG)
      DATA         CNAMPROG/'ASOR','T   '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CMSSG=' '
      MSGF='    '
      LOGUT=6
      LOGUP=7
      LISFLG='LOF '
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'asort.log',
     &     STATUS     = 'REPLACE',
     &     IOSTAT     = IOS)
C
      LU=1
   10 WRITE(6,15)
   15 FORMAT(1H ,'ENTER FILENAME->',$)
      READ(5,20)IWD
   20 FORMAT(20A4)
C
      IF(KMD.EQ.'end ') STOP
      IF(KMD.EQ.'END ') STOP
C
      CALL LOGOPEN(LU,IWD,IERR)
      IF(IERR.NE.0) GO TO 10
C
      NN=0
C
   50 WRITE(6,55)
   55 FORMAT(1H ,'ENTER FIELD LIMITS')
      READ(5,110)IWD
      IF(IWD1.EQ.'END ') STOP
   30 CALL LIMSET(IERR)
      IF(IERR.NE.0) GO TO 10
C   
  100 READ(LU,110,END=200)IWD
  110 FORMAT(20A4)
C
      NN=NN+1
      DO 120 I=1,20
      IDAT(I,NN)=IWD(I)
  120 CONTINUE
C   
      CALL SAVIT
C   
      GO TO 100
C   
  200 CALL METSOR
C   
      DO 220 N=1,MAX
      NDX=KEY(1,N)
      WRITE(6,210)(IDAT(I,NDX),I=1,20)
      WRITE(7,210)(IDAT(I,NDX),I=1,20)
  210 FORMAT(1H ,20A4)
  220 CONTINUE
      STOP
      END
C$PROG ISWAF
      FUNCTION ISWAF(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
      INTEGER*4 ITMP,JTMP
C
      BYTE IBY(4),JBY(4)
C
      EQUIVALENCE (IBY,ITMP),(JBY,JTMP)
C
C     SWAPS BYTES FOR ONE FULL-WORD "IWD"
C
      ITMP=IWD
      JBY(1)=IBY(4)
      JBY(2)=IBY(3)
      JBY(3)=IBY(2)
      JBY(4)=IBY(1)
      ISWAF=JTMP
      RETURN
      END
C$PROG LIMSET
      SUBROUTINE LIMSET(IERR)
C   
      COMMON/AAA/ IWD(20)
C   
      COMMON/BBB/ KEY(10,20000),NK,MAX
C   
      COMMON/CCC/ ILO(10),IHI(10),NLIM
C   
      INTEGER*4 LWD(2,40),ITYP(40)
C   
      IERR=0
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C   
      IF(NTER.NE.0)      GO TO 100
      IF(NF.LT.2)        GO TO 110
      IF(2*(NF/2).NE.NF) GO TO 110
      IF(NF.GT.20)       GO TO 120
C   
      NLIM=NF/2
      N=0
C   
      DO 10 I=1,NLIM
      N=N+1
      CALL LIMIV(LWD(1,N),1,80,ILO(I),IERR)
      IF(IERR.NE.0) GO TO 100
      N=N+1
      CALL LIMIV(LWD(1,N),1,80,IHI(I),IERR)
      IF(IERR.NE.0) GO TO 100
   10 CONTINUE
C   
      DO 20 I=1,NLIM
      IF(ILO(I).GT.IHI(I)) GO TO 130
   20 CONTINUE
C   
      DO 40 I=1,NLIM
      DO 30 J=1,NLIM
      IF(I.EQ.J) GO TO 30
      IF(ILO(I).GE.ILO(J).AND.ILO(I).LE.IHI(J)) GO TO 140
      IF(IHI(I).GE.ILO(J).AND.IHI(I).LE.IHI(J)) GO TO 140
   30 CONTINUE
   40 CONTINUE
C   
      ISUM=0
      DO 50 I=1,NLIM
      ISUM=ISUM+IHI(I)-ILO(I)+1
   50 CONTINUE
      IF(ISUM.GT.36) GO TO 150
C   
      NC=4+ISUM
      NK=(NC+3)/4
C   
      RETURN
C   
  100 WRITE(6,105)
  105 FORMAT(1H ,'SYNTAX ERROR OR ILLEGAL VALUE')
      GO TO 200
  110 WRITE(6,115)
  115 FORMAT(1H ,'ODD NUMBER OF ENTRIES IN LIMIT LIST')
      GO TO 200
  120 WRITE(6,125)
  125 FORMAT(1H ,'MORE THAN 10 LIMIT PAIRS')
      GO TO 200
  130 WRITE(6,135)
  135 FORMAT(1H ,'SOME LO-LIMIT GREATER THAN HI-LIMIT')
      GO TO 200
  140 WRITE(6,145)
  145 FORMAT(1H ,'OVERLAPPING LIMITS')
      GO TO 200
  150 WRITE(6,155)
  155 FORMAT(1H ,'REQUEST FOR SORTING ON MORE 36 CHARACTERS')
C   
  200 IERR=1
      RETURN
      END
C$PROG LOGOPEN
      SUBROUTINE LOGOPEN(LU,NAMF,IERR)    
C
      INTEGER*4 NAMF(20),NAMFIL(20)
C
      CHARACTER*80 CNAMFIL
C
      EQUIVALENCE (CNAMFIL,NAMFIL)
C
      IERR=0
C
      DO 10 I=1,20
      NAMFIL(I)=NAMF(I)
   10 CONTINUE
C
      CLOSE(UNIT=LU)
      IA=LSNB(NAMFIL,1,80)
      CALL ISBYTE(0,NAMFIL,IA)
C
      OPEN(UNIT     = LU,
     &     FILE     = CNAMFIL,
     &     STATUS   = 'OLD',
     &     ACCESS   = 'SEQUENTIAL',
     &     FORM     = 'FORMATTED',
     &     IOSTAT   = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL OPENERR(ISTAT)
                     IERR=ISTAT
                     ENDIF
      RETURN
      END
C$PROG METSOR
      SUBROUTINE METSOR
C   
      COMMON/BBB/ KEY(10,20000),NK,MAX
C   
      INTEGER*4 IT(10)
C   
      N1=2
      N2=NK-1
      M=MAX
  100 M=M/2
      IF(M.EQ.0) RETURN
      K=MAX-M
      J=1
  200 I=J
  300 L=I+M
C   
      DO 302 NN=N1,N2
      IF(KEY(NN,I).GT.KEY(NN,L)) GO TO 305
      IF(KEY(NN,I).LT.KEY(NN,L)) GO TO 400
  302 CONTINUE
      IF(KEY(NK,I).LE.KEY(NK,L)) GO TO 400
C   
  305 DO 310 II=1,NK
      IT(II)=KEY(II,I)
  310 CONTINUE
      DO 320 II=1,NK
      KEY(II,I)=KEY(II,L)
  320 CONTINUE
      DO 330 II=1,NK
      KEY(II,L)=IT(II)
  330 CONTINUE
      I=I-M
      IF(I.GE.1) GO TO 300
  400 J=J+1
      IF(J.GT.K) GO TO 100
      GO TO 200
      END
C$PROG OPENERR
      SUBROUTINE OPENERR(IOS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      IF(IOS.EQ.0) RETURN
C
      WRITE(CMSSG,10)IOS
   10 FORMAT('ERROR OPENING FILE - ZSTAT =',Z10)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG SAVIT
      SUBROUTINE SAVIT
C   
      COMMON/AAA/ IWD(20)
C   
      COMMON/BBB/ KEY(10,20000),NK,MAX
C   
      COMMON/CCC/ ILO(10),IHI(10),NLIM
C   
      DATA NK,MAX/5,0/
C   
      MAX=MAX+1
C   
      IF(MAX.GT.20000) GO TO 100
C   
      KEY(1,MAX)=MAX
C   
      LOC=5
C   
      DO 10 N=1,NLIM
C   
      CALL LODUP(IWD,ILO(N),IHI(N),KEY(1,MAX),LOC)
C   
      LOC=LOC+(IHI(N)-ILO(N))+1
C   
   10 CONTINUE
C
      DO 20 II=2,10
      KEY(II,MAX)=ISWAF(KEY(II,MAX))
   20 CONTINUE
C   
      RETURN
C   
  100 WRITE(6,110)
  110 FORMAT(1H ,'MORE THAN 20000 SOURCE LINES - BYE BYE')
      STOP
      END
