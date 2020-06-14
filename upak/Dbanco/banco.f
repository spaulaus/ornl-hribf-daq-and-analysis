C$PROG BANCO
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40)
C
      COMMON/BBB/ LIN,LOU,NLN,NERR,NUID
C
      COMMON/JJJ/ IDIR(880),NBAN,NBANO
C
      COMMON/NNN/ NAMFI(20),NAMFO(20)
C
      INTEGER*4 IHELP(14,16)
C
      CHARACTER*56 CHELP(16)
C
      CHARACTER*4  KMD,EXTI,EXTO
C
      EQUIVALENCE (KMD,LWD(1,1)),(CHELP,IHELP)
C
      DATA CHELP/
     &'LON/LOF       ;Turns output to banco.log ON/OFF         ',
     &'IN   fil.ban  ;Specifies ban-file as input              ',
     &'IN   fil.bed  ;Specifies bed-file as input              ',
     &'OU   fil.ban  ;Specifies ban-file as output             ',
     &'OU   fil.bed  ;Specifies bed-file as output             ',
     &'              ;The OU command always creates a new file.',
     &'              ;You can copy multiple input files to an  ',
     &'              ;output file but once the output is closed',
     &'              ;you cannot append to it.                 ',
     &'CLI           ;Closes input  file                       ',
     &'CLO           ;Closes output file                       ',
     &'NUID ID       ;Specifies next BAN-ID to use             ',
     &'COPY          ;Copies input to output                   ',
     &'                                                        ',
     &'HELP          ;Displays this list again                 ',
     &'END           ;Ends program                             '/
C
      CHARACTER*4 CNAMPROG(2)
      EQUIVALENCE (CNAMPROG,NAMPROG)
      DATA  CNAMPROG/'BANC','O   '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      LIN=1
      LOU=2
      INOP=0
      OUOP=0
      NUID=0
C
      CMSSG=' '
      LOGUT=6
      LOGUP=7
      LISFLG='LOF '
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'banco.log',
     &     STATUS     = 'REPLACE')
C
C
   50 WRITE(LOGUT,60)IHELP
   60 FORMAT(1H ,14A4)
C
  100 WRITE(LOGUT,105)
  105 FORMAT(' BANCO->',$)
C
      READ(5,110)IWD
  110 FORMAT(20A4)
C
      WRITE(CMSSG,115)IWD
  115 FORMAT(20A4)
      CALL MESSLOG(0,LOGUP)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMD.EQ.'in  ') go to 120
      IF(KMD.EQ.'ou  ') go to 140
C
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMD.EQ.'HELP') GO TO 50
      IF(KMD.EQ.'IN  ') GO TO 120
      IF(KMD.EQ.'CLI ') GO TO 130
      IF(KMD.EQ.'OU  ') GO TO 140
      IF(KMD.EQ.'CLO ') GO TO 150
      IF(KMD.EQ.'NUID') GO TO 160
      IF(KMD.EQ.'LON ') GO TO 170
      IF(KMD.EQ.'LOF ') GO TO 170
      IF(KMD.EQ.'COPY') GO TO 200
      IF(KMD.EQ.'END ') STOP
      GO TO 500
C
  120 INOP=0
C
      CALL GETNAM(IWD,NAMFI,EXTI,LDOT,INEW,IERR)
      IF(IERR.NE.0) GO TO 100
      CALL BOPEN(LIN,NAMFI,EXTI,IERR)
      IF(IERR.NE.0) GO TO 100
      INOP=1
      GO TO 100
C
  130 CLOSE(UNIT=LIN)
      INOP=0
      GO TO 100
C
  140 OUOP=0
      NBANO=0
C
      CALL GETNAM(IWD,NAMFO,EXTO,LDOT,INEW,IERR)
      IF(IERR.NE.0) GO TO 100
      CALL BOPEN(LOU,NAMFO,EXTO,IERR)
      IF(IERR.NE.0) GO TO 100
      OUOP=1
      GO TO 100
C
  150 CLOSE(UNIT=LOU)
      OUOP=0
      GO TO 100
C
  160 NUID=0
      CALL LIMIV(LWD(1,2),0,9999,NUID,IERR)
      IF(IERR.NE.0) GO TO 500
      GO TO 100
C
  170 LISFLG=KMD
      GO TO 100
C
  200 IF(INOP.EQ.0)    GO TO 520
      IF(OUOP.EQ.0)    GO TO 530
C
      CALL BANCOP(EXTI,EXTO)
C
      GO TO 100
C
  500 WRITE(CMSSG,505)
  505 FORMAT('ILLEGAL COMMAND - IGNORED')
      GO TO 700
C
  520 WRITE(CMSSG,525)
  525 FORMAT('INPUT FILE NOT OPEN')
      GO TO 700
C
  530 WRITE(CMSSG,535)
  535 FORMAT('OUTPUT FILE NOT OPEN')
      GO TO 700
C
  700 CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
C$PROG BANCOP
C
      SUBROUTINE BANCOP(EXTII,EXTOO)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40)
C
      COMMON/BBB/ LIN,LOU,NLN,NERR,NUID
C
      COMMON/JJJ/ IDIR(880),NBAN,NBANO
C
      COMMON/KKK/ TIT(20),FIL(6),ID,IH,DG,NP,KPAR(9),IX(64),JY(64)
C
      COMMON/NNN/ NAMFI(20),NAMFO(20)
C     ------------------------------------------------------------------
C
      INTEGER*4 MSG(7),NAMI(6),NAMO(6)
C
      EQUIVALENCE (NAMI,NAMFI),(NAMO,NAMFO)
C
      CHARACTER*4  EXTII,EXTOO,EXTI,EXTO
C
      SAVE
C
C     ------------------------------------------------------------------
C
      EXTI=EXTII
      EXTO=EXTOO
      IF(EXTI.EQ.'.BAN') EXTI='.ban'
      IF(EXTI.EQ.'.BED') EXTI='.bed'
      IF(EXTO.EQ.'.BAN') EXTO='.ban'
      IF(EXTO.EQ.'.BED') EXTO='.bed'
C
      IF(EXTI.EQ.'.bed') THEN
                         REWIND LIN
                         CALL BEDID(IERR)
                         REWIND LIN
                         ENDIF
C
      IF(EXTI.EQ.'.ban') THEN
      CALL BANIO(5,LIN,FIL,TIT,IH,0,DG,IDIR,JY,NP,NBAN,KPAR,MSG,
     &IERR)
      CALL BANERR(IERR,MSG,0)
      IF(IERR.NE.0)      RETURN
                         ENDIF
C
      WRITE(CMSSG,10)NBAN
   10 FORMAT('NO. OF BANANAS ON INPUT FILE =',I6)
      CALL MESSLOG(LOGUT,LOGUP)
C
      IHI=0
   20 ILO=IHI+1
      IF(ILO.GT.NBAN) GO TO 50
      IHI=ILO+9
      IF(IHI.GT.NBAN) IHI=NBAN
      WRITE(CMSSG,30)(IDIR(I),I=ILO,IHI)
   30 FORMAT(10I6)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 20
C
   50 DO 100 NN=1,NBAN
      IF(EXTI.EQ.'.bed') CALL BEDIN(IERR)
      IF(IERR.NE.0)      RETURN
      IF(EXTI.EQ.'.ban') CALL BANIN(IDIR(NN),IERR)
      IF(IERR.NE.0)      RETURN
      IDOL=ID
      IF(NUID.GT.0) ID=NUID
      IF(EXTO.EQ.'.bed') CALL BEDOU(IERR)
      IF(IERR.NE.0)      RETURN
      IF(EXTO.EQ.'.ban') CALL BANOU(ID,IERR)
      IF(IERR.NE.0)      RETURN
      WRITE(CMSSG,60)IDOL,NAMI,ID,NAMO
   60 FORMAT(I5,' from ',6A4,' stored as ',I5,' on ',6A4)
      CALL MESSLOG(LOGUT,LOGUP)
      IF(NUID.GT.0) NUID=NUID+1
  100 CONTINUE
      RETURN
      END
C$PROG BANERR
      SUBROUTINE BANERR(IERR,MSG,ID)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4 MSG(7)
C
      IF(IERR.EQ.0) RETURN
      WRITE(CMSSG,10)MSG,ID
   10 FORMAT(7A4,'  --  ID =',I7)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG BANIN
      SUBROUTINE BANIN(IBN,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/BBB/ LIN,LOU,NLN,NERR,NUID
C
      COMMON/KKK/ TIT(20),FIL(6),ID,IH,DG,NP,KPAR(9),IX(64),JY(64)
C
      INTEGER*4 MSG(7)
C
      CALL BANIO(1,LIN,FIL,TIT,IH,IBN,DG,IX,JY,NP,NID,KPAR,MSG,
     &IERR)
      CALL BANERR(IERR,MSG,IBN)
      ID=IBN
      RETURN
      END
C$PROG BANOU
      SUBROUTINE BANOU(IBN,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/BBB/ LIN,LOU,NLN,NERR,NUID
C
      COMMON/KKK/ TIT(20),FIL(6),ID,IH,DG,NP,KPAR(9),IX(64),JY(64)
C
      INTEGER*4 MSG(7)
C
      CALL BANIO(2,LOU,FIL,TIT,IH,IBN,DG,IX,JY,NP,NID,KPAR,MSG,
     &IERR)
      CALL BANERR(IERR,MSG,IBN)
      RETURN
      END
C$PROG BEDID
      SUBROUTINE BEDID(IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40)
C
      COMMON/BBB/ LIN,LOU,NLN,NERR,NUID
C
      COMMON/JJJ/ IDIR(880),NBAN,NBANO
C
      COMMON/KKK/ TIT(20),FIL(6),ID,IH,DG,NP,KPAR(9),IX(64),JY(64)
C
      INTEGER*4 LINO(880)
C
      REAL*4 XV
C
      CHARACTER*4  LAB
C
      EQUIVALENCE (LAB,IWD(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
      NLIN=0
      NBAN=0
  100 NLIN=NLIN+1
      READ(LIN,105,ERR=1000,END=500)IWD
  105 FORMAT(20A4)
      IF(LAB.NE.'INP ') GO TO 100
C
      IA=NXNB(IWD,4,80)
      IF(IA.LE.0)      GO TO 1020
      IB=NXBL(IWD,IA,80)-1
      IF(IB.LE.0)      GO TO 1020
      IF(IB-IA.GT.39)  GO TO 1020
      IA=IB+1
C
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER)
C
      IF(NTER.NE.0)    GO TO 1020
C
      CALL MILV(LWD(1,2),IDD,XV,KIND,MERR)
      IF(MERR.NE.0)    GO TO 1020
      NBAN=NBAN+1
      IDIR(NBAN)=IDD
      LINO(NBAN)=NLIN
      GO TO 100
C
  500 DO 550 I=1,NBAN
C
      IF(IDIR(I).GT.9999) THEN
                          WRITE(CMSSG,505)IDIR(I),LINO(I)
                          IERR=IERR+1
                          CALL MESSLOG(LOGUT,LOGUP)
                          ENDIF
C
  505 FORMAT('ID# = ',I8,'  AT LINE#',I5,'  IS .GT. 4 DIGITS')
C
      JS=I+1
      DO 520 J=JS,NBAN
      IF(IDIR(J).NE.IDIR(I)) GO TO 520
      WRITE(CMSSG,515)IDIR(I),LINO(I),LINO(J)
  515 FORMAT('ID#',I8,'  IS DUPLICATED ON LINES',I5,' & ',I5)
      IERR=IERR+1
      CALL MESSLOG(LOGUT,LOGUP)
  520 CONTINUE
  550 CONTINUE
      RETURN
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('ERROR READING INPUT FILE')
      GO TO 1200
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('SYNTAX ERROR ON INP-LINE')
C
 1200 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,1205)NLIN
 1205 FORMAT('PROCESS ABORTED AT LINE NUMBER -',I5)
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
C$PROG BEDIN
      SUBROUTINE BEDIN(IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40)
C
      COMMON/BBB/ LIN,LOU,NLN,NERR,NUID
C
      COMMON/KKK/ TIT(20),FIL(6),ID,IH,DG,NP,KPAR(9),IX(64),JY(64)
C
      INTEGER*4 IHTONP(10)
C
      REAL*4 XV
C
      CHARACTER*4  LAB
C
      EQUIVALENCE (LAB,IWD(1))
C
      INTEGER*4    BLANK
      DATA         BLANK/Z'20202020'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     **************************************************************
C     ROUTINE TO READ ONE ENTRY FROM A BED-FILE
C     **************************************************************
C
C     LET N="ENTRY #" AND M=N-1
C
C     LINE-(12*M+1) - 'INP ',FILENAME,IH,IB,DG,NP     (7A4,2X,4I5)
C     LINE-(12*M+2) - 'TIT ',TITLE                    (20A4)
C     LINE-(12*M+3) - 'GATE ',IPX,IPY,LXD,LXG,LYD,LYG (A4,1X,6I5)
C     LINE-(12*M+4) - 'CXY  ',X,Y X,Y X,Y .....       (A4,1X,14I5)
C     LINE-(12*M+12)- 'CXY  ',X,Y X,Y X,Y .....       (A4,1X,14I5)
C     **************************************************************
C     LIN  = LOGICAL UNIT # FOR INPUT BAN- OR BED-FILE
C     FIL  - CONTAINS .HIS-FILE NAME
C     TIT  - CONTAINS TITLE
C     IH   = HISTOGRAM ID #
C     ID   = BANANA    ID #
C     DG   = PROJECTION AXIS IN DEGREES
C     IX   - ARRAY CONTAINING X-COORDINATES
C     JY   - ARRAY CONTAINING Y-COORDINATES
C     NP   = # OF X,Y-POINTS
C     KPAR(I),I=1,9 - CONTAINS IPX,IPY,LXD,LXG,LYD,LYG,NUPM,IAUX,JAUX
C     IERR = ERROR FLAG
C     **************************************************************
C
C     ************************************** READ IN FIL,IH,IB,..NP
C
      IERR=0
      N=0
      NP=0
      NLN=NLN+1
      READ(LIN,100,ERR=1000,END=500)IWD
  100 FORMAT(20A4)
      IF(LAB.NE.'INP ')GO TO 1010
C
      IA=NXNB(IWD,4,80)
      IF(IA.LE.0)      GO TO 1020
      IB=NXBL(IWD,IA,80)-1
      IF(IB.LE.0)      GO TO 1020
      IF(IB-IA.GT.39)  GO TO 1020
C
      DO 120 I=1,10
      FIL(I)=BLANK
      IHTONP(I)=0
  120 CONTINUE
C
      CALL LODUP(IWD,IA,IB,FIL,1)
      IA=IB+1
C
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER)
C
      IF(NTER.NE.0)    GO TO 1020
      IF(NF.GT.4) NF=4
C
      DO 210 J=1,NF
      CALL MILV(LWD(1,J),IHTONP(J),XV,KIND,MERR)
      IF(MERR.NE.0)    GO TO 1020
  210 CONTINUE
C
      IH=IHTONP(1)
      ID=IHTONP(2)
      DG=IHTONP(3)
      NP=IHTONP(4)
C
C     ************************************** READ IN TIT-LINE
C
      NLN=NLN+1
      READ(LIN,100,ERR=1000,END=500)IWD
      IF(LAB.NE.'TIT ')GO TO 1030
C
      DO 212 I=1,19
      TIT(I)=IWD(I+1)
  212 CONTINUE
      TIT(20)=BLANK
C
C     ************************************** READ IN GATE-LINE
C
      DO 220 I=1,9
      KPAR(I)=0
  220 CONTINUE
      NLN=NLN+1
C
      READ(LIN,100,ERR=1000,END=500)IWD
      IF(LAB.NE.'GATE')GO TO 1040
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
C
      IF(NTER.NE.0)    GO TO 1050
      IF(NF.GT.9) NF=9
      DO 230 J=1,NF
      CALL MILV(LWD(1,J),KPAR(J),XV,KIND,MERR)
      IF(MERR.NE.0)    GO TO 1050
  230 CONTINUE
C
C     ************************************** READ IN BANANA-LINES
C
      N=0                                   !ZERO # PTS CNTR
      DO 260 I=1,100                        !LOOP ON ALL CXY LINES
      NLN=NLN+1
      READ(LIN,100,ERR=1000,END=500)IWD
C
      IF(I.GT.1.AND.LAB.NE.'CXY ') GO TO 400
      IF(I.EQ.1.AND.LAB.NE.'CXY ') GO TO 1060
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0)    GO TO 1070
      IF(NF.EQ.0)      GO TO 260
      NDO=NF/2
      IF(2*NDO.NE.NF)  GO TO 1070
      J=0
C
      DO 250 K=1,NDO                        !LOOP ON # X-Y PNTS
      N=N+1
      IF(N.GT.63)      GO TO 1080           !TST FOR TOO MANY 
      J=J+1
      IF(ITYP(J).NE.2) GO TO 1070
      CALL IVALU(LWD(1,J),IX(N),JERR)       !PICK UP X-VALUE
      IF(JERR.NE.0)    GO TO 1070
      J=J+1
      IF(ITYP(J).NE.2) GO TO 1070
      CALL IVALU(LWD(1,J),JY(N),JERR)       !PICK UP Y-VALUE
      IF(JERR.NE.0)    GO TO 1070
  250 CONTINUE
C
  260 CONTINUE
      NP=N
      RETURN
C
  400 NP=N
      BACKSPACE LIN
      NLN=NLN-1
      RETURN
C
  500 NP=N
      RETURN
C
C     **************************************************************
C     SET UP ERROR MESSAGES AND ERROR CODE - IERR
C     **************************************************************
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('ERROR READING INPUT FILE')
      GO TO 1200
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('SEQUENCE ERROR - INP-LINE EXPECTED')
      GO TO 1200
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('SYNTAX ERROR ON INP-LINE')
      GO TO 1200
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('SEQUENCE ERROR - TIT-LINE EXPECTED')
      GO TO 1200
C
 1040 WRITE(CMSSG,1045)
 1045 FORMAT('SEQUENCE ERROR - GATE-LINE EXPECTED')
      GO TO 1200
C
 1050 WRITE(CMSSG,1055)
 1055 FORMAT('SYNTAX ERROR ON GATE-LINE')
      GO TO 1200
C
 1060 WRITE(CMSSG,1065)
 1065 FORMAT('SEQUENCE ERROR - CXY-LINE EXPECTED')
      GO TO 1200
C
 1070 WRITE(CMSSG,1075)
 1075 FORMAT('SYNTAX ERROR ON CXY-LINE')
      GO TO 1200
C
 1080 WRITE(CMSSG,1085)
 1085 FORMAT('MORE THAN 63 X-Y POINTS - NOT ALLOWED')
C
 1200 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,1205)NLN
 1205 FORMAT('PROCESS ABORTED AT LINE NUMBER -',I5)
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
C$PROG BEDOU
      SUBROUTINE BEDOU(IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/BBB/ LIN,LOU,NLN,NERR,NUID
C
      COMMON/JJJ/ IDIR(880),NBAN,NBANO
C
      COMMON/KKK/ TIT(20),FIL(6),ID,IH,DG,NP,KPAR(9),IX(64),JY(64)
C
      INTEGER*4 JDIR(880)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      DO 10 I=1,NBANO
      IF(ID.EQ.JDIR(I)) GO TO 500
   10 CONTINUE
      NBANO=NBANO+1
      JDIR(NBANO)=ID
C
      WRITE(LOU,165)FIL,IH,ID,DG,NP
  165 FORMAT('INP ',6A4,4I6)
C
      IT=LSNB(TIT(1),1,72)
      IHI=IT/4+1
      IF(IHI.GT.19) IHI=19
      WRITE(LOU,170)(TIT(I),I=1,IHI)
  170 FORMAT('TIT ',19A4)
C
      WRITE(LOU,180)(KPAR(I),I=1,9)
  180 FORMAT('GATE',9I6)
C
      IHI=0
  200 ILO=IHI+1
      IF(ILO.GT.NP) GO TO 250
      IHI=ILO+5
      IF(IHI.GT.NP) IHI=NP
C
      WRITE(LOU,210)(IX(I),JY(I),I=ILO,IHI)
  210 FORMAT('CXY ',12I6)
      GO TO 200
C
  250 CONTINUE
      RETURN
C
  500 WRITE(CMSSG,505)ID
  505 FORMAT('ATTEMPT TO OUTPUT DUPLICATE ID =',I6,'  REJECTED')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
C$PROG BOPEN
      SUBROUTINE BOPEN(LU,NAMF,IEXT,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/BBB/ LIN,LOU,NLN,NERR,NUID
C
      COMMON/KKK/ TIT(20),FIL(6),ID,IH,DG,NP,KPAR(9),IX(64),JY(64)
C
      INTEGER*4 MSG(7)
C
      INTEGER*4 RECLVALU
C
      INTEGER*4 NAMF(20),NAME(20)
      CHARACTER*80 CNAME
C
      EQUIVALENCE (CNAME,NAME)
C
      CHARACTER*4  IEXT
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO OPEN BAN- & BED-FILES
C     ------------------------------------------------------------------
C
      DO 10 I=1,20
      NAME(I)=NAMF(I)
   10 CONTINUE
C
      IERR=0                                !ERROR FLAG
      CLOSE(UNIT=LU)                        !CLOSE FILE
      IF(LU.EQ.1) GO TO 100                 !TST FOR INPUT REQ
C
      IF(IEXT.EQ.'.bed') GO TO 50           !TST FOR BED-FILE
      IF(IEXT.EQ.'.BED') GO TO 50           !TST FOR BED-FILE
C
      OPEN(UNIT   = LU,                     !CREATE NEW BAN-FILE
     &     FILE   = CNAME,                  !FOR OUTPUT
     &     STATUS = 'NEW',
     &     ACCESS = 'DIRECT',
     &     RECL   = RECLVALU(80),
     &     IOSTAT = IOS)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   GO TO 200
                   ENDIF
C
      CALL BANIO(0,LOU,FIL,TIT,IH,0,DG,IX,JY,NP,NID,KPAR,MSG,
     &IERR)
      CALL BANERR(IERR,MSG,0)
      IF(IERR.NE.0) GO TO 200
C
      RETURN
C
   50 OPEN(UNIT   = LU,                     !CREATE NEW BED-FILE
     &     FILE   = CNAME,                  !FOR OUTPUT
     &     STATUS = 'NEW',
     &     ACCESS = 'SEQUENTIAL',
     &     IOSTAT = IOS)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   GO TO 200
                   ENDIF
C
      RETURN
C
  100 IF(IEXT.EQ.'.bed') GO TO 150
      IF(IEXT.EQ.'.BED') GO TO 150
C
      OPEN(UNIT   = LU,                     !OPEN BAN-FILE
     &     FILE   = CNAME,                  !FOR INPUT
     &     STATUS = 'OLD',
     &     ACCESS = 'DIRECT',
     &     RECL   = RECLVALU(80),
     &     IOSTAT = IOS)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   GO TO 200
                   ENDIF
      RETURN
C
  150 OPEN(UNIT   = LU,                     !OPEN  BED-FILE
     &     FILE   = CNAME,                  !FOR INPUT
     &     STATUS = 'OLD',
     &     ACCESS = 'SEQUENTIAL',
     &     IOSTAT = IOS)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   GO TO 210
                   ENDIF
C
      RETURN
C
  200 WRITE(CMSSG,205)
  205 FORMAT('ERROR CREATING FILE')
      GO TO 300
  210 WRITE(CMSSG,215)
  215 FORMAT('ERROR OPENING FILE')
C
  300 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
