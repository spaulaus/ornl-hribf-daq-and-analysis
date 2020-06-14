C$PROG HISMIL
      SUBROUTINE HISMIL(IERR,MSER)
C
      LOGICAL Q
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
      INTEGER*2 MIL(65536),IPATCH(200)
C
      INTEGER*4 MSER(10),MSG(10,2)
      CHARACTER*40 MSC(2)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
      EQUIVALENCE (MIL(1),MILF(1)),(MC,MILC),(MCF,MILCF)
C
      DATA MSC/'OH HIST LENGTH NOT SAME AS PREVIOUS H   ',
     &         '                                        '/
C
      DATA LSDAD,LSDADI/0,0/
C
      CHARACTER*4  KHT
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0                            !RESET ERROR FLAG
      LPAT=0                            !ZERO PATCH LOCATION INDEX
      MPOR=1                            !RESET PRODUCT OF PREVIOUS
C                                       !MULTIPLE OR'S
      MC=MILC+1                         !INC THE MIL-CNTR
      MIL(MC)=28                        !LOAD ACC
                                                   CALL QQ(0,28)
      MC=MC+1
      MIL(MC)=0                         !WITH 0
                                                   CALL QQ(0,1)
C
      DO 500 JC=1,NCON                  !LOOP ON # OF CONDITIONS
C
      MC=MC+1                           !INC THE MIL-CNTR
      MARK=2*(MC-1)                     !MARK THE NEXT MIL-LOCATION
C
      IF(ICTY(JC).EQ.'SING') GO TO 100
      IF(ICTY(JC).EQ.'LIST') GO TO 200
      IF(ICTY(JC).EQ.'BAN ') GO TO 300
      IF(ICTY(JC).EQ.'MAP ') GO TO 400
C
C     **************************************************************
C     SINGLE IN-LINE GATE
C     **************************************************************
C
  100 MIL(MC)=30                        !LOAD OP-CODE
                                                   CALL QQ(0,30)
      MIL(MC+1)=2*(ITPR(1,JC)-1)        !LOAD TST-PARM DISP
                                                   CALL QQ(1,2)
      MIL(MC+2)=MARK                    !LOAD MARK IN FAIL DISP LOC
                                                   CALL QQ(2,3)
      LPAT=LPAT+1                       !INC  PATCH LOCATION INDEX
      IPATCH(LPAT)=MC+2                 !SAVE LOCATION TO PATCH
      MIL(MC+3)=12                      !LOAD HIT DISP
                                                   CALL QQ(3,4)
      MIL(MC+4)=LIMS(2,JC)              !LOAD HI-LIMIT
                                                   CALL QQ(4,36)
      MIL(MC+5)=LIMS(1,JC)              !LOAD LO-LIMIT
                                                   CALL QQ(5,37)
C
      MC=MC+5                           !INC MIL-CNTR
      GO TO 500
C
C     **************************************************************
C     MULTIPLE GATE LIST (NOT IN LINE)
C     **************************************************************
C
  200 MIL(MC)=22                        !LOAD OP-CODE
                                                   CALL QQ(0,22)
      MIL(MC+1)=2*(ITPR(1,JC)-1)        !LOAD TST PARM DISP
                                                   CALL QQ(1,2)
      MIL(MC+2)=MARK                    !LOAD MARK IN FAIL DISP LOC
                                                   CALL QQ(2,3)
      LPAT=LPAT+1                       !INC  PATCH LOCATION INDEX
      IPATCH(LPAT)=MC+2                 !SAVE LOCATION TO PATCH
      MIL(MC+3)=16                      !LOAD HIT DISP
                                                   CALL QQ(3,4)
      MIL(MC+4)=MPOR                    !LOAD PARTIAL MISS INCREMENT
                                                   CALL QQ(4,5)
      MIL(MC+5)=-NORS(JC)               !LOAD MINUS # OF GATES
                                                   CALL QQ(5,6)
      MCF=(MC+7)/2                      !FULL WORD INDEX IN MILF
      MILF(MCF)=2*(LOCG(JC)-1)-MARK     !LOAD GATE-LIST DISP
                                                   CALL QQ(0,-7)
C
      MC=MC+7                           !INC  MIL-CNTR
      MPOR=MPOR*NORS(JC)                !CALC NEW MULT-OR'S PRODUCT
      GO TO 500
C
C     **************************************************************
C     BAN-LIST
C     **************************************************************
C
  300 MIL(MC)=23                        !LOAD OP-CODE
                                                   CALL QQ(0,23)
      MIL(MC+1)=2*(ITPR(2,JC)-1)        !LOAD 1ST TST-PARM (PY) DISP
                                                   CALL QQ(1,8)
      MIL(MC+2)=MARK                    !LOAD MARK IN FAIL DISP LOC
                                                   CALL QQ(2,3)
      LPAT=LPAT+1                       !INC  PATCH LOCATION INDEX
      IPATCH(LPAT)=MC+2                 !SAVE LOCATION TO PATCH
      MIL(MC+3)=24                      !LOAD HIT DISP
                                                   CALL QQ(3,4)
      MIL(MC+4)=MPOR                    !LOAD PARTIAL MISS INCREMENT
                                                   CALL QQ(4,5)
      MIL(MC+5)=-NORS(JC)               !LOAD MINUS # OF BANANAS
                                                   CALL QQ(5,10)
      MIL(MC+6)=LIMS(1,JC)              !LOAD RIGHT-SHIFT CNT FOR PX
                                                   CALL QQ(6,11)
      MIL(MC+7)=2*(ITPR(1,JC)-1)        !LOAD 2ND TST-PARM DISP (PX)
                                                   CALL QQ(7,9)
      MIL(MC+8)=0                       !LOAD 0 FOR ALIGNMENT
                                                   CALL QQ(8,13)
      MIL(MC+9)=LIMS(2,JC)              !LOAD SIZE OF EACH BAN-VECT
                                                   CALL QQ(9,12)
      MCF=(MC+11)/2                     !FULL WORD INDEX IN MILF
      MILF(MCF)=4*(LOCC(JC)-1)-MARK     !LOAD FULL-WD DISP OF BAN-LIST
                                                   CALL QQ(0,-7)
C
      MC=MC+11                          !INC  MIL-CNTR
      MPOR=MPOR*NORS(JC)                !CALC NEW #OR'S-PRODUCT
      GO TO 500
C
C     **************************************************************
C     MAPPED GATE-LIST
C     **************************************************************
C
  400 IF(MPOR.GT.1) GO TO 420           !TST FOR NEED TO SAVE ACC
C
C     MAPPED GATE - NO PREVIOUS MULTIPLE OR'S **********************
C
      MIL(MC)=29                        !LOAD OP-CODE
                                                   CALL QQ(0,29)
      MIL(MC+1)=2*(ITPR(1,JC)-1)        !LOAD TST-PARM DISP
                                                   CALL QQ(1,2)
      MIL(MC+2)=MARK                    !LOAD MARK IN FAIL DISP LOC
                                                   CALL QQ(2,3)
      LPAT=LPAT+1                       !INC  PATCH LOCATION INDEX
      IPATCH(LPAT)=MC+2                 !SAVE LOCATION TO PATCH
      MIL(MC+3)=16                      !LOAD HIT DISP
                                                   CALL QQ(3,4)
      MIL(MC+4)=LIMS(1,JC)              !LOAD SHIFT-CNT FOR TST-PARM
                                                   CALL QQ(4,11)
      MIL(MC+5)=LIMS(2,JC)              !LOAD SIZE FOR TST-PARM
                                                   CALL QQ(5,12)
      MCF=(MC+7)/2                      !FULL WORD INDEX IN MILF
      MILF(MCF)=4*(LOCC(JC)-1)-MARK     !LOAD FULL-WD DISP OF MAP
                                                   CALL QQ(0,-7)
C
      MC=MC+7                           !INC MIL-CNTR
      MPOR=MPOR*NORS(JC)                !CALC NEW OR'S PRODUCT
      GO TO 500
C
C     MAPPED GATE - PREVIOUS MULTIPLE OR'S - SAVE ACC **************
C
  420 MIL(MC)=17                        !LOAD OP-CODE TO SAVE ACC
                                                   CALL QQ(0,17)
      MIL(MC+1)=-2                      !LOAD V-PAR DISP
                                                   CALL QQ(1,2)
      MC=MC+2                           !INC  MIL-CNTR
      MARK=2*(MC-1)                     !MARK THE NEXT MIL LOCATION
C
      MIL(MC)=29                        !LOAD OP-CODE
                                                   CALL QQ(0,29)
      MIL(MC+1)=2*(ITPR(1,JC)-1)        !LOAD TST-PARM DISP
                                                   CALL QQ(1,2)
      MIL(MC+2)=MARK                    !LOAD MARK IN FAIL DISP LOC
                                                   CALL QQ(2,3)
      LPAT=LPAT+1                       !INC  PATCH LOCATION INDEX
      IPATCH(LPAT)=MC+2                 !SAVE LOCATION TO PATCH
      MIL(MC+3)=16                      !LOAD HIT DISP
                                                   CALL QQ(3,4)
      MIL(MC+4)=LIMS(1,JC)              !LOAD SHIFT-CNT FOR TST-PARM
                                                   CALL QQ(4,11)
      MIL(MC+5)=LIMS(2,JC)              !LOAD SIZE FOR TST-PARM
                                                   CALL QQ(5,11)
      MCF=(MC+7)/2                      !FULL WORD INDEX IN MILF
      MILF(MCF)=4*(LOCC(JC)-1)-MARK     !LOAD FULL-WD DISP OF MAP
                                                   CALL QQ(0,-7)
C
      MIL(MC+8)=21                      !LOAD OP-CODE TO MUL ACC
                                                   CALL QQ(8,21)
      MIL(MC+9)=MPOR                    !BY PRODUCT OF PREVIOUS OR'S
                                                   CALL QQ(9,5)
C
      MIL(MC+10)=20                     !LOAD OP-CODE TO ADD VIRTUAL
                                                   CALL QQ(10,20)
      MIL(MC+11)=-2                     !PARM TO ACC
                                                   CALL QQ(11,2)
C
      MC=MC+11                          !INC  MIL-CNTR
      MPOR=MPOR*NORS(JC)                !CALC NEW #OR'S PRODUCT
  500 CONTINUE
C
C     **************************************************************
C     ROUNDUP - NO MULTIPLE HISTOGRAMMING FOR NOW
C     **************************************************************
C
      JH=NHP                            !MOST SIGNIFICANT H-PARM
C
      DO 610 IH=1,NHP                   !LOOP ON # H-PARMS
      MC=MC+1                           !INC  THE MIL-CNTR
      MARK=2*(MC-1)                     !MARK THE NEXT MIL-LOCATION
      ICO=25                            !CODE FOR SUBSEQUENT H-PARM
      IF(JH.NE.NHP) GO TO 605           !TST FOR 1ST H-PARM
      IF(MPOR.GT.1) GO TO 605           !TST FOR ANY MULTIPLE OR'S
      ICO=24                            !SET CODE FOR 1ST H-PARM
  605 MIL(MC)=ICO                       !LOAD OP-CODE
                                                   CALL QQ(0,ICO)
      MIL(MC+1)=MARK                    !LOAD MARK IN FAIL DISP LOC
                                                   CALL QQ(1,3)
      LPAT=LPAT+1                       !INC  PATCH LOCATION INDEX
      IPATCH(LPAT)=MC+1                 !SAVE LOCATION TO PATCH
      NDX=KHP(JH,1)                     !GET INDEX OF H-PARM
      MIL(MC+2)=LOGB2(LPAR(NDX)/LHP(JH))!LOAD RIGHT-SHIFT COUNT
                                                   CALL QQ(2,11)
      MIL(MC+3)=2*(KHP(JH,1)-1)         !LOAD H-PARM DISP
                                                   CALL QQ(3,38)
      MIL(MC+4)=KRA(1,JH)               !LOAD MIN
                                                   CALL QQ(4,14)
      MIL(MC+5)=KRA(2,JH)-KRA(1,JH)+1   !LOAD MAX# FINAL CHANNELS
                                                   CALL QQ(5,15)
      JH=JH-1                           !DEC  H-PARM # INDEX
      MC=MC+5                           !INC  MIL-CNTR
  610 CONTINUE
C
      ICO=26                            !SET FOR 16 BIT PER CHANNEL
      IF(NHWPC.EQ.2) ICO=27             !TST FOR 32 BIT PER CHANNEL
      MC=MC+1                           !INC  MIL-CNTR
      MIL(MC)=ICO                       !OP-CODE TO INC (16 OR 32)
                                                   CALL QQ(0,ICO)
      MC=MC+1                           !INC  MIL-CNTR
      MIL(MC)=0                         !PAD
                                                   CALL QQ(0,13)
      MC=MC+2                           !INC MIL-CNTR BY 2 (FULL WD)
      MCF=MC/2                          !CALC MILF INDEX
      KDAD=NXDAD                        !HIST BASE ADDR FOR "H"
      IF(KHT.EQ.'OH  ') KDAD=LSDAD      !HIST BASE ADDR FOR "OH"
      MILF(MCF)=KDAD                    !LOAD HIST BASE ADDR IN MILF
                                                   CALL QQ(0,-16)
C                                       !MUST BE FULL-WD ALLIGNED
      MISSB=2*MC                        !BYTE # OF NEXT MIL-LOCATION
C
      LHIS=1                            !SET UP TO CALC SIZE OF ONE
C                                       !HISTOGRAM
      DO 620 I=1,NHP                    !LOOP TO COMPUTE PRODUCT OF
      LHIS=LHIS*(KRA(2,I)-KRA(1,I)+1)   !ALL HISTOGRAM DIMENSIONS
  620 CONTINUE
C
      LHISH=NHWPC*LHIS                  !LENGTH OF 1 HIST IN HALF-WDS
      CALL HISLOG(KDAD,LHISH,IERR,MSER) !PRODUCE SUMMARY & DIRECTORY
C
      NUDADI=MPOR*NHWPC*LHIS            !CALC TOTAL DISK INCREMENT
      IF(KHT.EQ.'H   ') GO TO 630
      IF(NUDADI.NE.LSDADI) GO TO 1010   !TST OH-TYPE FOR RIGHT LENG
      GO TO 640                         !AND DO NOTHING ELSE
C
  630 LSDAD=NXDAD                       !SAVE LAST BASE ADDRESS
      NXDAD=NXDAD+NUDADI                !CALC NEW  BASE ADDRESS
      LSDADI=NUDADI                     !SAVE TOTAL DISK INCREMENT
C
  640 DO 650 I=1,LPAT                   !LOOP TO PATCH "MISS-MARK"
      NDX=IPATCH(I)                     !GET LOCATION TO PATCH
      MIL(NDX)=MISSB-MIL(NDX)           !DO THE PATCH (ALL IN BYTES)
  650 CONTINUE
C
      MILC=MC                           !LAST USED MIL-LOCATION
      MILCF=MILC/2
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP MESSAGE
C     **************************************************************
C
 1010 JJ=1
C
      IERR=JJ
      DO 1210 I=1,10
      MSER(I)=MSG(I,JJ)
 1210 CONTINUE
      RETURN
      END
