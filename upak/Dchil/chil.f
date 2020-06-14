C$PROG CHIL
C     PROGRAM - CHIL  (STANDARD UNIX VERSION)
C
      LOGICAL Q
C
      INTEGER*2 GSID,GFOR,GSOR,GNOG,GLEN,MLEN,GAPO
C
      INTEGER*2 GXNX,GXLN
C
      INTEGER*2 BNID,BLEN
C
      INTEGER*4 GLOC,GXLOC,BLOC
      CHARACTER*80 CARG(2),NAMMIL,NAMCHL,NAMDRR,NAMPRT
      INTEGER*4 NARG(20,2)
      EQUIVALENCE (NARG,CARG)
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
      CHARACTER*4        CLWD(2,40)
      EQUIVALENCE       (CLWD,LWD)
C
      COMMON/BBB/ BNID(880),BLEN(880),BLOC(880),MXBAN,NBAN
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      COMMON/GGG/ GSID(128),GFOR(128),GSOR(128),GNOG(128),GLEN(128),
     &GLOC(128),MLEN(128),MLOC(128),GAPO(128),NGSET
C
      COMMON/HHH/ KHP(4,20),LHP(4),KRA(2,4),KGS(2,4),KHT,NHP,NHS,
     &            ICTY(100),NORS(100),LOCC(100),LOCG(100),ITPR(2,100),
     &            LIMS(2,100),IFBID(100),NCON
C
      COMMON/III/ GXNX(128),GXLN(128),GXLOC(128),NGXN
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
      CHARACTER*4 NUBPC
C
      COMMON/OOO/ LLLST(40),NULST(40),NLLST
C
      COMMON/PPP/ LPATCH(500),LAPAT(500),LPATI
C
      COMMON/QQQ/ ITEX(20),ITIT(10),NUID
      CHARACTER*4 ITEX,    ITIT
C
      COMMON/XXX/ IBANF,IBAFF,IGATF,IGAFF
      CHARACTER*4 IBANF,IBAFF,IGATF,IGAFF
C
      COMMON/YYY/ NAMHIL(20),LISKIN,LTA,LICO,NOHIS,ITRACE
      CHARACTER*4 NAMHIL,    LISKIN,         NOHIS
C
      COMMON/ZZZ/ JHPC(4096),LISTL,IREPACK,JREPACK(3)
      CHARACTER*4                  IREPACK,JREPACK
C
      INTEGER*2 MIL(65536)
C
      INTEGER*4    MSER(10),LINX(33)
      CHARACTER*4  OPLST(6)
      CHARACTER*24 OPLSTC
C
      EQUIVALENCE (OPLSTC,OPLST(1))
C
      EQUIVALENCE (MIL(1),MILF(1))
C
      DATA OPLSTC/'NOS NOT NOC FUL MIL NOTR'/
C
      CHARACTER*4  JOP
C
C     **************************************************************
      INTEGER*4    RECLVALU
C     **************************************************************
C
C     LOGICAL UNIT ASSIGNMENTS
C
C     UNIT 01 -  LIN  - FIL.chl - CHIL SOURCE FILE
C     UNIT 02 -  LOU  - SCRATCH FILE FOR PRE-PROCESSED SOURCE
C     UNIT 03 -  LER  - SCRATCH FILE FOR ERROR MESSAGES
CXXXXXUNIT 04 -  4    - FIL.OPT - CONTAINS "START STRING" INFO (UNIX NOT USED)
C     UNIT 00 -  0    - TERMINAL (UNIX Set to 0 == standard error was 5 on VAX)
C     UNIT 06 -  LU6  - LIST FILE/DEVICE (UNIX standard output. def is terminal)
C     UNIT 08 -  LTA  - SCRATCH FILE FOR HISTOGRAM TABLE
C     UNIT 09 -  LTA  - NULL    FILE FOR HISTOGRAM TABLE
C     UNIT 10 -  LDR  - DIR.TMP;1 - RENAMED TO FIL.drr
C     UNIT 11 -  LML  - MIL.TMP;1 - RENAMED TO FIL.mil
C     UNIT 13 -  LU   - OPENED BY ROUTINES NUGAT & NUBAN
C
C     **************************************************************
C
      OPEN(UNIT=2, STATUS='SCRATCH',ACCESS='SEQUENTIAL',
     &             RECL=128,FORM='UNFORMATTED')
C
      OPEN(UNIT=3, STATUS='SCRATCH',ACCESS='SEQUENTIAL',
     &             RECL=40 ,FORM='UNFORMATTED')
C
C     OPEN(UNIT=4, STATUS='OLD',ACCESS='SEQUENTIAL',
C    &             RECL=80 ,FORM='FORMATTED')
C
C     OPEN(UNIT=5, FILE='/dev/tty')
      OPEN(UNIT=8, STATUS='SCRATCH',ACCESS='SEQUENTIAL',
     &             RECL=140,FORM='FORMATTED')
C
      OPEN(UNIT=9, STATUS='SCRATCH',ACCESS='SEQUENTIAL',
     &             RECL=140,FORM='FORMATTED')
C
C
C     **************************************************************
C
C     **************************************************************
C     GSID(I) = ID# FOR     ITH GATE SET
C     GFOR(I) = FORM OF     ITH GATE-SET ('LI'/'MA' = LIST/MAP)
C     GSOR(I) = SOURCE OF   ITH GATE-SET ('IN'/'GF' = CHIL/FILE)
C     GNOG(I) = # GATES IN  ITH GATE-SET
C     GLEN(I) = LENGTH FOR  ITH GATE-SET (BASIS ON WHICH DEFINED)
C     GLOC(I) = HALF-WD INDEX OF ITH GATE-SET IN MIL (UN-MAPPED)
C     MLOC(I) = HALF-WD INDEX OF ITH GATE-SET IN MIL (MAPPED)
C     MLEN(I) = MAP LENGTH (BASIS) FOR ITH GATE-SET
C     NGSET   = # OF GATE-SETS SPECIFIED
C
C     (MILF(I),I=1    ,32768)  - IS THE INS -REGION OF MIL
C     (MILF(I),I=32769,40960)  - IS THE GATE-REGION OF MIL
C     (MILF(I),I=40961,262144) - IS THE MAP -REGION OF MIL
C
C     IGATOF  = GATE-REGION OFFSET IN MIL (IN HALF-WDS) = 65536
C     IMAPOF  = MAP -REGION OFFSET IN MIL (IN HALF-WDS) = 81920
C
C     NGATL   = # OF HALF-WDS LOADED INTO GATE-REGION OF MIL
C     NMAPL   = # OF HALF-WDS LOADED INTO MAP -REGION OF MIL
C
C     MXGATL  = MAX # HALF-WDS IN GATE-REGION
C     MXMAPL  = MAX # HALF-WDS IN MAP -REGION
C     **************************************************************
C
      DO 10 I=1,20
      NAMHIL(I)='    '
   10 CONTINUE
C
      Q=.FALSE.
      LISKIN='    '
      LTA=8
C
      NO_ARGS=IARGC()			!UNIX
      CARG(1)=' '
      CARG(2)=' '
      IF(NO_ARGS.LT.1)GOTO 20		!UNIX
      IF(NO_ARGS.GT.2)NO_ARGS=2		!UNIX
      DO I=1,NO_ARGS			!UNIX
         CALL GETARG(I,CARG(I))		!UNIX
      ENDDO				!UNIX	
C 
      IA=1
      IB=NXBL(NARG(1,1),IA,79)-1
      NAMMIL=CARG(1)
      NAMMIL(IB+1:)='.mil'
      NAMCHL=CARG(1)
      NAMCHL(IB+1:)='.chl'
      NAMDRR=CARG(1)
      NAMDRR(IB+1:)='.drr'
      NAMPRT=CARG(1)
      NAMPRT(IB+1:)='.prt'
C
      OPEN(UNIT=1, STATUS='OLD',ACCESS='SEQUENTIAL',
     &            FILE=NAMCHL,ERR=27)
C
      OPEN(UNIT=6,STATUS='UNKNOWN',ACCESS='SEQUENTIAL',
     &            FILE=NAMPRT)
  
C
      OPEN(UNIT       = 10,
     &     STATUS     = 'UNKNOWN',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(128),
     &     FORM       = 'UNFORMATTED',
     &     FILE       = NAMDRR)
C
      OPEN(UNIT       = 11,
     &     STATUS     = 'UNKNOWN',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(256),
     &     FORM       = 'UNFORMATTED',
     &     FILE       = NAMMIL)
C
      CALL LODUP(NARG(1,1),IA,IB,NAMHIL,1)
C
C     **************************************************************
C     PROCESS START STRING OPTIONS
C     **************************************************************
C
C      IA=IB+1
C     CALL REFOR(IWD,LWD,ITYP,NF,IA,80,NTER)
      IA=1
      NF=0
      CALL REFOR(NARG(1,2),LWD,ITYP,NF,IA,80,NTER)
      IF(NTER.EQ.0) GO TO 50
C
   20 WRITE(0,22)
      WRITE(0,24)
      WRITE(0,26)
   22 FORMAT(1H ,'ILLEGAL START OPTIONS')
   24 FORMAT(1H ,'LEGAL CMD IS: CHIL FILEPREFIX,<LIST FIL/DEV>,<OP1 OP2
     &OP3 ..>')
   26 FORMAT(1H ,'LEGAL OPTIONS ARE - NOS NOT NOC NOTR FUL MIL')
      STOP
   27 WRITE(0,*)'ERROR OPENING CHIL FILE',NAMCHL
      STOP
C
   50 DO 70 N=1,NF                      !LOOP ON # START OPTIONS
C
      JOP=CLWD(1,N)                     !PICK UP OPTION
      DO 55 I=1,6                       !TST OPTIONS FOR LEGAL
      IF(JOP.EQ.OPLST(I)) GO TO 60
   55 CONTINUE
      GO TO 20                          !GO DISPLAY ERROR MSG
C
   60 IF(JOP.EQ.'MIL ') Q=.TRUE.        !REQUEST MIL LISTING
      IF(JOP.EQ.'FUL ') LISKIN='FULL'   !EXPANDED SOURCE LISTING
      IF(JOP.EQ.'NOS ') LISKIN='NULL'   !NO SOURCE LISTING
      IF(JOP.EQ.'NOT ') LTA=9           !NO HISTOGRAM TABLE
   70 CONTINUE
C
C     **************************************************************
C     INITIALIZE SOME STUFF
C     **************************************************************
C
      DO 120 I=1,20
      ITEX(I)='    '
  120 CONTINUE
      DO 130 I=1,10
      ITIT(I)='    '
  130 CONTINUE
C
      DO 140 I=1,4096
      JHPC(I)=0
  140 CONTINUE
C
      NUID=0
C
      NLIN=0
      NOHIS='NO  '
C
      NERR=0
      NEREC=0
      NSOL=0
      NSYM=0
      NSYV=0
C
      LIN=1
      LOU=2
      LER=3
      LU6=6
C
      MXBAN=880
      MXPAR=2000
C
      IGATOF=65536
      IMAPOF=81920
      MXGATL=16384
      MXMAPL=442368
C
      LISTL=8192
C
      NGSET=0
      NBAN=0
      NGXN=0
      NGATL=0
      NMAPL=0
C
      MILC=0
      MILCF=0
      LPATI=0
      NHWPC=1
      NXDAD=0
      IREPACK='NO  '
      JREPACK(1)='NO  '
      JREPACK(2)='NO  '
      JREPACK(3)='NO  '
      NUBPC='NO  '
C
      CALL PASS1
C
      CALL PASS2
C
      MIL(MILC+1)=32                    !OPCODE TO LEAVE WCS
                                                   CALL QQ(1,32)
      MIL(MILC+2)=0                     !END OF MIL
                                                   CALL QQ(2,43)
      MILC=MILC+2
C
C     **************************************************************
C     PATCH ALL LABEL REFERENCE DISPLACEMENTS IN "MIL"
C     **************************************************************
C
      DO 160 I=1,LPATI
      NDX=LPATCH(I)
      CALL LAVAL(LAPAT(I),IV,IERR,MSER)
C
      IF(IERR.NE.0)  THEN
                     WRITE(0,165)MSER
                     WRITE(6,165)MSER
                     NERR=NERR+1
                     GO TO 160
                     ENDIF
C
      IDISP=IV-MIL(NDX)
C
      IF(IDISP.LE.0) THEN
                     WRITE(0,170)LAPAT(I)
                     WRITE(6,170)LAPAT(I)
                     NERR=NERR+1
                     GO TO 160
                     ENDIF
C
      MIL(NDX)=IDISP
C
  160 CONTINUE
C
  165 FORMAT(1H ,'* * *',10A4)
  170 FORMAT(1H ,'* * * ILLEGAL BACKWARD REFERENCE TO LABEL ',A4)
C
      IF(Q)CALL LISMIL
C
      IF(NERR.NE.0) WRITE(0,180)
      IF(NERR.NE.0) WRITE(6,180)
  180 FORMAT(1H ,'! ! ! ! COMPILATION ERRORS ! ! ! !')
      IF(NERR.NE.0) STOP
      IF(LTA.NE.8)  GO TO 250
C
      REWIND LTA
  210 READ(LTA,220,END=250)LINX
  220 FORMAT(33A4)
      WRITE(LU6,230)LINX
  230 FORMAT(33A4)
      GO TO 210
C
  250 CALL ENDIR
C
      CALL MILOUT
C
      WRITE(0,255)
      WRITE(6,255)
  255 FORMAT(1H ,'-----------------------------------------------'/)
C
      IF(IBANF.EQ.'NO  '.AND.IBAFF.EQ.'NO  ') GO TO 400
      IF(IBANF.EQ.'YES '.AND.IBAFF.EQ.'YES ') GO TO 400
C
      IF(IBANF.EQ.'OERR')  THEN
                           WRITE(0,290)
                           WRITE(6,290)
                           GO TO 400
                           ENDIF
C
      IF(IBANF.EQ.'YES ')  THEN
                           WRITE(0,300)
                           WRITE(6,300)
                           GO TO 400
                           ENDIF
C
      IF(IBAFF.EQ.'YES ')  THEN
                           WRITE(0,310)
                           WRITE(6,310)
                           GO TO 400
                           ENDIF
C
  290 FORMAT(1H ,'! ! ! WARNING - $BAF PRECEEDS $BAN - INCORRECT!')
  300 FORMAT(1H ,'! ! ! WARNING - BAN-FILE NOT SPECIFIED')
  310 FORMAT(1H ,'! ! ! WARNING - BAN-FILE GIVEN BUT NO $BAN-STATEMENT')
C
  400 IF(IGATF.EQ.'NO  '.AND.IGAFF.EQ.'NO  ') GO TO 500
      IF(IGATF.EQ.'YES '.AND.IGAFF.EQ.'YES ') GO TO 500
C
      IF(IGATF.EQ.'OERR')  THEN
                           WRITE(0,410)
                           WRITE(6,410)
                           GO TO 500
                           ENDIF
C
      IF(IGATF.EQ.'YES ')  THEN
                           WRITE(0,420)
                           WRITE(6,420)
                           GO TO 500
                           ENDIF
C
      IF(IGAFF.EQ.'YES ')  THEN
                           WRITE(0,430)
                           WRITE(6,430)
                           GO TO 500
                           ENDIF
C
  410 FORMAT(1H ,'! ! ! WARNING - $GAF PRECEEDS $GAT - INCORRECT!')
  420 FORMAT(1H ,'! ! ! WARNING - GAF-FILE NOT SPECIFIED')
  430 FORMAT(1H ,'! ! ! WARNING - GAF-FILE GIVEN BUT NO $GAT-STATEMENT')
C
  500 CLOSE(10)
      CLOSE(11)
      STOP
C
      END
