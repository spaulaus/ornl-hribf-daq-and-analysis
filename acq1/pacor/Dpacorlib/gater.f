C$PROG GATER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE GATER(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXG=100)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      CHARACTER*4  KIMO,GTYP,CALCALL
C
      INTEGER*4 PNAM(4)
C
      REAL*4 XV
C
      DATA NGAT,NGRED,NBOO,NCALL,NPATWD/0,0,0,0,0/
C
      DATA PATNO,MSKNO/0,0/
C
      integer*4 SETW
      character*4 cSETW
      equivalence (cSETW, SETW)
      DATA cSETW/'SETW'/
C
      INTEGER*4   YES,CGA
      character*4 cYES, cCGA
      equivalence (cYES,YES), (cCGA,CGA)
      DATA        cYES,cCGA/'YES ','$CGA'/
C
      SAVE
C
C     ************************************************************
C     DEFINITIONS FOR COMMON/PACJ/ - FOR ITH GATE
C     ************************************************************
C     GNAM(M,I),M=1,3 = GATE NAME (ASCII) (RAW OR CALCULATED)
C     GNAM(4,I)       = GATE NAME INDEX
C     GTYP(I)         = GATE TYPE = 'RAW' OR 'CAL'
C     PATN(I)         = ASSOCIATED PATTERN WORD# 
C     GMSK(I)         = ASSOCIATED MASK
C
C     GLO(J)          = GATE LO-LIMIT
C     GHI(J)          = GATE HI-LIMIT
C     LPTR(I)         = POINTER TO AUX LIST, COMMON/PACK/ FOR CAL 
C     RPTR(I)         = POINTER TO READ   LIST (INDEX IN GCNAF)
C
C     MPTR(I)         = POINTER TO MODULE LIST (INDEX IN /PACJ/)
C     GCNAF(M,I),M=1,5= RAW-GATE READ-LIST (C,N,A,F,MOTY)        
C     NENT(I)         = # OF ENTRIES FOR /PACK/ DATA
C
C     NGAT            = TOTAL# (RAW + CAL) GATE ENTRIES
C     NGRED           = # GATE-PARMS TO READ (# ENTRIES IN GCNAF)
C
C     PATNO           = CURRENT PATTERN WORD# 
C     MSKNO           = CURRENT MASK (SPECIFIES BIT TO SET)
C
C     ************************************************************
C
      IF(NCALL.GT.0) GO TO 50
C
C     ************************************************************
C     INITIALIZE NPATWD (LAST PATTERN WORD# USED ON FIRST CALL)
C     NOTE: ALL INDICES ARE INCREMENTED BY 1 IN POBGEN
C     ************************************************************
C
      NPATWD=0
      DO 10 I=1,NUMT
      IF(KIMO(I).EQ.'$LAT') NPATWD=NPATWD+1
   10 CONTINUE
      PATNO=NPATWD
      MSKNO=32768
      NCALL=1
C
C     ************************************************************
C     TEST FOR RAW ($RGA) OR CALCULATED ($CGA) REQUEST
C     ************************************************************
C
   50 IF(IWD(1).EQ.CGA)  THEN
      CALL GATECMP(IWD)
      CALCALL='YES '
      RETURN
      ENDIF
C
C     ************************************************************
C     PROCESS $RGA REQUEST
C
C     $RGAT GNAM(I),PNAM(J),LO,HI
C     ************************************************************
C
      IF(CALCALL.EQ.'YES ') THEN
      WRITE(CMSSG,55)
   55 FORMAT('Calculated gate specs MUST follow raw gate specs')
      CALL ERRLOG(LOGUT,LOGUP)
      ENDIF
C
      IF(NGAT.GE.MXG) GO TO 510               !TST FOR TABLE OVERFLO
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER)!SET FIELD WIDTH=12
C
      CALL GREAD(IWD,LWD,ITYP,NF,6,80,NTER)   !REFORMAT INPUT LINE
C
      IF(NTER.NE.0) GO TO 520                 !TST FOR ERROR
C
      CALL MILV3(LWD(1,2),GDX,XV,KIND,IERR)   !GATE-NAME INDEX   
      IF(IERR.NE.0) GO TO 530
      CALL MILV3(LWD(1,4),PDX,XV,KIND,IERR)   !PARM-NAME INDEX
      IF(IERR.NE.0) GO TO 530
      CALL MILV3(LWD(1,5),LO, XV,KIND,IERR)   !GATE LO-LIMIT
      IF(IERR.NE.0) GO TO 530
      CALL MILV3(LWD(1,6),HI, XV,KIND,IERR)   !GATE HI-LIMIT
      IF(IERR.NE.0) GO TO 530
C
      NGAT=NGAT+1                             !INC #GATES CNTR
      GTYP(NGAT)='RAW '                       !GATE-TYPE = RAW
      LPTR(NGAT)=0
      DO 110 I=1,3
      GNAM(I,NGAT)=LWD(I,1)                   !LOAD GATE-NAME
      PNAM(I)     =LWD(I,3)                   !LOAD PARM-NAME
  110 CONTINUE
      GNAM(4,NGAT)=GDX                        !LOAD GATE-INDEX
      PNAM(4)     =PDX                        !LOAD PARM-INDEX
      GLO(NGAT)   =LO                         !LOAD LO-LIMIT
      GHI(NGAT)   =HI                         !LOAD HI-LIMIT
C
      IF(NGAT.GT.1) THEN
      CALL NAMLOC(GNAM,GNAM(1,NGAT),NGAT-1,IDX)     !TST FOR MULT DEF
      IF(IDX.GT.0) GO TO 540
                    ENDIF
C
      IF(LEGLNAM(GNAM(1,NGAT)).NE.YES) GO TO 550 !TST FOR LEGAL
C
      CALL NAMLOC(NAMO,PNAM,NUMT,NDX)         !LOCATE PARM-NAME 
      IF(NDX.LE.0) GO TO 560
C
      MPTR(NGAT)=NDX                          !PARM-LIST INDEX
C
      CALL NEWMSK(PATNO,MSKNO)                !GET PAT-WD# & MASK
C
      PATN(NGAT)=PATNO                        !LOAD PATTERN WD#
      GMSK(NGAT)=MSKNO                        !LOAD MASK
C
C     ************************************************************
C     TEST FOR SPECIFIED PARAMETER ALREADY IN READ-LIST
C     ************************************************************
C
      NDO=NGAT-1                   !TST PREVIOUS PARM-LIST PNTRS
      DO 130 I=1,NDO               !FOR MATCHING CURRENT PARM-INDEX
      IF(NDX.EQ.MPTR(I)) THEN      !IF MATCH, THEN USE SAME 
      RPTR(NGAT)=RPTR(I)           !READ-LIST POINTER
      GO TO 1000                   !AND DON'T ADD TO READ-LIST
      ENDIF
  130 CONTINUE
C
      NGRED=NGRED+1                !IF NO MATCH, INC READ-LIST PNTR
      RPTR(NGAT)=NGRED             !LOAD READ-LIST POINTER
      GCNAF(1,NGRED)=CRAT(NDX)     !LOAD C
      GCNAF(2,NGRED)=SLOT(NDX)     !LOAD N
      GCNAF(3,NGRED)=SUBA(NDX)     !LOAD A
      GCNAF(4,NGRED)=FRED(NDX)     !LOAD F
      GCNAF(5,NGRED)=MOTY(1,NDX)   !LOAD MODULE-TYPE CODE

C
      GO TO 1000
C
C     ************************************************************
C     SEND ANY ERROR MESSAGES
C     ************************************************************
C
  510 WRITE(CMSSG,515)NGAT
  515 FORMAT('GATE TABLE OVERFLOW AT - ',I6)
      GO TO 800
  520 WRITE(CMSSG,525)
  525 FORMAT('TRUNCATION ERROR FROM - GREAD')
      GO TO 800
  530 WRITE(CMSSG,535)
  535 FORMAT('SYNTAX ERROR FROM MILV3')
      GO TO 800
  540 WRITE(CMSSG,545)(GNAM(I,NGAT),I=1,4)
  545 FORMAT('MULTIPLE DEFINITION OF GATE NAME = ',3A4,I4)
      GO TO 800
  550 WRITE(CMSSG,555)GNAM(1,NGAT)
  555 FORMAT('ILLEGAL GATE NAME = ',A4)
      GO TO 800
  560 WRITE(CMSSG,565)PNAM
  565 FORMAT('FROM GATER - PARM NAME NOT FOUND = ',3A4,I4)
      GO TO 800
C
  800 CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)    !RESET WIDTH TO 8
      RETURN
      END
