C$PROG GATECMP

C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE GATECMP(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXG=100)
      PARAMETER (MXC=500)
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/PACK/ BOOLIST(MXC),NBOO
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      INTEGER*4 RAWNAM(4)
C
      REAL*4 XV
C
      integer*4 setw
      character*4 csetw
      equivalence (csetw, setw)
      DATA cSETW,NBOO/'SETW',0/
C
      INTEGER*4   CAL,YES,RAW,NOT,AND,OR
      character*4 cCAL,cYES,cRAW,cNOT,cAND,cOR
      equivalence (cCAL, CAL), (cYES, YES), (cRAW, RAW)
      equivalence (cNOT, NOT), (cAND, AND), (cOR, OR)
      DATA        cCAL/'CAL '/
      DATA        cYES/'YES '/
      DATA        cRAW/'RAW '/
      DATA        cNOT/'NOT '/
      DATA        cAND/'AND '/
      DATA        cOR /'OR  '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     DEFINITIONS FOR COMMON/PACK/ 
C     ------------------------------------------------------------------
C     BOOLIST - CONTAINS A BOOLEAN LIST FOR CALCULATED GATES
C     THE FORM IS:
C
C     PATNDX, MASK, NOTCODE, GATNDX, OPCODE, NOTCODE, GATNDX......
C     ENDCODE, PATNDX, MASK, ....
C
C     NBOO            = NO. OF WORDS STORED IN BOOLIST 
C     ************************************************************
C
C     PROCESS: $CGAT  CNAM(J) = <N>GNAM(I) OP <N>GNAM(J) ...
C
C     ************************************************************
C
      IF(NGAT.GE.MXG) GO TO 510                !TSTFR TABLE OVERFLO
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER) !SET GREAD-FIELD=12
C
      CALL GREAD(IWD,LWD,ITYP,NF,6,80,NTER)    !REFORMAT LINE
      IF(NTER.NE.0) GO TO 520
C
      CALL MILV3(LWD(1,2),GDX,XV,KIND,IERR)    !CAL GATE-NAME INDEX
      IF(IERR.NE.0) GO TO 500
C
      NGAT=NGAT+1                              !INC GATE COUNTER
      GTYP(NGAT)=CAL                           !SET GATE-TYPE=CAL
      LPTR(NGAT)=NBOO+1                        !PNT TO NEXT BOOL INDEX
      NENT(NGAT)=0                             !RESET # BOOL ENTRIES
      DO 20 I=1,3
      GNAM(I,NGAT) =LWD(I,1)                   !LOAD GATE-NAME
   20 CONTINUE
      GNAM(4,NGAT)=GDX                         !LOAD GATE-NAME INDEX
C
      IF(NGAT.GT.1) THEN
      CALL NAMLOC(GNAM,GNAM(1,NGAT),NGAT-1,IDX) !TSTFR NAME EXISTS
      IF(IDX.GT.0) GO TO 540                    !ERROR IF YES
                    ENDIF
C
      IF(LEGLNAM(GNAM(1,NGAT)).NE.YES) GO TO 550  !TSTFR LEGAL 
C
      CALL NEWMSK(PATNO,MSKNO)                  !GET NEW PATTERN WD#
C                                               !AND MASK
C
      PATN(NGAT)=PATNO                          !LOAD PATTERN WD#
      GMSK(NGAT)=MSKNO                          !LOAD MASK
C
C     ************************************************************
C     DECODE BOOLEAN EXPRESSION - <N>GNAM(I)  OP <N> GNAM(J) .....
C     ************************************************************
C
      JJ=3                                      !POINT TO 3RD FIELD
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF UNDEF
C
      BOOLOP=0                                  !RESET OP- WORD
      BOONOT=0                                  !RESET NOT-WORD
C
      IF(LWD(1,JJ).EQ.NOT) THEN                 !TSTFR NOT
      BOONOT=1                                  !IF YES, SET IT
      JJ=JJ+1                                   !INC FIELD PNTR
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF UNDEF
      ENDIF
C
      DO 30 I=1,3 
      RAWNAM(I)=LWD(I,JJ)                       !LOAD RAW GATE-NAME
   30 CONTINUE
      JJ=JJ+1                                   !INC FIELD PNTR
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF UNDEF
      CALL MILV3(LWD(1,JJ),IDX,XV,KIND,IERR)    !GET GATE-NAME INDX
      IF(IERR.NE.0) GO TO 500                   !TSTFR ERROR
      RAWNAM(4)=IDX                             !LOAD IT
C
      DO 40 I=1,NGAT                            !TSTFR RAWNAM EXISTS
      IF(GTYP(I).NE.RAW) GO TO 40
      IF(ISEQUL(RAWNAM,GNAM(1,I),4).EQ.YES) GO TO 50
   40 CONTINUE
      GO TO 560                                 !ERROR IF NON-EXIST
C
C                                               !OTHERWISE,
   50 CALL BOOLOD(PATNO+1)                      !LOAD PATTERN INDEX
                                                !START WITH NDX=2
      CALL BOOLOD(MSKNO)                        !LOAD MASK
      CALL BOOLOD(BOONOT)                       !LOAD NOTCODE
      CALL BOOLOD(I)                            !LOAD GATE-LIST NDX
      NENT(NGAT)=1                              !SET 1 BOOL ENTRY
C
C     ************************************************************
C     PROCESS ANY OTHER BOOLEAN FACTORS
C     ************************************************************
C
  100 JJ=JJ+1                                   !INC FIELD INDEX
      IF(JJ.GT.NF) THEN                         !TSTFR DONE
      CALL BOOLOD(-1)                           !IF YES, LOAD ENDCODE
      GO TO 1000
      ENDIF
C
      IF(LWD(1,JJ).EQ.AND) GO TO 105            !TSTFR OR- OPER
      IF(LWD(1,JJ).EQ.OR ) GO TO 110            !TSTFR AND-OPER
      GO TO 500                                 !ERROR IF NEITHER
C                                               !IF OK,
  105 BOOLOP=0                                  !AND-CODE
      GO TO 115
  110 BOOLOP=1                                  !OR -CODE
C
  115 JJ=JJ+1                                   !INC FIELD POINTER
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF OVERFLO
C
      BOONOT=0                                  !RESET NOT-CODE
      IF(LWD(1,JJ).EQ.NOT) THEN                 !TSTFR NOT-WORD
      BOONOT=1                                  !LOAD IF FOUND
      JJ=JJ+1                                   !INC FIELD INDEX
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF OVERFLO
      ENDIF
C
      DO 120 I=1,3
      RAWNAM(I)=LWD(I,JJ)                       !LOAD GATE-NAME           
  120 CONTINUE
      JJ=JJ+1                                   !INC FIELD INDEX
      IF(JJ.GT.NF) GO TO 500                    !ERROR IF OVERFLO
      CALL MILV3(LWD(1,JJ),IDX,XV,KIND,IERR)    !DECODE GATE-NAME-INDEX
      IF(IERR.NE.0) GO TO 500                   !TSTFR ERROR
      RAWNAM(4)=IDX                             !LOAD INDEX IN RAWNAM
C
      DO 140 I=1,NGAT                           !TSTFR EXIST
      IF(GTYP(I).NE.RAW) GO TO 140
      IF(ISEQUL(RAWNAM,GNAM(1,I),4).EQ.YES) GO TO 150
  140 CONTINUE
      GO TO 560                                 !ERROR IF NON-EXIST
C
C                                               !OTHERWISE,
  150 CALL BOOLOD(BOOLOP)                       !LOAD OP- CODE
      CALL BOOLOD(BOONOT)                       !LOAD NOT-CODE
      CALL BOOLOD(I)                            !LOAD GATE-LIST NDX
      NENT(NGAT)=NENT(NGAT)+1                   !INC BOOL COUNTER
      GO TO 100
C
C     ************************************************************
C     SEND ANY ERROR MESSAGES
C     ************************************************************
C
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR IN CALCULATED GATE SPECIFICATION')
      GO TO 800
C
  510 WRITE(CMSSG,515)NGAT
  515 FORMAT('GATE TABLE OVERFLOW AT - ',I6)
      GO TO 800
  520 WRITE(CMSSG,525)
  525 FORMAT('TRUNCATION ERROR FROM - GREAD')
      GO TO 800
  540 WRITE(CMSSG,545)(GNAM(I,NGAT),I=1,4)
  545 FORMAT('MULTIPLE DEFINITION OF GATE NAME = ',3A4,I4)
      GO TO 800
  550 WRITE(CMSSG,555)GNAM(1,NGAT)
  555 FORMAT('ILLEGAL GATE NAME = ',A4)
      GO TO 800
  560 WRITE(CMSSG,565)RAWNAM
  565 FORMAT('RAW GATE-NAME NOT FOUND = ',3A4,I4)
      GO TO 800
C
  800 CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)    !RESET WIDTH TO 8
      RETURN
      END
