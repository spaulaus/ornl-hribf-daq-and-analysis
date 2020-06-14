C$PROG BILNAM    - Builds full device name from abreviation
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
C
      SUBROUTINE BILNAM(IWD,DSK,EXT,NAMFD,IEXT,LDOT,INEW,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4 IWD(20),NAMFD(20),DSK(2),EXT
C
      INTEGER*4 BLANK
C
      DATA      BLANK/Z'20202020'/
C
      DATA ICOM,IDOT,ICOL,ISEM/Z'2C',Z'2E',Z'3A',Z'3B'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ON ENTRY  - IWD   CONTAINS - CMD <DEV:>FILNAM<.EXT><!VER><,NEW>
C                 DSK   CONTAINS - DEFAULT DEV:
C                 EXT   CONTAINS - DEFAULT .EXT
C
C     ON RETURN - NAMFD CONTAINS - <DEV:>FILNAM<.EXT>
C                 IEXT  CONTAINS - <.EXT> ACTUALLY USED
C                 INEW  CONTAINS - <NEW>
C     ------------------------------------------------------------------
C
      DO 10 I=1,20                      !LOOP TO BLANK FIL/DEV NAME
      NAMFD(I)=BLANK
   10 CONTINUE
      IEXT=BLANK                        !BLANK FIL/DEV EXT WORD
      INEW=BLANK                        !BLANK NEW-FLAG
      LDOT=0                            !LOC OF DOT (1ST EXT BYTE)
      IERR=0                            !ERROR FLAG
C
      LLO=NXBL(IWD,1,80)                !LOC OF 1ST BLANK AFTER CMD
      IF(LLO.LE.0) GO TO 200            !ERROR IF NOT FOUND
      LLO=NXNB(IWD,LLO,80)              !LOC OF 1ST FIL/DEV BYTE
      IF(LLO.LE.0) GO TO 200            !ERROR IF NOT FOUND
      LHI=LSNB(IWD,LLO,80)              !LOC OF LAST NON-BLANK
      IF(LHI.LE.0) GO TO 200            !ERROR IF NOT FOUND
C
      LDOT=LEXT(IWD,LLO,LHI)            !LOC OF DOT (1ST EXT BYTE)
      LCOL=IFIND(IWD,ICOL,LLO,LHI)      !LOC OF : (LAST DEV-BYTE)
      LSEM=IFIND(IWD,ISEM,LLO,LHI)      !LOC OF ! (1ST  VER-BYTE)
      LCOM=IFIND(IWD,ICOM,LLO,LHI)      !LOC OF , (1ST  NEW-BYTE)
C
      NCL=0                             !INIT # BYTES LOADED IN NAMFD
C
      IF(LCOL.LE.0) GO TO 20            !TST FOR DEV-FIELD
C
      CALL LODUP(IWD,LLO,LCOL,NAMFD,1)  !LOAD DEVICE NAME
      NCL=LCOL-LLO+1                    !# BUTES LOADED
      IA=LCOL+1                         !NEXT BYTE IN IWD TO PROCESS
      GO TO 30
C
   20 IA=LLO                            !NEXT BYTE IN IWD TO PROCESS
      IF(DSK(1).EQ.BLANK) GO TO 30      !TST FOR DEFAULT DEV GIVEN
      NCL=LSNB(DSK,1,8)                 !GET # BYTES IN DEVICE NAME
      CALL LODUP(DSK,1,NCL,NAMFD,1)     !LOAD DEFAULT DEVICE NAME
C
   30 IB=LHI                            !LAST BYTE IN IWD FOR FILNAM
      IF(LCOM.GT.0) IB=LCOM-1           !OMIT NEW-FIELD
      CALL LODUP(IWD,IA,IB,NAMFD,NCL+1) !LOAD FILNAM
      NCL=NCL+IB-IA+1                   !# BYTES LOADED
      IF(LDOT.GT.0) GO TO 40            !TST FOR EXTENSION SPECIFIED
      NEX=LSNB(EXT,1,4)                 !#BYTES IN DEFAULT EXTENSION
      IF(NEX.LE.0)  GO TO 40            !TST FOR DEFAULT EXT GIVEN
      CALL LODUP(EXT,1,NEX,NAMFD,NCL+1) !LOAD DEFAULT EXT
C
   40 IF(LDOT.EQ.0) THEN                !SET EXT-NAME IN IEXT
                    IEXT=EXT
                    GO TO 50
                    ENDIF
C
      CALL LODUP(IWD,LDOT,LDOT+3,IEXT,1)
C
   50 LDOT=LEXT(NAMFD,1,80)             !LOC OF 1ST EXT-BYTE IN NAMFD
C
      IF(LCOM.LE.0) RETURN              !TST FOR COMMA (NEW-FIELD)
      LC=LCOM
      CALL LODUP(IWD,LC+1,LC+3,INEW,1)  !IF YES, LOAD "INEW"
      RETURN
C
  200 WRITE(CMSSG,205)                  !ERROR RETURN
  205 FORMAT('SYNTAX ERROR OR ILLEGAL FIL/DEV SPECIFICATION')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
