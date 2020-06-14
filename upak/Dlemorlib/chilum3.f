C$PROG CHILUM3   - CHIL processor for L003 formatted data
C
C     ******************************************************************
C     BY J. A. Biggerstaff AT HHIRF - LAST MODIFIED by WTM 04/23/2002
C     ******************************************************************
C
C
      SUBROUTINE CHILUM3(INBUF,NWDS)         !LINKAGE CHANGE * * *
C
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
      INTEGER*2    INBUF(*)
C
C     ------------------------------------------------------------------
      COMMON/LM12/ MILF(262144)
C
      INTEGER*4    MILF
C
      INTEGER*4    MIL(0:65535)
C
      INTEGER*2    MILH(0:131071)
C
      EQUIVALENCE (MIL(0),MILF(1)),(MILH(0),MILF(1))
C     ------------------------------------------------------------------
      INTEGER*4    CLX,  CLY
C
      PARAMETER   (CLX=4,CLY=4)               !MBUF DIMENSIONS
C
      COMMON/LM02/ MBUF,JBN,IPO,NBC,LX,LY,LXB,LJP
C
      INTEGER*2    MBUF(0:CLX-1,0:CLY)        !PLUS A SPARE

      INTEGER*2    JBN(0:CLY-1),IPO(0:CLY-1),NBC(CLY)

      INTEGER*4    LX,LY,LXB,LJP
C     ------------------------------------------------------------------
      INTEGER*4    DUM
C
      PARAMETER   (DUM=4)
C
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF
C
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
C
      INTEGER*2    OUBUF(0:DUM)
C     ------------------------------------------------------------------
      COMMON/LM10/ FWLEN,NPARM,TOFSAVE
      INTEGER*4    FWLEN,NPARM,TOFSAVE
C     ------------------------------------------------------------------
      COMMON/LM28/ UBUFF(2048),TOFFSET
      INTEGER*4    UBUFF,      TOFFSET
C     ------------------------------------------------------------------
C     THESE ARE THINGS I NEED TO SAVE ACROSS CALLS
C
      INTEGER*4    HFWD
C
      PARAMETER   (HFWD=Z'FFFF')              !HALFWORD MASK
C
      INTEGER*4    TBUFF(0:1999)
C
      INTEGER*4    TBUF(2000)                 !USE ID# AS FTN INDEX ****
C
      EQUIVALENCE (TBUF(1),TBUFF(0),UBUFF(25))                    !****
C     ------------------------------------------------------------------
      INTEGER*4    ACCUM,NWDS,MILPTR,CODE,P1
C
      INTEGER*4    IOFF,SIZE,DATA,COUNT,ORD,DMIN,DMAX,VBN,RBN,IPLACE
C
      INTEGER*4    MASK,ID1,IDL,OOF,OEND,PARM,RETPUSH,RETCHK
C
      INTEGER*4    I,J
C
      INTEGER*4    ISHFT
C
C     EXTERNAL     ISHFT
C
      INTEGER*4    NSH
C
      CHARACTER*4  STAT
C
      SAVE
C
C     ==================================================================
C
C
C     STATEMENT NUMBERS 17-34 CORRESPOND TO THE "MIL" OPCODE
C               NUMBERS 123-170 (PACK) ARE LINE #'S IN JAPROC05
C               NUMBERS 175-470 ARE LINE #'S IN JPROF03
C               NUMBERS > 500 ARE JUST NUMBERS
C     ------------------------------------------------------------------
C
C
C     ==================================================================
C     This is where we unpack the events
C     ==================================================================
C
      ACCUM=0                              !KEEP F7O HAPPY
C
      STAT='INIT'
C
  179 DO 180 I=FWLEN,0,-1
      TBUFF(I)=-1
  180 CONTINUE
C
      CALL CHILUN(INBUF,NWDS,TBUFF,NPARM,STAT)
C
      IF(STAT.EQ.'GOOD') GO TO 217
      IF(STAT.EQ.'NULL') GO TO 179
      RETURN
C
C     ==================================================================
C     Setup entry - deleted here - done in routine PVSETUP
C     ==================================================================
C
C     ==================================================================
C     SCRUB entry - deleted here - done in PVSETUP - in entry PRSCRUB
C     ==================================================================
C
C     ==================================================================
C     Now for the fun ! ! ! !
C     This is where we interpret the "mighty instruction list"
C     ==================================================================
C
  217 CONTINUE                             !"EVENT"
C
      MILPTR=0                             !A HALFWORD POINTER
  221 CONTINUE                             !"PRONE"
      CODE=MILH(MILPTR)
      P1=MILH(MILPTR+1)
C
      GO TO (17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)
     1 ,CODE-16                            !"DISPATCH"
C
      STOP 'BAD MIL CODE'                  !IF FALL THROUGH
C
   17 CONTINUE                             !STORE ACCUMULATOR
      TBUF(P1)=ACCUM                                              !****
      GO TO 989                            !GO BUMP POINTER
C
   18 CONTINUE                             !LOAD ACCUMULATOR
      ACCUM=TBUF(P1)                                              !****
      GO TO 989
C
   19 CONTINUE                             !SHIFT ACCUMULATOR
      ACCUM=ISHFT(ACCUM,P1)
      GO TO 989
C
   20 CONTINUE                             !ADD PARM TO ACCUMULATOR
      IF(ACCUM.GE.0)  THEN                 !OK TO PROCEED
        IF(TBUF(P1).LT.0)  THEN            !NULL PARAM             ****
          ACCUM=-1                         !FORCE NEGATIVE
        ELSE
          ACCUM=ACCUM+TBUF(P1)             !DO THE ACCUMULATE      ****
        ENDIF
      ENDIF
      GO TO 989
C
   21 CONTINUE                             !MPY (BY IMMEDIATE VAL)
      ACCUM=ACCUM*P1
      GO TO 989
C
   22 CONTINUE                             !OR-GATE PROCEDURE
      IF(ACCUM.LT.0)   GO TO 881           !ABORT, PREVIOUS FAILURE
      IOFF=MIL(MILPTR/2+3)+MILPTR          !POINTER TO GATE LIST   ****
      SIZE=2                               !STEP SIZE
C
  878 CONTINUE                             !COMMON OR - BANANA PROC
      DATA=TBUF(P1)                                               !****
      IF(DATA.LT.0)  GO TO 881             !ABORT, NULL PARAM
      COUNT=MILH(MILPTR+5)                 !COUNT WAS NEG FOR WCS  ****
      DO 880 I=1,COUNT
      IF(DATA.LT.MILH(IOFF+1)) GO TO 881   !BELOW LOLIM, ABORT
      IF(DATA.LE.MILH(IOFF))  GO TO 882    !FOUND A HIT
      IOFF=IOFF+SIZE                       !NEXT GATE-PAIR
      ACCUM=ACCUM+MILH(MILPTR+4)           !ADD THE INCREMENT
  880 CONTINUE                             !ABORT IF FALL-THROUGH
  881 CONTINUE                             !MISSED GATE
      ACCUM=-1                             !FLAG IT
      P1=MILH(MILPTR+2)                    !'MISS' DISPLACEMENT
      GO TO 31
  882 CONTINUE                             !GATE HIT
      P1=MILH(MILPTR+3)                    !'HIT' DISPLACEMENT
      GO TO 31
C
   23 CONTINUE                             !"BANANA GATES"
      IF(ACCUM.LT.0)  GO TO 881            !ABORT
      ORD=TBUF(MILH(MILPTR+7))                                    !****
CX    ORD=ISHFT(ORD,-MILH(MILPTR+6))
      NSH=-MILH(MILPTR+6)                  !Make it I*4
      ORD=ISHFT(ORD,NSH)
      SIZE=MILH(MILPTR+9)
      IF(ORD.GE.SIZE)  GO TO 881           !ABORT
      SIZE=2*SIZE
      IOFF=MILPTR+MIL(MILPTR/2+5)+2*ORD                           !****
      GO TO 878                            !USE COMMON PROCEDURE
C
   24 CONTINUE                             !PREP 1ST DIMENSION
      ACCUM=0                              !WCS WAS MORE SOPHISTICATED
C
   25 CONTINUE                             !PREP CONTINUE
      IF(ACCUM.LT.0) GO TO 890             !ABORT
      DATA=TBUF(MILH(MILPTR+3))                                   !****
      IF(DATA.LT.0)  GO TO 890             !ABORT
CX    DATA=ISHFT(DATA,-MILH(MILPTR+2))
      NSH=-MILH(MILPTR+2)                  !Make it I*4
      DATA=ISHFT(DATA,NSH)
      DMIN=MILH(MILPTR+4)
      IF(DMIN.GT.0) THEN                   !SUBTRACT BASELINE
       DATA=DATA-DMIN
       IF(DATA.LT.0)  GO TO 890            !AND RECHECK
      ENDIF
      DMAX=MILH(MILPTR+5)
      DMAX=IAND(DMAX,HFWD)
      IF(DATA.GE.DMAX)  GO TO 890          !ABORT
      ACCUM=ACCUM*DMAX+DATA
      MILPTR=MILPTR+6                      !FIXED 'HIT' DISPLACEMENT
      GO TO 221
  890 CONTINUE                             !ABORTING
      ACCUM=-1
      GO TO 31                             !P1 HAS 'MISS'
C
   27 CONTINUE                             !ADD-1 TO 32 BIT CHAN
      ACCUM=2*ACCUM                        !EQUIVALENT 16-BIT CHAN
C
   26 CONTINUE                             !ADD-1 TO 16 BIT CHAN
      ACCUM=ACCUM+MIL(MILPTR/2+1)          !ADD BASE ADDRESS
      VBN=ISHFT(ACCUM,-15)                 !VIRTUAL BLOCK NUMBER
      RBN=JBN(VBN)                         !REAL BLOCK NUMBER
      IF(RBN.GE.0)  GO TO 920              !DISK HISTOGRAM
CX    IF(RBN.EQ.-1) THEN                   !HALFWORD HIS
CX     CALL MEM_ADD1_HW(ACCUM+1)
CX    ELSE                                 !FULLWORD HIS
CX     CALL MEM_ADD1_FW(ACCUM/2+1)
CX    ENDIF
      GO TO 921
C
  920 CONTINUE                             !DISK HISTOGRAM
      IPLACE=IPO(VBN)
      IPO(VBN)=IPLACE+1
      MBUF(IPLACE,RBN)=IAND(ACCUM,Z'7FFF')
      IF(IPLACE.GE.CLX-1) CALL PHASE1(VBN) !NOTE LINKAGE REVERSION
  921 MILPTR=MILPTR+4
      GO TO 221
C
   28 CONTINUE                             !LOAD IMMEDIATE
      ACCUM=P1
      GO TO 989
C
   29 CONTINUE                             !TLU / MAPPED GATE
      ORD=TBUF(P1)                                                !****
      IF(ORD.LT.0) GO TO 930               !ABORT
CX    ORD=ISHFT(ORD,-MILH(MILPTR+4))
      NSH=-MILH(MILPTR+4)                  !Make it I*4
      ORD=ISHFT(ORD,NSH)
      IF(ORD.GE.MILH(MILPTR+5)) GO TO 930  !ABORT
      ACCUM=MILH(MILPTR+MIL(MILPTR/2+3)+ORD)                      !****
      IF(ACCUM.LT.0) GO TO 930             !ABORT
      P1=MILH(MILPTR+3)                    !SELECT 'HIT' EXIT
      GO TO 31
  930 CONTINUE                             !ABORTING
      ACCUM=-1
      P1=MILH(MILPTR+2)                    !SELECT 'MISS' EXIT
      GO TO 31
C
   30 CONTINUE                             !IN-LINE GATE
      DATA=TBUF(P1)                                               !****
      IF(DATA.LT.MILH(MILPTR+5)) GO TO 940 !LESS THAN LOLIM
      IF(DATA.GT.MILH(MILPTR+4)) GO TO 940 !ABOVE HILIM
      P1=MILH(MILPTR+3)                    !SELECT 'HIT'
      GO TO 31
  940 P1=MILH(MILPTR+2)                    !SELECT 'MISS'
      GO TO 31
C
   32 CONTINUE                             !VARIOUS EXITS
      MILPTR=MILPTR+2                      !PROBABLE INCREMENT
      GO TO (239,1001,1002,1003,1011,1011,1011),P1+1
      STOP 'BAD MIL EXIT CODE'
C
  239 CONTINUE                             !EVENT DONE
      GO TO 179                            !GO SEARCH
C
   33 CONTINUE                             !BIT TESTING
      DATA=TBUF(P1)                                               !****
      IF(DATA.LT.0) GO TO 950              !=> 'NONE'
      MASK=MILH(MILPTR+5)
      MASK=IAND(MASK,HFWD)
      MASK=IAND(MASK,DATA)
      IF(MASK.EQ.0) GO TO 950              ! NONE
      IF(MASK.EQ.DATA) GO TO 960           ! ALL MATCH
      P1=MILH(MILPTR+2)                    !SELECT 'SOME' EXIT
      GO TO 31
  950 P1=MILH(MILPTR+3)                    !SELECT 'NONE' EXIT
      GO TO 31
  960 P1=MILH(MILPTR+4)                    !SELECT 'ALL' EXIT
      GO TO 31
C
  989 P1=2                                 !FOR "SIMPLE" PROCEDURES****
C
   31 CONTINUE                             !GO TO - FIX MILPTR
      MILPTR=MILPTR+IAND(P1,HFWD)          !UNSIGNED DISPLACEMENT  ****
      GO TO 221
C
   34 CONTINUE                             !COMPUTED GO TO
      P1=MILH(MILPTR+ACCUM+1)
      GO TO 31
C
C     ==================================================================
C     THUS WE COMPLETE PROCEDURES FOR THE MIGHTY INSTRUCTION LIST
C     ==================================================================
C
 1001 CALL USERSUB1(TBUF)                  !CALL USERSUB'S
      GO TO 221
 1002 CALL USERSUB2(TBUF)
      GO TO 221
 1003 CALL USERSUB3(TBUF)
      GO TO 221
C
C     ==================================================================
C     "REPACK" RESURRECTED FROM PRE-WCS DAYS
C     ==================================================================
C
 1011 CONTINUE                             !"PACK"
      J=P1-3                               !STREAM NUMBER
      ID1=MILH(MILPTR)                     !FIRST PARM NUMBER
      IDL=MILH(MILPTR+1)                   !LAST PARM NUMBER
      MILPTR=MILPTR+2
C
      CALL CHILOUT(ID1,IDL)                !Repack to output stream-1
      GO TO 221
C
      END
