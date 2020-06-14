C$PROG LISMIL
      SUBROUTINE LISMIL
C
      LOGICAL Q
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      INTEGER*2 MIL(65536),MILM(65536)
C
      INTEGER*4 MSG(13,50)
      CHARACTER*52 MSC(50)
C
      EQUIVALENCE (MIL(1),MILF(1)),(MILM(1),MILMF(1))
C
      EQUIVALENCE (MSC(1),MSG(1,1))
      DATA (MSC(J),J=1,10)/
     1'P1     - IMMEDIATE VALUE                            ',  !01
     2'PDISP  - TEST PARAMETER DISPLACEMENT                ',  !02
     3'CDISP  - (FAIL) (MISS - LAST OPCODE LOCATION)       ',  !03
     4'CDISP  - (HIT )                                     ',  !04
     5'INCR   - VALUE ADDED TO ACC FOR EACH PARTIAL MISS   ',  !05
     6'NEGNG  - MINUS THE NUMBER OF GATES                  ',  !06
     7'GDISP  - DISPLACEMENT OF GATE/BAN LIST (FULL WORD)  ',  !07
     8'PDISP1 - 1ST BAN TEST-PARAMETER DISPLACEMENT        ',  !08
     9'PDISP2 - 2ND BAN TEST-PARAMETER DISPLACEMENT        ',  !09
     A'NEGNB  - MINUS THE NUMBER OF BANANAS                '/  !10
      DATA (MSC(J),J=11,20)/
     1'SHIFT  - RIGHT SHIFT (+) TO APPLY TO "ABSCISSA"     ',  !11
     2'SIZE   - LENGTH (FULL WDS) OF EACH FFG VECTOR       ',  !12
     3'PAD                                                 ',  !13
     4'MIN    - BASELINE SUBTRACTED FROM PARM AFTER SHIFT  ',  !14
     5'MAXNC  - # CHANNELS ALLOWED (AFTER SHIFT & SUBTRACT)',  !15
     6'HISBASE- HISTOGRAM BASE ADDRESS (FULL WORD)         ',  !16
     7'OPCODE - STORE ACCUMULATOR IN (VIRTUAL) PARAMETER   ',  !17
     8'OPCODE - LOAD ACCUMULATOR WITH PARAMETER VALUE      ',  !18
     9'OPCODE - SHIFT ACCUMULATOR BY SIGNED (IMMEDIATE) VAL',  !19
     A'OPCODE - ADD PARAMETER VALUE TO THE ACCUMULATOR     '/  !20
      DATA (MSC(J),J=21,30)/
     1'OPCODE - MULTIPLY ACCUMULATOR BY (IMMEDIATE) VALUE  ',  !21
     2'OPCODE - DO BASIC "OR" GATE PROCEDURE               ',  !22
     3'OPCODE - DO "BANANA" GATE PROCEDURE                 ',  !23
     4'OPCODE - PREPARE FIRST HISTOGRAM PARAMETER          ',  !24
     5'OPCODE - FOLD IN SUBSEQUENT HISTOGRAM PARAMETER     ',  !25
     6'OPCODE - INCREMENT 16-BIT HISTOGRAM                 ',  !26
     7'OPCODE - INCREMENT 32-BIT HISTOGRAM                 ',  !27
     8'OPCODE - LOAD ACCUMULATOR (IMMEDIATE)               ',  !28
     9'OPCODE - TABLE LOOKUP/MAPPED GATE                   ',  !29
     A'OPCODE - SIMPLE INLINE GATE                         '/  !30
      DATA (MSC(J),J=31,40)/
     1'OPCODE - "GO TO"                                    ',  !31
     2'OPCODE - EXIT WCS - [CALL USERSUB] OR [REPACK]      ',  !32
     3'OPCODE - 3-WAY BIT TESTING                          ',  !33
     4'OPCODE - COMPUTED GO TO                             ',  !34
     5'                                                    ',  !35
     6'IHI    - SINGLE IN-LINE GATE HI-LIMIT               ',  !36
     7'ILO    - SINGLE IN-LINE GATE LO-LIMIT               ',  !37
     8'PDISP  - HIST PARAMETER DISPLACEMENT                ',  !38
     9'CDISP  - IF SOME BITS MATCH MASK                    ',  !39
     A'CDISP  - IF NO   BITS MATCH MASK                    '/  !40
      DATA (MSC(J),J=41,50)/
     1'CDISP  - IF ALL  BITS MATCH MASK                    ',  !41
     2'MASK   - FOR BIT-TEST (HEX)                         ',  !42
     3'END    - END OF MIL                                 ',  !43
     4'HAND INSERTED HALF-WORD CODE ! ! ! ! ! ! ! ! ! ! ! !',  !44
     5'HAND INSERTED FULL-WORD CODE ! ! ! ! ! ! ! ! ! ! ! !',  !45
     6'USERSUB #                                           ',  !46
     7'REPACK STREAM #                                     ',  !47
     8'MINIMUM RARAMETER # FOR REPACK                      ',  !48
     9'MAXIMUM PARAMETER # FOR REPACK                      ',  !49
     A'WHAT THE HELL IS THIS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'/  !50
C
      CHARACTER*4  IFLG
C
      SAVE
C
C
      WRITE(LU6,5)
    5 FORMAT(1H1,'HERE COME DE CHIL - HERE COME DE CHIL'/)
C
      NC=0                              !ZERO THE MIL-CNTR
C
   10 NC=NC+1                           !INC THE MIL-CNTR
      IF(NC.GT.32768) RETURN
      IBY=2*(NC-1)                      !CALC LOWEST BYTE#
      IFLG='    '                       !RESET OPCODE FLAG
      JT=MILM(NC)                       !GET THE LABEL INDEX
      IF(JT.EQ.0) GO TO 10              !SKIP IF NOT SET
      IF(JT.LT.0) GO TO 60              !TST FOR NEG (FULL WORD)
C
      IF(JT.GE.17.AND.JT.LE.34) IFLG='****'  !FLAG OPCODES
      ICO=MIL(NC)                       !PICK UP 16-BIT CODE
C
   20 IF(JT.GT.50) JT=50                !PROTECT AGAINST WILD STUFF
      IF(JT.EQ.28) WRITE(LU6,25)
      IF(JT.GE.31.AND.JT.LE.34) WRITE(LU6,25)
   25 FORMAT(1H )
      IF(JT.EQ.42) GO TO 40
      WRITE(LU6,30)IFLG,IBY,NC,ICO,(MSG(I,JT),I=1,13) !LIST CODE & LABEL
   30 FORMAT(1H ,A4,2X,2I6,I10,4X,13A4)
      GO TO 10
C
   40 WRITE(LU6,50)IFLG,IBY,NC,ICO,(MSG(I,JT),I=1,13) !LIST CODE & LABEL
   50 FORMAT(1H ,A4,2X,2I6,Z10,' H  ',13A4)
      GO TO 10
C
   60 NDX=(NC+1)/2                      !GET FULL-WORD INDEX
      ICO=MILF(NDX)                     !PICK UP 32-BIT WORD
      JT=IABS(JT)                       !MAKE LABEL INDEX POS
      IF(JT.GT.50) JT=50                !PROTECT AGAINST WILD STUFF
      IF(JT.GE.17.AND.JT.LE.34) IFLG='****'  !FLAG OPCODE
      NC=NC+1                           !INC MIL-CNTR 1 EXTRA
      GO TO 20                          !GO LIST IT
C
      END
