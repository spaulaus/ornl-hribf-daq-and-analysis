C$PROG CHILCON   - Converts WCS style chil for fortran compatibility
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CHILCON(NW)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM12/ MILF(262144)                                                
      INTEGER*4    MILF
C     ------------------------------------------------------------------
      INTEGER*2    MILH(524288)
C
      EQUIVALENCE (MILH(1),MILF(1))
C
      INTEGER*4    NW,N,NF,NLST,IGO,NDX,IT,I,J
C
      SAVE
C
C     ==================================================================
C     ROUTINE TO CONVERT WCS STYLE CHIL INSTRUCTION LIST TO
C     INSTRUCTION LIST COMPATIBLE WITH FORTRAN CHIL PROCESSOR.
C     ==================================================================
C
      N=1
C
      NLST=0
  100 CONTINUE
C
  110 IF(N.GT.NW) RETURN
      IGO=MILH(N)-16
      IF(IGO.LT.0)  RETURN
      IF(IGO.GT.18) RETURN
C
      GO TO (170,180,190,200,210,220,230,240,250,260,270,280,290,
     &       300,310,320,330,340) IGO
      RETURN
C
C                               !(17) STORE ACC IN PARM ****************
  170 MILH(N+1)=MILH(N+1)/2+1   !PDISP - BYTE-INDEX TO HWD-INDEX
      N=N+2                     !ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(18) LOAD ACC WITH PARM ***************
  180 MILH(N+1)=MILH(N+1)/2+1   !PDISP - BYTE-INDEX TO HWD-INDEX
      N=N+2                     !ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(19) SHIFT ACC BY P1 ******************
  190 N=N+2                     !NO CHANGE - ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(20) ADD PARM TO ACC ******************
  200 MILH(N+1)=MILH(N+1)/2+1   !PDISP - BYTE-INDEX TO HWD-INDEX
      N=N+2                     !ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(21) MULTIPLY ACC BY P1 ***************
  210 N=N+2                     !NO CHANGE - ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(22) 1-D GATE LIST PROCESSING *********
  220 MILH(N+1)=MILH(N+1)/2+1   !PDISP - BYTE-INDEX TO HWD-INDEX
      MILH(N+2)=MILH(N+2)/2     !CDISP(FAIL) - BYTE-DISP TO HWD-DISP
      MILH(N+3)=MILH(N+3)/2     !CDISP(HIT)  - BYTE-DISP TO HWD-DIDP
      MILH(N+5)=-MILH(N+5)      !-#GATES TO +#GATES
      NF=(N+6)/2+1              !FULL-WD INDEX OF TABLE DISP (GDISP)
      MILF(NF)=MILF(NF)/2       !GDISP - BYTE-DISP TO HWD-DISP
      NLST=MILH(N+5)            !# OF GATES FOR ANY COMPUTED GOTO
      N=N+8                     !ADVANCE CHIL-INDEX
      GO TO 110
C
C                               !(23) 2-D GATE PROCESSING **************
  230 MILH(N+1)=MILH(N+1)/2+1   !PDISP(1) - BYTE-INDEX TO HWD-INDEX
      MILH(N+2)=MILH(N+2)/2     !CDISP(FAIL) - BYTE-DISP TO HWD-DISP
      MILH(N+3)=MILH(N+3)/2     !CDISP(HIT)  - BYTE-DISP TO HWD-DISP
      MILH(N+5)=-MILH(N+5)      !-#GATES TO +#GATES
      MILH(N+7)=MILH(N+7)/2+1   !PDISP(2) - BYTE-INDEX TO HWD-INDEX
      NF=(N+10)/2+1             !FULL-WD INDEX OF GATE TABLE (GDISP)
      MILF(NF)=MILF(NF)/2       !GDISP - BYTE-DISP TO HWD-DISP
      NLST=MILH(N+5)            !# OF GATES FOR ANY COMPUTED GOTO
      N=N+12                    !ADVANCE CHIL-INDEX
      GO TO 110
C
C                               !(24) FIRST CHAN# CALCULATION **********
  240 MILH(N+1)=MILH(N+1)/2     !CDISP(FAIL) - BYTE-DISP TO HWD-DISP
      MILH(N+3)=MILH(N+3)/2+1   !PDISP - BYTE-INDEX TO HWD-INDEX
      N=N+6                     !ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(25) SUBSEQUENT CHAN# CALCULATION *****
  250 MILH(N+1)=MILH(N+1)/2     !CDISP(FAIL) - BYTE-DISP TO HWD-DISP
      MILH(N+3)=MILH(N+3)/2+1   !PDISP - BYTE-INDEX TO HWD-INDEX
      N=N+6                     !ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(26) 16-BIT/CHAN INCREMENTING *********
  260 N=N+4                     !NO CHANGE - ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(27) 32-BIT/CHAN INCREMENTING *********
  270 N=N+4                     !NO CHANGE - ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(28) LOAD ACC WITH P1 *****************
  280 N=N+2                     !NO CHANGE - ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(29) TABLE LOOKUP / MAPPED GATES ******
  290 MILH(N+1)=MILH(N+1)/2+1   !PDISP - BYTE-INDEX TO HWD-INDEX
      MILH(N+2)=MILH(N+2)/2     !CDISP(FAIL) - BYTE-DISP TO HWD-DISP
      MILH(N+3)=MILH(N+3)/2     !CDISP(HIT)  - BYTE-DISP TO HWD-DISP
      NF=(N+6)/2+1              !FULL-WD INDEX OF TDISP
      MILF(NF)=MILF(NF)/2       !TDISP - BYTE-DISP TO HWD-DISP
      N=N+8                     !ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(30) SIMPLE IN-LINE GATE **************
  300 MILH(N+1)=MILH(N+1)/2+1   !PDISP - BYTE-INDEX TO HWD-INDEX
      MILH(N+2)=MILH(N+2)/2     !CDISP(FAIL) - BYTE-DISP TO HWD-DISP
      MILH(N+3)=MILH(N+3)/2     !CDISP(HIT)  - BYTE-DISP TO HWD-DISP
      NLST=MILH(N+4)+1          !HI-LIMIT+1 FOR POSSIBLE IFP
      N=N+6                     !ADVANCE CHIL-INDEX
      GO TO 110
C
C                               !(31) GO TO (ADD P1 TO CHIL PNTR) ******
  310 MILH(N+1)=MILH(N+1)/2     !CDISP - BYTE-DISP TO HWD-DISP
      N=N+2                     !ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(32) EXIT CHIL PROCESSOR **************
  320 IT=MILH(N+1)              !SUB-CODE
      N=N+2                     !ADVANCE CHIL-INDEX
      IF(IT.GT.3) N=N+2         !TEST FOR CALL REPACK SUB-CODE
      GO TO 100
C
C                               !(33) BIT-TEST AND 3-WAY BRANCH ********
  330 MILH(N+1)=MILH(N+1)/2+1   !PDISP - BYTE-INDEX TO HWD INDEX
      MILH(N+2)=MILH(N+2)/2     !CDISP(SOME) - BYTE-DISP TO HWD-DISP
      MILH(N+3)=MILH(N+3)/2     !CDISP(NONE) - BYTE-DISP TO HWD-DISP
      MILH(N+4)=MILH(N+4)/2     !CDISP(ALL)  - BYTE-DISP TO HWD-DISP
      N=N+6                     !ADVANCE CHIL-INDEX
      GO TO 100
C
C                               !(34) COMPUTED GOTO ********************
  340 DO 345 I=1,NLST           !LOOP ON # DISPLACEMENT ENTRIES
      NDX=N+I                   !CHIL INDEX
      MILH(NDX)=MILH(NDX)/2     !CDISP - BYTE-DISP TO HWD-DISP
  345 CONTINUE
      J=NLST
      IF(2*(J/2).EQ.J) J=J+1    !TEST FOR PAD
      N=N+J+1                   !ADVANCE CHIL-INDEX
      GO TO 100
C
      END
