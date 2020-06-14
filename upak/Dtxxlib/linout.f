C$PROG LINOUT
C
      SUBROUTINE  LINOUT(LCH,IPO,ILB,ILL,IHI,ISPA,NLPG,NPAG)
C
      COMMON/BBB/  IFMTA,IFMTB,IDATE(3),IFODX,JUSTON
      CHARACTER*16 IFMTA,IFMTB
      CHARACTER*4                             JUSTON
C
      INTEGER*4 LCH(240,2),NCH(120,2),NWD(30),NWD21(21)
C
      EQUIVALENCE (NWD21,NWD)
C
      INTEGER*4   X20
C
      DATA        X20/'20'X/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
C     *************************************************************
C     OUTPUTS CHARACTERS CONTAINED IN ICH IN JUSTIFIED LINES
C     *************************************************************
C   
   10 JUSTF=0                               !CLR JUSTIFY FLAG
      LAST=LASTC(LCH(1,1),IPO)              !LAST NON-BLANK IN LCH
      NUP=LAST                              !LAST NON-BLANK IN LCH
      IF(LAST.LE.IHI) GO TO 40              !CAN WE PRINT ALL
C   
      JUSTF=1                               !SET JUSTIFY FLAG
      NN=IHI+1                              !INIT BACKWARD PNTR
   20 NN=NN-1                               !DEC  BACKWARD PNTR
      IF(LCH(NN,1).NE.X20) GO TO 20         !TST FOR BLANK
C   
   30 NN=NN-1                               !INIT FOR NON-BL SEARCH
      IF(LCH(NN,1).EQ.X20) GO TO 30         !TST FOR NON-BLANK
C   
      NUP=NN                                !LAST BYTE TO PRINT
   40 IF(NUP.LE.0) GO TO 50                 !TST FOR EMPTY
C   
      DO 45 I=1,NUP                         !LOAD PART TO BE PRINTED
      NCH(I,1)=LCH(I,1)                     !INTO NCH
      NCH(I,2)=LCH(I,2)
   45 CONTINUE
C   
   50 IA=NUP+1                              !1ST BYTE TO BLANK OUT
      IF(IA.GT.120) GO TO 70                !TST FOR FULL
C   
      DO 60 I=IA,120                        !BLANK REST OF NCH
      NCH(I,1)=X20
      NCH(I,2)=0
   60 CONTINUE
C   
   70 LAST=NUP                              !LAST BYTE PRINTED
C   
      IF(JUSTON.NE.'ON  ') GO TO 100
      IF(JUSTF.NE.0)CALL ADJUST(NCH,ILL,IHI)!TST FOR ADJUST NEEDED
C
  100 DO 105 I=1,30
      NWD(I)='20202020'X
  105 CONTINUE
C
      CALL PAC120(NCH(1,1),NWD,1,120)       !PACK INTO NWD
C
      CALL TRIMOUT(NWD,120)
C
      IF(ISPA.NE.1)  WRITE(7,120)           !TST FOR DBL SPACE
  120 FORMAT(1H )
C   
      CALL PAGER(ISPA,NLPG,NPAG,IHI)        !GIVE PAGER A SHOT
C   
C     *************************************************************
C     SHIFT THE UN-PRINTED PART OF THE BUFFER SO IT STARTS AT ILB
C     *************************************************************
C   
      NN=LAST+1                             !1ST UN-PRINTED BYTE
      IF(NN.GT.IPO) GO TO 140               !TST FOR .GT. IPO
C   
      DO 130 I=NN,IPO                       !FIND NEXT NON-BLANK
      IF(LCH(I,1).NE.X20) GO TO 160
  130 CONTINUE
C   
  140 DO 150 I=1,240                        !ZOT ENTIRE BUFFER
      LCH(I,1)=X20
      LCH(I,2)=0
  150 CONTINUE
      IPO=ILB-1                             !RESET IOP
      RETURN                                !AND RETURN
C   
  160 NN=I                                  !1ST NON-BLANK
      NUP=IPO                               !LAST BYTE IN BUFFER
      IPO=ILB-1                             !INIT LOAD PNTR
      IF(NN.GT.NUP) GO TO 180               !1ST NON-B OUT OF RANGE?
C   
      DO 170 I=NN,NUP                       !SHIFT DATA IN BUFFER
      IPO=IPO+1                             !INC PNTR
      LCH(IPO,1)=LCH(I,1)                   !LOAD THE BYTE
      LCH(IPO,2)=LCH(I,2)                   !AND THE FLAG
      LCH(I,1)=X20                          !BLANK BYTE JUST SHIFTED
      LCH(I,2)=0                            !AND THE FLAG
  170 CONTINUE
C   
  180 NUP=ILB-1                             !PNTR TO ZOT 1ST PART
      IF(NUP.LT.1) GO TO 200                !TST FOR NONE TO ZOT
C   
      DO 190 I=1,NUP                        !ZOT THE "INDENT PART"
      LCH(I,1)=X20
      LCH(I,2)=0
  190 CONTINUE
C   
  200 ILL=ILB                               !SET "INDENT" TO 2ND VAL
      IF(IPO.GT.IHI) GO TO 10               !TST FOR ENOUGH TO PRINT
C   
      RETURN                                !OTHERWISE, RETURN
      END
