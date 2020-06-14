C$PROG SLICO     - Outputs 2-D slices for COPAD, SETUM, SHIFT, SLOUT
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SLICO(LUD,LUH,ID,IY,IDAT,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
C     ------------------------------------------------------------------
      COMMON/TDX9/ NEGSET
      INTEGER*4    NEGSET
      CHARACTER*4  CNEGSET
      EQUIVALENCE (CNEGSET,NEGSET)
C     ------------------------------------------------------------------
      INTEGER*4    JDATF(16384),NDX(4),IDAT(1)
C
      INTEGER*4    JHEDF(32),MSER(10)
C   
      INTEGER*2    JDATH(32768)
C
      INTEGER*2    JHEDH(64),MINC(4),MAXC(4),ND,NHW
C   
      EQUIVALENCE (JDATF(1),IDATF(32769))
C
      EQUIVALENCE (JDATH(1),JDATF(1)),
     &            (JHEDH(1),JHEDF(1)),
     &            (ND      ,JHEDH(1)),
     &            (NHW     ,JHEDH(2)),
     &            (MINC(1) ,JHEDH(15)),
     &            (MAXC(1) ,JHEDH(19))
C   
      DATA NDX/1,1,0,0/
C
      DATA MXSL,ISLO,ISHI,NCH,MINX,MAXX,MINY,MAXY/8*0/
C
      DATA NSLI,NHW/0,1/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ACCUMULATES SLICES IN CORE UNTIL:
C   
C     (A) THE MAX # OF SLICES ARE ACCUMULATED OR,
C     (B) A NON-CONTIGUOUS SLICE IS "PUSHED OUT"
C     (C) A NEGATIVE VALUE OF "IY" IS PASSED
C   
C     AT SUCH TIME, ALL IN-CORE SLICES ARE FLUSHED TO DISK
C     ------------------------------------------------------------------
C     IY = 0 SAYS INIT - DO 1ST HISIO CALL TO GET ATTRIBUTES
C     IY > 0 SAYS ACCUMULATE SLICE - IY
C     IY < 0 SAYS FLUSH TO DISK
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(IY.GT.0) GO TO 100                 !TST FOR ACCUMULATE
      IF(IY.LT.0) GO TO 250                 !TST FOR FLUSH
C   
C     ------------------------------------------------------------------
C     INIT - GET OUTPUT HISTOGRAM ATTRIBUTES
C     ------------------------------------------------------------------
C   
      NDX(1)=1                              !OTHERWISE, INIT
      NDX(2)=1
C   
      CALL HISIO('INIT',LUD,LUH,ID,NDX,0,JHEDF,JDATH,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
      CALL HISIO('READ',LUD,LUH,ID,NDX,0,JHEDF,JDATH,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
C   
      MINX=MINC(1)+1                        !MIN X-INDEX AVAILABLE
      MAXX=MAXC(1)+1                        !MAX X-INDEX AVAILABLE
      MINY=MINC(2)+1                        !MIN Y-INDEX AVAILABLE
      MAXY=MAXC(2)+1                        !MAX Y-INDEX AVAILABLE
C   
      IDX=MINX                              !START INDEX IN IDAT
      NCH=MAXC(1)-MINC(1)+1                 !# OF X-CHANS AVAILABLE
      NWPS=NHW*NCH                          !# HALF-WDS/SLICE
      MXSL=32768/NWPS                       !MAX # SLICES IN-CORE
      NSLI=0                                !# IN-CORE SLICES
      RETURN
C   
C     ------------------------------------------------------------------
C     ACCUMULATE IN-CORE SLICES
C     ------------------------------------------------------------------
C   
  100 IF(IY.LT.MINY) GO TO 320              !TST FOR OUT OF RANGE
      IF(IY.GT.MAXY) GO TO 320              !TST FOR OUT OF RANGE
C   
      IF(NSLI.LE.0) THEN                    !TST FOR FIRST PUSH
                    ISLO=IY                 !SET  LO-SLICE
                    ISHI=IY-1               !INIT HI-SLICE
                    GO TO 120               !GO SAVE IT
                    ENDIF
C   
      IF(IY.EQ.ISHI+1) GO TO 120            !TST FOR CONTIG PUSH
C   
      NDX(1)=1                              !OTHERWISE, FLUSH
      NDX(2)=ISLO-MINY+1                    !SET NDX(2) FOR HISIO
      NC=NCH*(ISHI-ISLO+1)                  !# OF CHANNELS TOTAL
C   
      CALL HISIO('INIT',LUD,LUH,ID,NDX,NC,JHEDF,JDATH,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
      CALL HISIO('RITE',LUD,LUH,ID,NDX,NC,JHEDF,JDATH,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
C   
      ISLO=IY                               !SET  LO-SLICE
      ISHI=IY-1                             !INIT HI-SLICE
      NSLI=0                                !INIT # SLICES SAVED
C   
  120 ISHI=ISHI+1                           !INC HI-SLICE
      N=NSLI*NCH                            !INIT BUFF-INDEX
      IF(NHW.EQ.1) GO TO 140                !TST FOR HALF-WORD
C   
      DO 130 I=1,NCH                        !SAVE FULL-WORD
      N=N+1
      JDATF(N)=IDAT(I)
  130 CONTINUE
      GO TO 200                             !GO TST FOR FULL BUFF
C
  140 IF(CNEGSET.NE.'OFF ') GO TO 160       !TST FOR MODIFY NEG#
C   
      DO 150 I=1,NCH                        !SAVE HALF-WORD
      N=N+1
      JDATH(N)=IAND(IDAT(I),Z'FFFF')
  150 CONTINUE
      GO TO 200
C
  160 DO 170 I=1,NCH                        !SAVE HALF-WORD AND
      N=N+1                                 !MODIFY NEGATIVE#
      IT=IDAT(I)
      IF(IT.LT.0) IT=NEGSET                 !RESET IF NEGATIVE
      JDATH(N)=IAND(IT,Z'FFFF')             !MASK TO 16-BITS
  170 CONTINUE
C   
  200 NSLI=NSLI+1                           !INC # SLICES SAVED
      IF(NSLI.LT.MXSL) RETURN               !TST FOR MAX #
C   
C     ------------------------------------------------------------------
C     FLUSH IN-CORE BUFFER TO DISK & RESET POINTERS
C     ------------------------------------------------------------------
C   
  250 NDX(1)=1                              !SET X-INDEX
      NDX(2)=ISLO-MINY+1                    !SET NDX(2) FOR HISIO
      NC=NCH*(ISHI-ISLO+1)                  !TOTAL # CHANNELS
C   
      CALL HISIO('INIT',LUD,LUH,ID,NDX,NC,JHEDF,JDATH,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
      CALL HISIO('RITE',LUD,LUH,ID,NDX,NC,JHEDF,JDATH,IERR,MSER)
      CALL HISIOER(IERR,MSER)
C   
      NSLI=0                                !RESET # SLICES SAVED
      RETURN
C   
C     ------------------------------------------------------------------
C     RETURN ERROR MESSAGES
C     ------------------------------------------------------------------
C   
  320 WRITE(CMSSG,325)
  325 FORMAT('REQUEST OUTPUT OF ILLEGAL SLICE')
      CALL MESSLOG(6,7)
      IERR=1
      RETURN
      END
