C$PROG SETUP     - Screen setup for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 05/22/2005
C     ******************************************************************
C
      SUBROUTINE SETUP(ISETX)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,
     &                               VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 TY,KI
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      common/sd02a/ modty(512),vmemod(20),vmesn(20),vmeidx(20),nvme
      character*8   modty,     vmemod
      integer*4                           vmesn,    vmeidx,    nvme
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      CHARACTER*4  KTERM
C     ------------------------------------------------------------------
      COMMON/SD09/ CC(512),NN(512),AA(512),FF(512),VBUF(512),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
      COMMON/SD12/ SCNDX(10),LIMLO(10),LIMHI(10),NLIM,MXLIM
      INTEGER*4    SCNDX,                        NLIM,MXLIM
      REAL*4                 LIMLO,    LIMHI
C     ------------------------------------------------------------------
      COMMON/SD13/ ISET,LULG
      CHARACTER*4  ISET
      INTEGER*4         LULG
C     ------------------------------------------------------------------
      COMMON/SDXX/ ISKIP(512),SKIPLAB(8,512),NSKIP
      INTEGER*4    ISKIP,     SKIPLAB,       NSKIP
C
      CHARACTER*32 SKIPLABC(512)
      EQUIVALENCE (SKIPLABC,SKIPLAB)
C     ------------------------------------------------------------------
      INTEGER*4    WINCOL,WINROW,IERR
C
      CHARACTER*4  ISETX,NORMF,ITMP,LASTYP
C
      CHARACTER*320 CLWDL
      CHARACTER*4   CLWD(2,40),CIWD(20)
      EQUIVALENCE  (CLWD,LWD),(CIWD,IWD),(CLWDL,LWD)
C
      CHARACTER*1   COMBYT
      EQUIVALENCE  (COMBYT,IWD)
C
      CHARACTER*32  COMLAB
      EQUIVALENCE  (COMLAB,IWD)
C
      REAL*4       XV
C
      INTEGER*4    LOCOD(2),COLWID
C
      BYTE         BYCOD(4,2),BTMP(4)
C
      EQUIVALENCE (BYCOD,LOCOD),(BTMP,ITMP)
C
      INTEGER*4    X20
      DATA         X20/'20'X/
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     SETUP - READS THE SCA-FILE AND SETS UP THE REQUIRED ARRAYS, ETC
C     NEEDED TO DIRECT THE READING AND DISPLAY
C     ------------------------------------------------------------------
C     LA(I,J)  = LABEL  FOR SCALER-J
C     CN(J)    = CRATE# FOR SCALER-J
C     SN(J)    = SLOT#  FOR SCALER-J
C      A(J)    = SUBAD  FOR SCALER-J
C      F(J)    = F-CLR  FOR SCALER-J
C     TY(J)    = TYPE   FOR SCALER-J  ('ECL ' OR '    ')
C     KI(J)    = KIND   FOR SCALER-J  ('    ' OR 'FLOT' OR 'NONE')
C     VN(J)    = NEW-V  FOR SCALER-J
C     VO(J)    = OLD-V  FOR SCALER-J
C     VD(J)    = VN-VO  FOR SCALER-J
C     PV(J)    = V-PNTR FOR SCALER-J  (POINTS TO VALUE IN VBUF)
C     LO(J)    = C-PNTR FOR SCALER-J  (FIRST ENTRY IN POL-OPR FOR CAL)
C     HI(J)    = C-PNTR FOR SCALER-J  (LAST  ENTRY IN POL-OPR FOR CAL)
C
C     NR       = # OF RAW SCALERS
C     NC       = # OF CAL SCALERS
C     NT       = TOTAL # OF SCALERS (NR+NC)
C     NDD      = TOTAL # SCALERS TO BE DISPLAYED
C
C     VBUF     = VALUE-BUFFER FOR ECL SCALERS (READ 32/CNAF)
C     POL      - POINTS TO SYMBOLS REFERENCED IN SCALER COMPUTATION
C     OPR      - OPERAND LIST ASSOCIATED WITH REFERENCED SYMBOLS
C
C     ECN(K)   = CRATE# FOR ECL SCALER-K
C     ESN(K)   = SLOT#  FOR ECL SCALER-K
C     NEC      = NUMBER OF  ECL SCALERS
C
*     modty(j) = VME module type (SIS3820 or CAEN820)
*     nvme     = number of VME scalers
*
C     LALOC(J) = PRE-CODED SCREEN LOCATION FOR SCALER-J LABEL
C     DALOC(J) = PRE-CODED SCREEN LOCATION FOR SCALER-J DATA
C
C     COLWID   = Col-width for scaler (name, count, rate)
C     ------------------------------------------------------------------
C
      CALL WINSIZE(WINCOL,WINROW,IERR)
C
      IADD=0
      IF(NLIM.GT.0) IADD=2
      MAXR=WINROW-NLIM-IADD-2
      MAXC=WINCOL
C
      ISETX='NO  '
      LASTYP=' '
      N=0
      NR=0
      NC=0
      NT=0
      NDD=0
      NSKIP=0
      LINO=0
      COLWID=31
C
      MAXCC=(MAXC+1)/COLWID
      MAXD=MAXR*MAXCC-1
      MAXDD=MAXD+1
C
      DO 20 J=1,500
      DO 10 I=1,3
      LA(I,J)='20202020'X
   10 CONTINUE
      KI(J)='    '
      TY(J)='    '
      SKIPLABC(J)=' '
      ISKIP(J)=0
   20 CONTINUE
C
   50 LINO=LINO+1
C
      READ(LULG,55,END=210)IWD
   55 FORMAT(20A4)
C
      IF(CIWD(1).EQ.'$END') GO TO 200
C
      IF(CIWD(1).EQ.' ') THEN
      IF(LASTYP.NE.' ') GO TO 330
      ISKIP(NSKIP+N+1)=1
      NSKIP=NSKIP+1
      LASTYP='HED'
      GO TO 50
      ENDIF
C
      IF(COMBYT.EQ.'#') THEN
      IF(LASTYP.NE.' ') GO TO 330
      ISKIP(NSKIP+N+1)=1
      SKIPLABC(NSKIP+N+1)=COMLAB
      NSKIP=NSKIP+1
      LASTYP='HED'
      GO TO 50
      ENDIF
C
      LASTYP=' '
C
      IA=NXBL(IWD,1,80)
      IF(IA.GT.12) GO TO 300
      IF(IA.LE.0)  GO TO 300
C
      N=N+1
      IF(N.GT.MAXT) GO TO 310
C
      CALL LODUP(IWD,1,IA,LA(1,N),1)
C
      ib = ifind(iwd,'3b'x,1,80)
      if (ib .le. 0) ib = 81
      ib = ib - 1
      CALL GREAD(IWD,LWD,ITYP,NF,IA,ib,NTER)
      IF(NTER.NE.0) GO TO 300
C
      read(clwdl,1000)modty(n)
 1000 format(a8)
      if(modty(n) .eq. 'SIS3820') go to 1010
      if(modty(n) .eq. 'CAEN820') go to 1010
      modty(n) = ' '
      go to 1030
*
 1010 read(clwdl,1020)modty(n),sn(n),a(n)
 1020 format(a8,2i8)
      if (sn(n) .le. 0) go to 340
      cn(n) = -1
      ty(n) = 'VME '
      go to 1040
*
 1030 continue
**      write(*,9000) n,nf
**9000  format('N = ',i3,' NF = ',i2)
      if (nf .eq. 5) then
        READ(CLWDL,60)CN(N),SN(N),A(N),F(N),TY(N)
   60   FORMAT(4I8,A4)
      elseif (nf .eq. 4 .and. ityp(4) .eq. 2) then
        read(clwdl,60)cn(n),sn(n),a(n),f(n)
        ty(n) = '    '
      elseif (nf .eq. 4 .and. ityp(4) .eq. 1) then
        read(clwdl,61)cn(n),sn(n),a(n),ty(n)
   61   format(3i8,a4)
        f(n) = 0
      elseif (nf .eq. 3) then
        read(clwdl,60)cn(n),sn(n),a(n)
        f(n) = 0
        ty(n) = '    '
      elseif (nf .lt. 3) then
        go to 300
      endif
**      write(*,9010) n,cn(n),sn(n),a(n),f(n),ty(n)
**9010  format('N = ',i3,3x,4i5,a4)
C
 1040 continue
      IA=IFIND(IWD,'3B'X,1,80)
C
      IF(IA.LE.0) THEN
                  NDD=NDD+1
                  IF(NDD+NSKIP.GT.MAXD) GO TO 320
                  GO TO 50
                  ENDIF
C
      IA=IA+1
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER)
      IF(NTER.NE.0) GO TO 300
C
      NORMF='NO  '
      DO 70 I=1,NF
      IF(CLWD(1,I).EQ.'NOD ') KI(N)='NONE'
      IF(CLWD(1,I).EQ.'NOR ') THEN
                              NORI=N
                              NORMF='YES '
                              NORF=1.0
                              ENDIF
   70 CONTINUE
C
      IF(KI(N).NE.'NONE') NDD=NDD+1
      IF(NDD+NSKIP.GT.MAXD) GO TO 320
C
      IF(NORMF.EQ.'NO  ') GO TO 50
C
      DO 80 I=1,NF
      IF(ITYP(I).NE.2) GO TO 80
      CALL MILV(LWD(1,I),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 300
      NORF=XV
   80 CONTINUE
C
      GO TO 50
C
  200 NR=N
      NT=NR
      CALL COMSET
C
      GO TO 220
C
  210 NR=N
      NT=N
C
  220 NROW=0
      NCOL=1
C
      IF(KTERM.EQ.'ANSI') GO TO 250
C
      DO 230 I=1,MAXDD
      NROW=NROW+1
      IF(NROW.GT.MAXR) THEN
                       NROW=1
                       NCOL=NCOL+COLWID
                       ENDIF
C
      LALOC(1,I)='00003D1B'X
      DALOC(1,I)='00003D1B'X
C
      IT=X20+NROW-1
      CALL ISBYTE(IT,LALOC(1,I),2)
      IT=X20+NCOL-1
      CALL ISBYTE(IT,LALOC(1,I),3)
      IT=X20+NROW-1
      CALL ISBYTE(IT,DALOC(1,I),2)
      IT=X20+NCOL+11-1
      CALL ISBYTE(IT,DALOC(1,I),3)
  230 CONTINUE
      GO TO 290
C
  250 DO 270 I=1,MAXDD
      NROW=NROW+1
      IF(NROW.GT.MAXR) THEN
                       NROW=1
                       NCOL=NCOL+COLWID
                       ENDIF
C
      LOCOD(1)='00005B1B'X
      LOCOD(2)='4800003B'X
C
      WRITE(ITMP,255)NROW
  255 FORMAT(I2.2)
      BYCOD(3,1)=BTMP(1)
      BYCOD(4,1)=BTMP(2)
      LALOC(1,I)=LOCOD(1)
      DALOC(1,I)=LOCOD(1)
C
      WRITE(ITMP,255)NCOL
      BYCOD(2,2)=BTMP(1)
      BYCOD(3,2)=BTMP(2)
      LALOC(2,I)=LOCOD(2)
C
      MCOL=NCOL+11
      WRITE(ITMP,255)MCOL
      BYCOD(2,2)=BTMP(1)
      BYCOD(3,2)=BTMP(2)
      DALOC(2,I)=LOCOD(2)
C
  270 CONTINUE
C
      LOCOD(1)='00005B1B'X
      LOCOD(2)='4800003B'X
      WRITE(ITMP,255)MAXR
      BYCOD(3,1)=BTMP(1)
      BYCOD(4,1)=BTMP(2)
C
      NKOL=1
      WRITE(ITMP,255)NKOL
      BYCOD(2,2)=BTMP(1)
      BYCOD(3,2)=BTMP(2)
      DATLOC(1)=LOCOD(1)
      DATLOC(2)=LOCOD(2)
C
      LOCOD(1)='00005B1B'X
      LOCOD(2)='4800003B'X
      WRITE(ITMP,255)MAXR+2
      BYCOD(3,1)=BTMP(1)
      BYCOD(4,1)=BTMP(2)
C
      NKOL=1
      WRITE(ITMP,255)NKOL
      BYCOD(2,2)=BTMP(1)
      BYCOD(3,2)=BTMP(2)
      HOMLOC(1)=LOCOD(1)
      HOMLOC(2)=LOCOD(2)
C
  290 CALL SETECL
C
      CALL SETPAR
C
      CLOSE(UNIT=1)
C
      ISETX='YES '
C
      RETURN
C
  300 WRITE(6,305) lino
  305 FORMAT(1H ,'SYNTAX ERROR on line ',i3)
      GO TO 400
C
  310 WRITE(6,315)MAXT
  315 FORMAT(1H ,'MORE THAN',I4,' RAW SCALERS REQUESTED')
      GO TO 400
C
  320 CALL DINGER(3)
      WRITE(6,325)MAXD,NLIM
  325 FORMAT(1H ,'Window too small to display ',I3,' scalers & ',I3,
     &           ' limits')
      GO TO 400
C
  330 CALL DINGER(3)
      WRITE(6,335)LINO
  335 FORMAT('Multiple Heading Lines defined on ',I3,' Not allowed')
      STOP
C
  340 write(6,345) lino
  345 format(1h ,'VME scaler number must be greater than 0 on ',i3)
      go to 400
  400 RETURN
      END
