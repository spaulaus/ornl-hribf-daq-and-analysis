C$PROG DISP      - Displays table of scaler values & rates
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 05/22/2005
C     ******************************************************************
C
      SUBROUTINE DISP(MODE)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                               VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
      CHARACTER*4 KI
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      CHARACTER*4  KTERM
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
      COMMON/SDXX/ ISKIP(512),SKIPLAB(8,512),NSKIP
      INTEGER*4    ISKIP,     SKIPLAB,       NSKIP
C
      CHARACTER*32 SKIPLABC(512)
      EQUIVALENCE (SKIPLABC,SKIPLAB)
C     ------------------------------------------------------------------
      INTEGER*4    JV(3),KV(3),JV2(2),KV2(2)
C
      INTEGER*4    IBUF(4096),TMF(2)
C
      INTEGER*2    IBUH(8192),TMH(4)
C
      CHARACTER*16 CBUF
C
      INTEGER*2    KBUH(8)
C
      EQUIVALENCE (CBUF,KBUH)
C
      REAL*4       VNF(512),VOF(512),VDF(512)
C
      EQUIVALENCE (JV2,JV),(KV2,KV),(LV2,LV),(MV2,MV)
      EQUIVALENCE (VNF,VN),(VOF,VO),(VDF,VD)
      EQUIVALENCE (TMF,TMH),(IBUH,IBUF)
C
      SAVE
C
C     ------------------------------------------------------------------
C     DISPLAYS SCALER VALUES AND RATES
C     ------------------------------------------------------------------
C
      DO 5 I=1,4096
      IBUF(I)=0
    5 CONTINUE
C
      N=0
      NXH=1
      NBYLL=4
C
      IF(KTERM.EQ.'ANSI') NBYLL=8
C
      DO 50 I=1,NT
C
      IF(KI(I).EQ.'NONE') GO TO 50
C
      N=N+1
C
      IF(ISKIP(N).NE.0) THEN
      CALL SENDBUF(6,LALOC(1,N),NBYLL)
      CALL SENDBUF(6,SKIPLAB(1,N),27)
      N=N+1
      ENDIF
C
      CALL SENDBUF(6,LALOC(1,N),NBYLL)
C
      CALL SENDBUF(6,LA(1,I),11)
C
      IF(MODE.EQ.0) GO TO 10
C
   10 TMF(1)=DALOC(1,N)
      TMF(2)=DALOC(2,N)
      IBUH(NXH)  =TMH(1)
      IBUH(NXH+1)=TMH(2)
C
      IF(KTERM.NE.'ANSI') THEN
                          NXH=NXH+2
                          GO TO 12
                          ENDIF
C
      IBUH(NXH+2)=TMH(3)
      IBUH(NXH+3)=TMH(4)
      NXH=NXH+4
C
   12 IF(KI(I).NE.'FLOT') GO TO 20
C
      CALL FLO8(VNF(I),JV)
      CALL FLO8(VDF(I),KV)
      WRITE(CBUF,15)JV2,KV2
   15 FORMAT(2A4,2A4)
      GO TO 30
C
   20 CALL FLI8(VN(I), JV)
      CALL FLO8(VDF(I),KV)
C
      WRITE(CBUF,15)JV2,KV2
C
   30 JJ=0
      DO 40 II=1,8
      IBUH(NXH+JJ)=KBUH(II)
      JJ=JJ+1
   40 CONTINUE
C
      NXH=NXH+11
C
   50 CONTINUE
C
      NBY=2*(NXH-1)
      IF(NBY.LT.4) GO TO 60
C
      CALL SENDBUF(6,IBUF,NBY)
C
   60 NBYY=4
      IF(KTERM.EQ.'ANSI') NBYY=8
C
      CALL SENDBUF(6,HOMLOC,NBYY)
C
      RETURN
      END
