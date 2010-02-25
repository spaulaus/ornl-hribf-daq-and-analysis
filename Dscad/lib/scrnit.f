C$PROG SCRNIT    - Clears SCAD screen & writes labels in proper location 
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 05/22/2005
C     ******************************************************************
C
      SUBROUTINE SCRNIT
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
      COMMON/SDXX/ ISKIP(512),SKIPLAB(8,512),NSKIP
      INTEGER*4    ISKIP,     SKIPLAB,       NSKIP
C
      CHARACTER*32 SKIPLABC(512)
      EQUIVALENCE (SKIPLABC,SKIPLAB)
C     ------------------------------------------------------------------
      INTEGER*4 STAT
C
      SAVE
C
C     ------------------------------------------------------------------
C     CLEARS SCREEN AND WRITES LABELS IN THEIR PROPER LOCATION
C     ------------------------------------------------------------------
C
      CALL SCLR
C
      NBY=4
      IF(KTERM.EQ.'ANSI') NBY=8
C
      N=0
      NSK=0
C
      DO 20 I=1,NT
C
      IF(KI(I).EQ.'NONE') GO TO 20
      N=N+1
C
      IF(ISKIP(N).NE.0) THEN
      CALL SENDBUF(6,LALOC(1,N),NBY)
      CALL SENDBUF(6,SKIPLAB(1,N),27)
      N=N+1
      ENDIF
C
      CALL SENDBUF(6,LALOC(1,N),NBY)
C
      CALL SENDBUF(6,LA(1,I),11)
C
   20 CONTINUE
C
      CALL DISP(0)
C
      RETURN
      END
