C$PROG BUFMAN    - Buffer manager for routine REBUF
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE BUFMAN(JFIR,JLAS,NUM)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM03/ LBUF(24576)
      INTEGER*4    LBUF
C     ------------------------------------------------------------------
      COMMON/LM19/ JCNF,IREBF
      CHARACTER*4  JCNF,IREBF
C     ------------------------------------------------------------------
      INTEGER*4    JFIR,JLAS,NUM
C
      INTEGER*4    NN,IA,IB,NP,I
C
      INTEGER*2 LLIST(49152),NULIST(16384),PBUF(2000),LIST
C
      EQUIVALENCE (LLIST,LBUF),(NULIST,LBUF(16385))
C
      DATA NP/0/
C
      INTEGER*2    XFFFF
      DATA         XFFFF/-1/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      NN=0
      NUM=0
      IA=JFIR
      IB=JLAS
C
      IF(JCNF.EQ.'YES ') GO TO 50
C
      DO 10 I=IA,IB
      IF(LLIST(I).EQ.XFFFF) GO TO 20
   10 CONTINUE
      RETURN
C
   20 JCNF='YES '
      IA=I+1
      NP=0
C
   50 DO 100 I=IA,IB
C
      IF(LLIST(I).EQ.XFFFF) THEN
      IF(NP.GT.1) CALL REBUF(PBUF,NP,NULIST,NN)
      NP=0
      GO TO 100
                            ENDIF
C
      NP=NP+1
      PBUF(NP)=LLIST(I)
C
  100 CONTINUE
      NUM=NN
      RETURN
      END
