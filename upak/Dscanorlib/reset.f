C$PROG RESET     - Resets pointers etc, called when you "zero"
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/01/98
C     ******************************************************************
C
      SUBROUTINE RESET
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    CLX,  CLY
C
      PARAMETER   (CLX=2,CLY=2048)            !MBUF DIMENSIONS
C
      COMMON/SC08/ MBUF,JBN,IPO,NBC,LX,LY,LXB,LJP
C
      INTEGER*2    MBUF(0:CLX-1,0:CLY)        !PLUS A SPARE

      INTEGER*2    JBN(0:CLY-1),IPO(0:CLY-1),NBC(CLY)

      INTEGER*4    LX,LY,LXB,LJP
C     ------------------------------------------------------------------
      COMMON/SC09/ JHPC(4096)
      INTEGER*4    JHPC
C     ------------------------------------------------------------------
      BYTE BYHPC(16384)
      EQUIVALENCE (BYHPC,JHPC)
C
      INTEGER*4    I
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ==================================================================
C     ROUTINE TO RESET POINTERS ETC - GETS CALLED WHEN YOU DO "ZERO"
C     ==================================================================
C
      DO 20 I=1,LY            !LOOP TO RESET POINTERS, ETC
      NBC(I)=0                !# BLOCKS ON DISK FOR CHAIN-I
      IPO(I-1)=1              !BUFF-POINTER     FOR CHAIN-I
      MBUF(0,I-1)=-1          !PREVIOUS BLOCK POINTER FOR CHAIN-I
      JBN(I-1)=I-1            !REAL BUFF# FOR ITH MBUF
   20 CONTINUE
      LJP=LY+1                !LAST MBUF# PUSHED (WRITTEN TO DISK)
C                             !NEXT ALTERNATE BUFF TO ASSIGN
C
C
      DO 30 I=1,LY            
      JBN(I-1)=-1                      !SET TO -1 FOR 16-BITS/CHAN
      IF(BYHPC(I).NE.0) JBN(I-1)=-2    !SET TO -2 FOR 32-BITS/CHAN
   30 CONTINUE
C
      RETURN
      END
