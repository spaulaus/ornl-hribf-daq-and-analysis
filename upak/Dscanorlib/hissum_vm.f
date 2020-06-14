C$PROG HISSUM_VM - Generates/displays sum of all in-core histograms
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 02/17/99
C     ******************************************************************
C
      SUBROUTINE HISSUM_VM
C   
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC17/ IOFF(8000),IOFH(8000),NDIM(8000),NHPC(8000),
     &             LENX(8000),LENH(8000)
C
      INTEGER*2    LENX,                 NDIM,      NHPC
      INTEGER*4    IOFF,      IOFH
      INTEGER*4               LENH
C     ------------------------------------------------------------------
      INTEGER*4    MXNH
C
      PARAMETER   (MXNH=6144)
C   
      INTEGER*4    ISUM(MXNH),IDLST(MXNH)
C
      INTEGER*4    NN,IA,IB,ID,IADDR,I
C
      INTEGER*2    MEM_GET_VALUE_HW,IVALH
C
      INTEGER*4    MEM_GET_VALUE_FW,IVALU
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
      DO 10 I=1,MXNH
      ISUM(I)=0
      IDLST(I)=0
   10 CONTINUE
C
      NN=0
      DO 200 ID=1,8000
C
      IF(LENX(ID).LE.0) GO TO 200    !TEST FOR UNUSED
C
      IF(NHPC(ID).EQ.2) GO TO 100    !TEST FOR 32-BIT/CHANNEL
C
      IA=IOFH(ID)                    !DO IT FOR 16-BITS/CHANNEL
      IB=IA+LENH(ID)-1
      NN=NN+1
C
      DO 20 I=IA,IB
      IADDR=I
      IVALH=MEM_GET_VALUE_HW(IADDR)
      ISUM(NN)=ISUM(NN)+IVALH
   20 CONTINUE
C
      IDLST(NN)=ID
      GO TO 200
C
  100 IA=IOFF(ID)                    !DO IT FOR 32-BITS/CHANNEL
      IB=IA+LENH(ID)-1
      NN=NN+1
C
      DO 120 I=IA,IB
      IADDR=I
      IVALU=MEM_GET_VALUE_FW(IADDR)
      ISUM(NN)=ISUM(NN)+IVALU
  120 CONTINUE
C
      IDLST(NN)=ID
C
  200 CONTINUE
C
      WRITE(6,210)(IDLST(I),ISUM(I),I=1,NN)
  210 FORMAT(1H ,I5,'=',I7,I5,'=',I7,I5,'=',I7,I5,'=',I7,I5,'=',I7,I5,
     &              '=',I7)
C
      RETURN
      END
