C$PROG COUNTNCC  - Adds 1 count/call to mem hists (cks range & compress)
C
C     ******************************************************************
c     By RLV at HRIBF - modeled after count1cc.f
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 02/17/99
C     ******************************************************************
C
      SUBROUTINE COUNTNCC(ID,IVAL,IX,IY)
C   
C     ------------------------------------------------------------------
C     COUNT 1 CHECK and COMPRESS (1D and 2D)
C     ROUTINE TO ADD ONE COUNT PER CALL TO MEMORY HISTOGRAMS
C     DIFFERS FROM COUNT1 IN THAT COMPRESSION AND RANGE CHECKING
C     ARE DONE. IX,IY ARE RAW PARAMETER VALUES.
C     !!!NOTE!!! requests to increment nonexistant histograms are 
C     igored without comment.
C     ------------------------------------------------------------------
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
      COMMON/SC18/ ICMP(4,8000),IMIN(4,8000),IMAX(4,8000),MAXOFF
C
      INTEGER*2    ICMP,        IMIN,        IMAX
      INTEGER*4                                           MAXOFF
C     ------------------------------------------------------------------
      INTEGER*4    ID,IVAL,IX,IY,ICX,ICY,IC,NDX
C     ------------------------------------------------------------------
C   
      IF(NDIM(ID).LE.0)RETURN                !Check existance
C   
C   
c     ICX=RSHIFT(IX,ICMP(1,ID))              !COMPRESS DEC fortran
      ICX=ISHFT(IX,-ICMP(1,ID))              !COMPRESS ansi fortran
C
C                                            ! CHECK X RANGE
      IF(ICX.LT.IMIN(1,ID).OR.ICX.GT.IMAX(1,ID))RETURN
      ICX=ICX-IMIN(1,ID)
      IC=ICX
      IF(NDIM(ID).EQ.2)THEN
c     ICY=RSHIFT(IY,ICMP(2,ID))              !COMPRESS DEC fortran
      ICY=ISHFT(IY,-ICMP(2,ID))              !COMPRESS ansi fortran
C
C                                            ! CHECK Y RANGE
      IF(ICY.LT.IMIN(2,ID).OR.ICY.GT.IMAX(2,ID))RETURN
      ICY=ICY-IMIN(2,ID)
      IC=ICY*LENX(ID)+ICX                    !CHAN-OFF FOR 2-D
      ENDIF
C   
      IF(NHPC(ID).EQ.2) THEN                 !TST FOR FULL-WD CHAN
      NDX=IOFF(ID)+IC                        !FULL-WD INDEX
      CALL MEM_ADDN_FW(NDX,IVAL)             !FULL-WD ADD-ONE
      RETURN
      ELSE
C
      NDX=IOFH(ID)+IC                        !HALF-WD INDEX
      CALL MEM_ADDN_HW(NDX,IVAL)             !HALF-WD ADD-ONE
      ENDIF
C
      RETURN
      END
