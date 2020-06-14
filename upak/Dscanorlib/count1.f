C$PROG COUNT1    - Adds 1 count/call to mem hists (checks range)
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 07/19/99
C     ******************************************************************
C
      SUBROUTINE COUNT1(ID,IX,IY)
C   
C     ------------------------------------------------------------------
C     Changed to be the same as COUNT1C - better for on-line use
C     ------------------------------------------------------------------
C     COUNT 1 and CHECK  (1D and 2D)
C     ROUTINE TO ADD ONE COUNT PER CALL TO MEMORY HISTOGRAMS
C     DIFFERS FROM original COUNT1 IN THAT RANGE CHECKING IS DONE. 
C     ANY COMPRESSION OR RESCALING MUST BE DONE BY YOU PRIOR
C     TO CALL.
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
      INTEGER*4    ID,IX,IY,ICX,ICY,IC,NDX
C     ------------------------------------------------------------------
C   
      IF(NDIM(ID).LE.0)RETURN                         !Check existance
C   
      ICX=IX-IMIN(1,ID)                               !Check X-range
      ICY=IY-IMIN(2,ID)
      IF(IX.LT.IMIN(1,ID).OR.IX.GT.IMAX(1,ID))RETURN
      IC=ICX
C
      IF(NDIM(ID).EQ.2)THEN                           !Check Y-range
      IF(IY.LT.IMIN(2,ID).OR.IY.GT.IMAX(2,ID))RETURN
      ENDIF
C
      IF(NHPC(ID).EQ.2) THEN                          !Tsfr full-WD
      NDX=IOFF(ID)+IC                                 !Full-WD index
      CALL MEM_ADD1_FW(NDX)                           !Full-WD add-one
      RETURN
      ELSE
C
      NDX=IOFH(ID)+IC                                 !Half-WD index
      CALL MEM_ADD1_HW(NDX)                           !Half-WD add-one
      ENDIF
C
      RETURN
      END
