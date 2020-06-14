C$PROG DRRSUB
C
      SUBROUTINE DRRSUB(IEXST)
C
C     ****************************************************************
C     Example DRRSUB which reserves space for:
C     30 256-channel 1-D histograms at 32-bits/channel and
C     3  256*256 (range 0,199 0,199) 2-D histograms at 32-bits/channel
C     ****************************************************************
C
      IMPLICIT INTEGER*4 (A-Z)
C
      CALL DRRMAKE
C
      DO 10 HID=1,30
C
      CALL HD1D(
     &          HID,    !HIST ID
     &          2,      !# HWDS/CHANNEL
     &          2048,   !RAW  PARAM LENGTH
     &          256,    !HIST PARAM LENGTH
     &          0,      !MIN  PARAM "RANGE" VALUE
     &          255,    !MAX  PARAM "RANGE" VALUE
     &      '1D HIS')   !TITLE
C
   10 CONTINUE
C
      DO 20 HID=31,34
      CALL HD2D(
     &          HID,    !HIST ID
     &          2,      !# HWDS/CHANNEL
     &          2048,   !RAW  PARAM X-LENGTH
     &          256,    !HIST PARAM X-LENGTH
     &          0,      !MIN  X-PAR "RANGE" VALUE
     &          199,    !MAX  X-PAR "RANGE" VALUE
     &          2048,   !RAWW PARAM Y-LENGTH
     &          256,    !HIST PARAM Y-LENGTH
     &          0,      !MIN  Y-PAR "RANGE" VALUE
     &          199,    !MAX  Y-PAR "RANGE" VALUE
     &     '2D HIST')   !TITLE (40 BYTES MAX)
C
   20 CONTINUE  
C
      CALL ENDRR
      RETURN
      END     
C$PROG HISSUB    - Example HISSUB
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/09/99/
C     ******************************************************************
C
      SUBROUTINE HISSUB(IBUF,NHW)
C
      IMPLICIT NONE
C
C     ==================================================================
C     Example HISSUB
C     Processes full tape buffers.
C     Histograms all raw parameters at resolution specified by DRRSUB 
C     with his-ID = parameter number.
C     ==================================================================
      LOGICAL     BANTESTN,GATTESTI
C
      INTEGER*2   IBUF(*)
C
      INTEGER*4   NHW,NPAR,IERR,MXID,I,IX,IY
C
      INTEGER*4   IXX
C
      INTEGER*4   IDLST(2000),DALST(2000),EXPEV(2000)
C
      CHARACTER*4 DONE
C
      DATA        MXID/2000/
C     ------------------------------------------------------------------
C
      DONE='NO  '
C
  100 IERR=0
C
      CALL UNPACKL(
     &           IBUF,   !I*2 - raw data buffer
     &           NHW,    !I*4 - # of I*2 words in IBUF
     &           MXID,   !I*4 - max-ID (dimension of IDBUF & EVBUF)
     &           IDLST,  !I*4 - ID-list   for returned event
     &           DALST,  !I*4 - Data-list for returned event
     &           EXPEV,  !I*4 - Expanded event data array
     &           NPAR,   !I*4 - # of parameters in this event
     &           DONE,   !I*4 - YES/NO - requests new IBUF
     &           IERR)   !I*4 - 0 means OK, nonzero means error
C
      IF(DONE.EQ.'YES ') RETURN
C
      IF(IERR.NE.0)      GO TO 100
C
      IF(NPAR.LE.0)      GO TO 100
C
C
      DO 120 I=1,NPAR
C
      CALL COUNT1CC(IDLST(I),DALST(I),0)
C
  120 CONTINUE
C
      IX=EXPEV(15)
      IY=EXPEV(17)
C
CX    IF(.NOT.GATTESTI(1,IX)) GO TO 100
C
      CALL COUNT1CC(31,IX,IY)
C
      IF(BANTESTN(1,IX,IY)) CALL COUNT1CC(32,IX,IY)
C
      IXX=IX+2047
C
      IF(BANTESTN(1,IXX,IY)) CALL COUNT1CC(32,IX,IY)
C
      IF(BANTESTN(2,IX,IY)) CALL COUNT1CC(33,IX,IY)
C
      IF(BANTESTN(3,IX,IY)) CALL COUNT1CC(34,IX,IY)
C
      GO TO 100
C
      END
