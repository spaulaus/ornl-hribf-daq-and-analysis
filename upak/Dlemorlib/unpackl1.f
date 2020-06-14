C$PROG UNPACKL1  - Unpacks L001 events to ID- & data-lists & expanded EV
C
C     ******************************************************************
C     From J.W. McConnell AT HRIBF - LAST MODIFIED by WTM 04/24/2002
C     ******************************************************************
C
C     ==================================================================
C     Routine returns one event from the current input buffer.
C
C     The array IDLST containes a list of parameter IDs in the order
C     found in the input buffer.
C
C     The array DALST contains the data associated with IDLST entries.
C
C     NP is the number of parameters for this event.  
C     Hence, only IDLST(1) thru IDLST(NP) are valid.
C
C     The array EXPEV is the expanded parameter data. 
C     Use the parameter ID as the index to this array.  
C
C     For example, dataum for parameter 17 is stored in EXPEV(17).
C     If EXPEV(I) is zero, either this event did not have that 
C     parameter or the dataum was zero.
C
C     The arrays IDLST, DALST &  EXPEV should be dimensioned as large
C     as the maximum parameter-ID (MXID) expected. 
C     ------------------------------------------------------------------
C     CALL args are:     IBUF,  NHW,   MXID
C
C     RETURNED args are: IDLST, DALST, EXPEV, NP,  DONE, IERR
C     ==================================================================
C
      SUBROUTINE UNPACKL1(
     &           IBUF,   !I*2 - raw data buffer
     &           NHW,    !I*4 - # of I*2 words in IBUF
     &           MXID,   !I*4 - max-ID (dimension of IDLST & EXPEV)
     &           IDLST,  !I*4 - ID-list   for returned event
     &           DALST,  !I*4 - Data-list for returned event
     &           EXPEV,  !I*4 - expanded-event array
     &           NP  ,   !I*4 - # of parameters in this event
     &           DONE,   !C*4 - YES/NO - requests new IBUF
     &           IERR)   !I*4 - 0 means OK, nonzero means error
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      INTEGER*2    IBUF(*),MASK
C
      INTEGER*4    NHW,NP,IERR,EXPEV(*),IDLST(*),DALST(*),MXID
C
      INTEGER*4    NCALL,MAXEV,NEV,NI,I
C
      DATA         NCALL/0/
C
      DATA         MASK/Z'7FFF'/
C
      CHARACTER*4  DONE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(NCALL.EQ.0) THEN
      MAXEV=(LSTL-NSKIP)/MAXIP
      NEV=0
      NI=NSKIP
      DONE='NO  '
      NCALL=1
      ENDIF
C
      DO 100 I=1,MAXIP
      NI=NI+1
      IDLST(I)=I
      EXPEV(I)=IAND(IBUF(NI),MASK)
      DALST(I)=EXPEV(I)
  100 CONTINUE
C
      NP=MAXIP
      NEV=NEV+1
C
      IF(NEV.GE.MAXEV) THEN
      DONE='YES '
      NCALL=0
      ENDIF
C
      RETURN
      END
