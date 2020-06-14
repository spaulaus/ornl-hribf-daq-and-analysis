C$PROG UNPACKL3  - Unpacks L003 event to ID- & data-lists & expanded EV
C
C     ******************************************************************
C     From J.W. McConnell AT HRIBF - LAST MODIFIED by WTM 01/09/99
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
C     CALL     args are: IBUF,  NHW,   MXID
C
C     RETURNED args are: IDLST, DALST, EXPEV, NP, DONE, IERR
C     ==================================================================
C
      SUBROUTINE UNPACKL3(
     &           IBUF,   !I*2 - raw data buffer
     &           NHW,    !I*4 - # of I*2 words in IBUF
     &           MXID,   !I*4 - max-ID (dimension of IDLST & EXPEV)
     &           IDLST,  !I*4 - list of IDs  in returned event
     &           DALST,  !I*4 - Data-list for returned event
     &           EXPEV,  !I*4 - expanded-event array
     &           NPAR,   !I*4 - # of parameters in this event
     &           DONE,   !C*4 - YES/NO - requests new IBUF
     &           IERR)   !I*4 - 0 means OK, nonzero means error
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC27/ BNDX
      INTEGER*4    BNDX
C     ------------------------------------------------------------------
      INTEGER*2    IBUF(*)
C
      INTEGER*4    NHW,NPAR,IERR,IDLST(*),DALST(*),EXPEV(*),MXID
C
      INTEGER*4    MXZOT,NZOT,DATA,ID,LO,HI,I,J
C
      DATA         MXZOT,NZOT/0,0/
C
      CHARACTER*4  DONE
C
      SAVE
C
C     ------------------------------------------------------------------
C     Erase the last event in the EXPEV aray.
C     ------------------------------------------------------------------
C
      DONE='NO  '
C
      IF(MXID.GT.MXZOT) THEN              !Test for and init
      DO 5 I=1,MXID                       !expanded buffer to -1
      EXPEV(I)=-1
    5 CONTINUE
      MXZOT=MXID
      ENDIF
C
      DO 10 I=1,NZOT
      EXPEV(IDLST(I))=-1
   10 CONTINUE
      IERR=0
      NPAR=0
      NZOT=0
C
C     ------------------------------------------------------------------
C     BNDX points to first parameter in new event
C     ------------------------------------------------------------------
C
      DO 100 I=BNDX,NHW,2
C
C
      ID   = IBUF(I)                      !First number is ID
      DATA = IBUF(I+1)                    !Next  number is DATA
C
      IF(    ID.NE.-1) GO TO 50           !Tst for not an end-of-event
      IF(  DATA.NE.-1) GO TO 40           !Tst for L003 format error
      IF(NPAR.LE.0)    GO TO 40           !Tst for empty buffer
C
C                                         !Otherwise, good event found
      BNDX = I+2                          !Set index to next event
      NZOT = NPAR                         !# of parameters to zero
      RETURN                              !Return one event
C
C
C                                         !L003 format error or
   40 IERR=1                              !empty event buffer, take alt
      NZOT = NPAR                         !# parameters to zero 
C     DONE='YES '                         !Request next buffer
      BNDX=I+2                            !Set index to next event
      RETURN                              !RETURN with IERR=1
C
C
   50 ID = IAND(ID,'7FFF'X)
C
      IF(ID.EQ.0.OR.ID.GT.MXID) THEN      !? too much data for event-buf
      NZOT = NPAR                         !Save # parameters to zero
      IERR=2                              !Non-zero IERR
      BNDX=I+2                            !Set index to next event
      RETURN                              !Normal return
      ENDIF
C
C                                         !Otherwise,
      NPAR=NPAR+1                         !Inc # parameters counter
      IDLST(NPAR)=ID                      !Save ID   in IDLST
      EXPEV(ID)=IAND(DATA,'FFFF'X)        !Save DATA in event-buffer
      DALST(NPAR)=EXPEV(ID)               !Save DATA in DALST
C                                         !One parameter saved
C
  100 CONTINUE
C
C     ------------------------------------------------------------------
C     End of IBUF
C     ------------------------------------------------------------------
C
      BNDX=1                              !Set index to 1st of new buf
C
      DONE='YES '                         !Request a new buffer
C
      RETURN                              !Request new buffer.

      END
