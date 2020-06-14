C$PROG UNPACKL2  - Unpacks L002 event to ID- & data-lists & expanded EV
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
C     CALL args are:     IBUF,  NHW,   MXID
C
C     RETURNED args are: IDLST, DALST, EXPEV, NP,  DONE, IERR
C     ==================================================================
C
      SUBROUTINE UNPACKL2(
     &           IBUF,   !I*2 - raw data buffer
     &           NHW,    !I*4 - # of I*2 words in IBUF
     &           MXID,   !I*4 - max-ID (dimension of IDLST & EXPEV)
     &           IDLST,  !I*4 - ID-list   for returned event
     &           DALST,  !I*4 - Data-list for returned event
     &           EXPEV,  !I*4 - expanded-event array
     &           NP,     !I*4 - # of parameters in this event
     &           DONE,   !C*4 - YES/NO - requests new IBUF
     &           IERR)   !I*4 - 0 means OK, nonzero means error
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC04/ JCNF,IHEDN,MBFL   !JCNF = cont-flag 
      INTEGER*4         IHEDN,MBFL   !NO says find next FFFF
      CHARACTER*4  JCNF
C     ------------------------------------------------------------------
      COMMON/SC27/ BNDX
      INTEGER*4    BNDX
C     ------------------------------------------------------------------
      INTEGER*2    IBUF(*)
C
      INTEGER*4    NHW,NPAR,IERR,EXPEV(*),IDLST(*),DALST(*),MXID
C
      INTEGER*4    MXZOT,NZOT,DATA,ID,NP,I,J
C
      DATA         MXZOT,NZOT,NPAR/0,0,0/
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
      IF(NPAR.EQ.0) THEN                  !Tst for completed event
      DO 10 I=1,NZOT                      !Loop to reset all EXPEV
      EXPEV(IDLST(I))=-1                  !entries that were previously
   10 CONTINUE
      ENDIF
C
      IERR=0                              !Reset error flag
      NZOT=0                              !Reset #parms to zero
C
      IF(JCNF.EQ.'YES ') GO TO 100        !tst for non-contig data 
C
      DO 20 I=BNDX,NHW                    !Look for next FFFF
      IF(IBUF(I).EQ.-1) GO TO 30
   20 CONTINUE                            !If not found,
      GO TO 220                           !Request new buffer
C
   30 JCNF='YES '
      BNDX=I+1
      NZOT=0
      NPAR=0
C
C     ------------------------------------------------------------------
C     BNDX points to first parameter in new event
C     ------------------------------------------------------------------
C
  100 DO 200 I=BNDX,NHW
C
      IF(IBUF(I).EQ.-1) THEN              !Tst for end-of-event
      BNDX=I+1                            !Set index to next event
      NZOT=NPAR                           !#parms to zero next time
      NP=NPAR                             !#parms in event
      NPAR=0                              !Says Completed event
      RETURN
      ENDIF
C
C
      IF(IBUF(I).LT.0) THEN               !Tst for ID word
      ID=IAND(IBUF(I),'7FFF'X)            !Get ID value
      IF(ID.LE.0.OR.ID.GT.MXID) GO TO 300 !Tst for error
      GO TO 200
      ENDIF
C
C
      DATA=IBUF(I)                        !Otherwise, its data
      EXPEV(ID)=IAND(DATA,'FFFF'X)        !Store in EXPEV
      IF(NPAR.GE.MXID) GO TO 300          !Tst for error
      NPAR=NPAR+1                         !Inc # of parameters
      IDLST(NPAR)=ID                      !Store   ID in list
      DALST(NPAR)=DATA                    !Store DATA in list
      ID=ID+1                             !Inc ID# for next time
C
  200 CONTINUE
C
C     ------------------------------------------------------------------
C     End of IBUF
C     ------------------------------------------------------------------
C
  220 BNDX=1                              !Set index to 1st of new buf
C
      DONE='YES '                         !Request a new buffer
C
      RETURN
C
C     ------------------------------------------------------------------
C     Error returns
C     ------------------------------------------------------------------
C
  300 IERR=1                              !Set error flag
      BNDX=I+1                            !Point to next IBUF word
      JCNF='NO  '                         !Set to "look for" next event
      NZOT=NPAR                           !#parms to zero next time
      NPAR=0                              !Says completed event
      NP=0                                !Empty event buffer
      RETURN
C
      END
