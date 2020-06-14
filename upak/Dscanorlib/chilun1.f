C$PROG CHILUN1   - Unpacks L001 events for CHIL processor (CHILUM3)
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/27/99/
C     ******************************************************************
C
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
C     RETURNED args are: IDLST, DALST, EXPEV, NPAR,  DONE, IERR
C     ==================================================================
C
      SUBROUTINE CHILUN1(
     &           IBUF,   !I*2 - raw data buffer
     &           NHW,    !I*4 - # of I*2 words in IBUF
     &           EXPEV,  !I*4 - expanded-event array
     &           MXPAR,  !I*4 - Max parameter ID
     &           STAT)   !C*4 - 'GOOD' means normal event
C                        !      'DONE'means end-of-buffer
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM                      
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
C     ------------------------------------------------------------------
      INTEGER*2    IBUF(*),MASK
C
      CHARACTER*4  STAT,JDONE
C
      INTEGER*4    NHW,NPAR,IERR,EXPEV(*),MXID,MXPAR
C
      INTEGER*4    NCALL,MAXEV,NEV,NI,I,NP
C
      DATA         NCALL,JDONE/0,'NO  '/
C
      DATA         MASK/'7FFF'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      STAT='GOOD'
C
      IF(JDONE.EQ.'YES ') THEN
      STAT='DONE'
      NCALL=0
      JDONE='NO  '
      RETURN
      ENDIF
C
      IF(NCALL.EQ.0) THEN
      MAXEV=(LSTL-NSKIP)/MAXIP
      NEV=0
      NI=NSKIP
      STAT='GOOD'
      NCALL=1
      ENDIF
C
      NP=0
      DO 100 I=1,MAXIP
      EXPEV(I)=0
      NI=NI+1
      IF(IBUF(NI).LE.0) GO TO 100
      NP=NP+1
      EXPEV(I)=IAND(IBUF(NI),MASK)
  100 CONTINUE
C
      NPAR=NP
      NEV=NEV+1
C
      JDONE='NO  '
      IF(NEV.GE.MAXEV) JDONE='YES '
C
      RETURN
      END
