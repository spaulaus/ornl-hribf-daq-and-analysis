C$PROG CHILUN    - Event unpack routine for CHIL processor (CHILUM3)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/12/98
C     Adapted from McConnell's  UNPACKBB
C     ******************************************************************
C
C     Call arguments:
C     INTEGER*2  IBUF  - raw event data buffer
C     INTEGER*4  NHW   - number of INT*2 words in IBUF
C     INTEGER*4  MXPAR - Max ID.  EVBUF must be dimensioned
C                        at least as great as MXPAR
C     Returns:
C     INTEGER*4  EVBUF(ID) - Event data for parameter ID
C     INTEGER*4  NPAR - Number of parameters in this event.
C     INTEGER*4  STAT - 'GOOD' means everything normal
C                STAT = 'NULL' means empty event
C                STAT = 'DONE' means end of buffer
C                STAT = 'ERR ' means L003 format error
C
C     NPAR = No. of parameters stored in EVBUF
C     BNDX = Current index in IBUF
C     ID   = Parameter-ID, i.e. index in EVBUF
C     ==================================================================
C     Unpacks one event per call from IBUF
C     ==================================================================
C
      SUBROUTINE CHILUN(IBUF,NHW,EVBUF,MXPAR,STAT)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB
      CHARACTER*4                                   LFORM
C     ------------------------------------------------------------------
C
      INTEGER*2 IBUF(*)
C
      INTEGER*4 EVBUF(*)
C
      INTEGER*4 NHW,NPAR,MXPAR
C
      CHARACTER*4  STAT
C
      INTEGER*4 DATA,I,J,ID,BNDX/1/
C
      INTEGER*4 X7FFF,XFFFF
      DATA      X7FFF,XFFFF/'7FFF'X,'FFFF'X/
C
      INTEGER*4 IAND
C
      SAVE
C
C     ==================================================================
C
      IF(LFORM.EQ.'L001') THEN       !Tst for format = L001 
      CALL CHILUN1(IBUF,
     &             NHW,
     &             EVBUF,
     &             MXPAR,
     &             STAT)
      RETURN
      ENDIF
C                                    !Otherwise, its   L003
C
C     ==================================================================
C
      IF(STAT.EQ.'INIT') BNDX=1
C
      NPAR = 0
      STAT = 'GOOD'
C
C     ------------------------------------------------------------------
C     BNDX points to first parameter in new event
C     ------------------------------------------------------------------
C
      DO 100 I = BNDX, NHW, 2        !Loop to end of buffer
C
      ID   = IBUF(I)                 !First # is ID followed by DATA
      DATA = IBUF(I+1)
C
C     ------------------------------------------------------------------
C     For: ID .NE. -1   - we have a normal parameter
C     ------------------------------------------------------------------
C
      IF(ID.NE.-1)   THEN
C
      ID = IAND(ID,X7FFF)
C
      IF(ID.EQ.0.OR.ID.GT.MXPAR) THEN  !Tst for illegal ID number
C                                      !If illegal,
      STAT = 'ERR '                    !Error status
      RETURN
      ENDIF
C
C                                   !Otherwise, we have one
      NPAR = NPAR + 1               !parameter extraction completed
      EVBUF(ID) = IAND(DATA,XFFFF)  !Store DATA
      GO TO 100                     !Go to end of loop
      ENDIF
C
C     ------------------------------------------------------------------
C     For:  ID = -1   - we have an end-of-event (possible)
C     ------------------------------------------------------------------
C
      IF(DATA.EQ.-1) THEN           !Normal end-of-event found
C
      IF(NPAR.GT.0)THEN             !Check for any data loaded
      BNDX = I + 2                  !Move index to start of next event
      RETURN                        !Return one event
      ENDIF
C
C     ------------------------------------------------------------------
C     Otherwise, its an error
C     ------------------------------------------------------------------
C
      BNDX = I + 2                  !Move index to start of next event
      STAT ='NULL'                  !NULL status
      RETURN
      ENDIF
C
  100 CONTINUE
C
C     ------------------------------------------------------------------
C     When we get here, we have reached the end of ibuf. Reset the index
C     for IBUF to the beginning of the buffer for the next call.
C     ------------------------------------------------------------------
C
      BNDX = 1
      STAT = 'DONE'                 !Request new buffer
      RETURN
C
      END
