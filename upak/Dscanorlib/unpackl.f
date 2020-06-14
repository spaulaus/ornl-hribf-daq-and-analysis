C$PROG UNPACKL   - Calls UNPACKL1, UNPACKL2, or UNPACKL3 as per LFORM
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/09/99/
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
      SUBROUTINE UNPACKL(
     &           IBUF,   !I*2 - raw data buffer
     &           NHW,    !I*4 - # of I*2 words in IBUF
     &           MXID,   !I*4 - max-ID (dimension of IDLST & EXPEV)
     &           IDLST,  !I*4 - ID-list   for returned event
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
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM                      
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      INTEGER*2 IBUF(*)
C
      INTEGER*4 NHW,NPAR,IERR,EXPEV(*),IDLST(*),DALST(*),MXID
C
      CHARACTER*4  DONE
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(LFORM.EQ.'L003') THEN
      CALL UNPACKL3(
     &              IBUF,   !I*2 - raw data buffer
     &              NHW,    !I*4 - # of I*2 words in IBUF
     &              MXID,   !I*4 - max-ID (dimension of IDLST & EXPEV)
     &              IDLST,  !I*4 - ID-list   for returned event
     &              DALST,  !I*4 - Data-list for returned event
     &              EXPEV,  !I*4 - expanded-event array
     &              NPAR,   !I*4 - # of parameters in this event
     &              DONE,   !C*4 - YES/NO - requests new IBUF
     &              IERR)   !I*4 - 0 means OK, nonzero means error
      RETURN
      ENDIF
C
C
      IF(LFORM.EQ.'L002') THEN
      CALL UNPACKL2(
     &              IBUF,   !I*2 - raw data buffer
     &              NHW,    !I*4 - # of I*2 words in IBUF
     &              MXID,   !I*4 - max-ID (dimension of IDLST & EXPEV)
     &              IDLST,  !I*4 - ID-list   for returned event
     &              DALST,  !I*4 - Data-list for returned event
     &              EXPEV,  !I*4 - expanded-event array
     &              NPAR,   !I*4 - # of parameters in this event
     &              DONE,   !C*4 - YES/NO - requests new IBUF
     &              IERR)   !I*4 - 0 means OK, nonzero means error
      RETURN
      ENDIF
C
C
      IF(LFORM.EQ.'L001') THEN
      CALL UNPACKL1(
     &              IBUF,   !I*2 - raw data buffer
     &              NHW,    !I*4 - # of I*2 words in IBUF
     &              MXID,   !I*4 - max-ID (dimension of IDLST & EXPEV)
     &              IDLST,  !I*4 - ID-list   for returned event
     &              DALST,  !I*4 - Data-list for returned event
     &              EXPEV,  !I*4 - expanded-event array
     &              NPAR,   !I*4 - # of parameters in this event
     &              DONE,   !C*4 - YES/NO - requests new IBUF
     &              IERR)   !I*4 - 0 means OK, nonzero means error
      RETURN
      ENDIF
C
C
      END
