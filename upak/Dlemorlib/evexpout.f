C$PROG EVEXPOUT  - Outputs 1 expanded event to stream-1 (USER-mode)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE EVEXPOUT(EVBUF,ILO,IHI)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    DUM
      PARAMETER   (DUM=4)
C
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2                                OUBUF(0:DUM)
C     ------------------------------------------------------------------
      COMMON/LM17/ NPARX,RESYN
      INTEGER*4    NPARX
      CHARACTER*4        RESYN
C     ------------------------------------------------------------------
      COMMON/LM29/ FMTI,FMTO
      CHARACTER*4  FMTI,FMTO
C     ------------------------------------------------------------------
      INTEGER*4    EVBUF(*)
C
      INTEGER*4    ILO,IHI,NWDS,OOF,OEND,PARMID,LASID,I,J
C
      INTEGER*4    X8000
      DATA         X8000/Z'8000'/
C
      SAVE
C
C     ==================================================================
C     OUPTR(J)  - CONTAINS OUTPUT BUFFER POINTER   FOR STREAM-J
C               - INITIALIZED TO 0 BY MAIN PROG & PUTPUT
C
C     OOFSET(J) - CONTAINS CURRENT OFFSET IN OUBUF FOR STREAM-J
C               =    -1  FOR UNDEFINED
C               =     0  FOR BUFFER-1 OF STREAM-1
C               = 16384  FOR BUFFER-2 OF STREAM-1
C               = 32768  FOR BUFFER-1 OF STREAM-2
C               = 49152  FOR BUFFER-2 OF STREAM-2
C               = 65536  FOR BUFFER-1 OF STREAM-3
C               = 81920  FOR BUFFER-2 OF STREAM-3
C
C     OUSIZ(J)  = OUTPUT BUF SIZE (USUALLY 4096)   FOR STREAM-J
C
C     OUBUF     = CONTAINS OUTPUT BUFFERS (1&2) FOR STREAMS (1,2&3) 
C
C     RESYN     = 'YES ' REQUESTS RESYNC ON NEXT EVENT 
C     RESYN     = 'NO  ' SAYS CONTINUE PROCESSING WITHOUT RESYNC
C     ==================================================================
C
      J=1                                  !SET STREAM #1
      OOF=OOFSET(J)                        !OFFSET IN OUBUF
      IF(OOF.LT.0) STOP 'NO OUT BUF'       !TST FOR UN-SET
      OEND=OOF+OUSIZ(J)                    !END-OF-BUFFER  
      OOF =OOF+OUPTR(J)                    !CURRENT POINTER VALUE
C
C     ------------------------------------------------------------------
C
      NWDS=2*(IHI-ILO+1)+2                 !MAX# words in this event
C
      IF(FMTO.EQ.'L001') NWDS=IHI-ILO+1    !MAX# for format L001
C
      IF((OEND-OOF).LT.NWDS) THEN          !Tst for enough space
      DO 20 I=OOF,OEND                     !If not,
      OUBUF(I)=-1                          !Zot rest of buffer
   20 CONTINUE
      OOFSET(J)=-1                         !Un-set output buffer
C
      CALL PUTPUT(J)                       !PUTPUT WILL SET 
C                                          !OUPTR(J) = 0
C                                          !OUSIZ(J) = BUF SIZE
C                                          !OOFSET(J)= OUBUF OFFSET 
C
      OOF=OOFSET(J)                        !HAS PVPINI BEEN DONE?
      IF(OOF.LT.0) STOP 'NO OUT BUF'       !TST FOR UN-SET
      OEND=OOF+OUSIZ(J)                    !OUPTR(J) = 0 NOW
C
      ENDIF
C
C     ------------------------------------------------------------------
C     Output in L003 format
C     ------------------------------------------------------------------
C
      IF(FMTO.EQ.'L002') GO TO 200
      IF(FMTO.EQ.'L001') GO TO 300
C
      DO 100 I=ILO,IHI                     !Loop on saved parms
      IF(EVBUF(I).LT.0) GO TO 100          !Skip non-existant
      PARMID=I+X8000                       !Parm-ID
      OUBUF(OOF)=IAND(PARMID,Z'FFFF')      !Load output with ID
      OOF=OOF+1                            !Inc  output pointer
      OUBUF(OOF)=EVBUF(I)                  !Load output with DATA
      OOF=OOF+1                            !Inc  output pointer
  100 CONTINUE
      OUBUF(OOF)=-1                        !Add the mighty FFFF
      OOF=OOF+1                            !Inc  output pointer
      OUBUF(OOF)=-1                        !Add the mighty FFFF
      OOF=OOF+1                            !Inc  output pointer
C
      RESYN='NO  '                         !REQUEST NO RESYNC
      OUPTR(J)=OOF-OOFSET(J)               !SAVE OUT-PNTR FOR LATER
C
      RETURN
C
C     ------------------------------------------------------------------
C     Output in L002 format
C     ------------------------------------------------------------------
C
  200 LASID=-1                             !Init last-ID output
C
      DO 220 I=ILO,IHI                     !Loop on saved parms
      IF(EVBUF(I).LT.0) GO TO 220          !Skip non-existant
C
      IF(I.NE.LASID+1) THEN                !Tst for non-contig IDs
      PARMID=I+X8000                       !Parm-ID
      OUBUF(OOF)=IAND(PARMID,Z'FFFF')      !Load output with ID
      OOF=OOF+1                            !Inc  output pointer
      ENDIF
C
      LASID=I                              !Save last ID
      OUBUF(OOF)=EVBUF(I)                  !Load output with DATA
      OOF=OOF+1                            !Inc  output pointer
  220 CONTINUE
      OUBUF(OOF)=-1                        !Add the mighty FFFF
      OOF=OOF+1                            !Inc  output pointer
C
      RESYN='NO  '                         !REQUEST NO RESYNC
      OUPTR(J)=OOF-OOFSET(J)               !SAVE OUT-PNTR FOR LATER
C
      RETURN
C
C     ------------------------------------------------------------------
C     Output in L001 format
C     ------------------------------------------------------------------
C
  300 DO 320 I=ILO,IHI                     !Loop on saved parms
      OUBUF(OOF)=EVBUF(I)                  !Load output with DATA
      OOF=OOF+1                            !Inc  output pointer
  320 CONTINUE
C
      RESYN='NO  '                         !REQUEST NO RESYNC
      OUPTR(J)=OOF-OOFSET(J)               !SAVE OUT-PNTR FOR LATER
C
      RETURN
C
      END
