C$PROG REFOR
      SUBROUTINE REFOR(IWD,LWD,ITYP,NF,IMIN,IMAX,NTER)
C
      DIMENSION JWD(80),IWD(1),LWD(1),ITYP(1)
C
      DATA JNUM,JALP,JDEL,MAXB,LF/1,2,3,320,8/
C
      INTEGER*4   BLANK,X0D,X3B
      character*4 cblank
      equivalence (cblank,blank)
      DATA        cBLANK/'    '/
      DATA        X0D  /z'0D'/
      DATA        X3B  /z'3B'/
C
      SAVE
C
      NTER=0                                !ZERO # TRUNCATION ERRS
      NF=0                                  !ZERO # FIELDS
C
C     **************************************************************
C     UNPACK BYTES INTO FULL WORDS
C     **************************************************************
C
      N=0                                   !ZERO BYTE COUNTER
      DO 20 I=IMIN,IMAX                     !LOOP ON SPECIFIED RANGE
      CALL ILBYTE(JTEMP,IWD,I-1)
      IF(JTEMP.EQ.X0D) GO TO 25             !TST FOR CARRAGE RETURN
      IF(JTEMP.EQ.X3B) GO TO 25             !TST FOR "!" - COMMENT
      N=N+1                                 !INC BYTE CNTR
      JWD(N)=JTEMP
   20 CONTINUE
C
   25 NS=0                                  !ZERO # SECTIONS (FIELDS)
      JLO=1                                 !SET SCAN LO-LIMIT
      JHI=N                                 !SET SCAN HI-LIMIT
      KHI=0                                 !CHAP PNTR IN LWD
   30 NS=NS+1                               !INC # FIELD CNTR
      LWD(2*NS-1)=BLANK                     !BLANK NEXT FIELD
      LWD(2*NS)  =BLANK                     !BLANK NEXT FIELD
      KLO=KHI+1                             !LWD PNTR LO-LIMIT
      KHI=KLO+7                             !LWD PNTR HI-LIMIT
C
C     **************************************************************
C     STARTING AT JLO, FIND THE FIRST NON-DELIMITER
C     **************************************************************
C
      IF(JLO.GT.JHI) RETURN                 !TST FOR DONE
      DO 40 I=JLO,JHI                       !FIND NON-DELIMITER
      IF(KINDA(JWD(I)).NE.JDEL) GO TO 45    !TST FOR DELIMITER
   40 CONTINUE
      RETURN                                !DONE IF NOT FOUND
C
   45 JLO=I                                 !ADJUST SCAN PNTR
C
      IF(KINDA(JWD(I)).EQ.JNUM) GO TO 100   !TST 1ST CHAR FOR NUMERIC
C
C     **************************************************************
C     LEFT JUSTIFY FIELDS STARTING WITH ALPHABETIC CHARACTER
C     **************************************************************
C
      K=KLO                                 !LWD CHAR PNTR
      NF=NS                                 !FIELD CNTR
      ITYP(NF)=1                            !SET ALPHA FIELD TYPE
      DO 50 I=JLO,JHI                       !LOOP UNTIL NXT DELIMITER
      IF(KINDA(JWD(I)).EQ.JDEL) GO TO 55    !TST FOR DELIMITER
      IF(K.LE.KHI) GO TO 48                 !TST FOR FIELD OVERFLO
      NTER=NTER+1                           !IF YES, INC TRUNK CNTR
      GO TO 50                              !AND SKIP IT
   48 CALL ISBYTE(JWD(I),LWD,K-1)           !OTHERWISE, STORE BYTE
      K=K+1                                 !INC BYTE CNTR
   50 CONTINUE
      RETURN                                !DONE IN NO DEL FOUND
C
   55 JLO=I+1                               !OTHERWISE, ADJUST SCAN PNTR
      GO TO 30                              !AND GO BACK FOR MORE
C
C     **************************************************************
C     RIGHT JUSTIFY FIELDS STARTING WITH NUMERIC CHARACTER
C     **************************************************************
C
  100 DO 110 I=JLO,JHI                      !FIND NEXT DELIMITER
      IF(KINDA(JWD(I)).EQ.JDEL) GO TO 120   !TST FOR DELIMITER
  110 CONTINUE
      I=JHI+1                               !SET UP TO RUN BACKWARDS
  120 JUP=I-1
      NDO=JUP-JLO+1                         !GET # BYTES
      IF(NDO.LT.1) GO TO 30                 !TST FOR NULL??
      IF(NDO.LE.8) GO TO 125                !TST FOR FIELD OVERFLO
      NTER=NTER+(NDO-8)                     !IF YES, SET ERROR
      NDO=8                                 !AND LIMIT LOOP TO 8
  125 K=KHI                                 !START AT HI FIELD-BYTE
      J=JUP                                 !START AT HI INPUT-BYTE
      NF=NS                                 !# OF FIELDS
      ITYP(NF)=2                            !SET NUMERIC FIELD TYPE
      DO 130 N=1,NDO                        !REVERSE LOOP ON BYTES
      CALL ISBYTE(JWD(J),LWD,K-1)           !STORE BYTE IN LWD
      J=J-1                                 !DEC INPUT CNTR
      K=K-1                                 !DEC OUTPUT CNTR
  130 CONTINUE
      JLO=JUP+2                             !ADJUST SCAN PNTR
      GO TO 30                              !GO BACK FOR MORE
C
      END
