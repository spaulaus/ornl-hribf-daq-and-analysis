C$PROG SPKSWAP   - Byte-swaps spk-file & replaces on disk
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE SPKSWAP(KMD)
C   
      IMPLICIT INTEGER*4 (A-Z)
C   
C     ------------------------------------------------------------------
      COMMON/BBB/ LUS,LUD,LUH,LUF
C     ------------------------------------------------------------------
      COMMON/SSS/ IDAT(512),HMSK(32),LOHED(256),ILO,IHI,NID
C     ------------------------------------------------------------------
      CHARACTER*4 KMD,BYORD
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IREC=1
C   
      CALL DREAD(LUS,IDAT,IREC,2048,STAT)   !READ IN DIRECTORY
      IF(STAT.NE.0) RETURN
C   
      NID=IDAT(3)                           !# OF ID'S      ON FILE
      NHW=IDAT(4)-1                         !# OF HALF-WDS ON FILE
      IDAT4=IDAT(4)
C
      BYORD='GOOD'                          !Init byte-order flag
      IF(NID.GT.256) BYORD='BAD '           !Tst byte-order
      IF(NID.LT.0  ) BYORD='BAD '           !Tst byte-order
C
      IF(KMD.EQ.'SWAP') GO TO 100           !Is it a test or swap request
C
      IF(BYORD.EQ.'GOOD') WRITE(6,50)
      IF(BYORD.NE.'GOOD') WRITE(6,55)
   50 FORMAT(1H ,'Byte-order is compatible with this platform')
   55 FORMAT(1H ,'Byte-order is incompatible with this platform')
      RETURN
C
C     ******************************************************************
C     Do the byte-swap
C     ******************************************************************
C
  100 DO 110 I=1,22                         !SET HEADER BYTE-SWAP MASK
      HMSK(I)=2                             !FULL WORD CONVERT
  110 CONTINUE
      DO 115 I=2,4
      HMSK(I)=0                             !NO CONVERT (ASCII)
  115 CONTINUE
      DO 120 I=5,7
      HMSK(I)=1                             !HALF WORD CONVERT
  120 CONTINUE
      DO 125 I=23,32
      HMSK(I)=0                             !NO CONVERT (ASCII)
  125 CONTINUE
C
      IF(BYORD.EQ.'BAD ') THEN              !If bad,
      CALL SWAPPER(2,NID)                   !Byte-swap #IDs
      CALL SWAPPER(2,IDAT4)                 !Byte-swap #half-words
      NHW=IDAT4-1
                          ENDIF
C
      NREC=(NHW+1023)/1024                  !# OF RECORDS  ON FILE
C   
      N=6                                   !1ST ENTRY IN OFFSET LIST
      DO 150 I=1,NID                        !LOOP ON # ID'S
      LOC=IDAT(N)
      IF(BYORD.NE.'GOOD')CALL SWAPPER(2,LOC)!BYTE-SWAP IF NEEDED
      LOHED(I)=LOC/2+1                      !FULL-WD DISK-ADDR OF HEDR
      N=N+2                                 !INC DIRECTORY INDEX
  150 CONTINUE
C   
      DO 160 I=3,512                        !LOOP TO CONVERT DIRECTORY
      CALL SWAPPER(2,IDAT(I))               !CONVERT FULL-WORD
  160 CONTINUE
C
      CALL DRITE(LUS,IDAT,IREC,2048,STAT)   !WRITE DIRECTORY TO DISK
      IF(STAT.NE.0) GO TO 500
C   
      IHI=512                               !INIT NEXT WORD#
  200 ILO=IHI+1                             !NEXT DISK FULL-WORD
      IHI=ILO+511                           !LAST DISK FULL-WORD
C   
      IREC=IREC+1
      IF(IREC.GT.NREC) GO TO 500            !TST FOR DONE
C   
      CALL DREAD(LUS,IDAT,IREC,2048,STAT)   !READ IN DATA RECORD
      IF(STAT.NE.0) GO TO 500               !QUIT IF ERROR
C   
      CALL CONV                             !CONVERT (SWAP BYTES)
C   
      CALL DRITE(LUS,IDAT,IREC,2048,STAT)   !WRITE BACK TO FILE
      IF(STAT.NE.0) GO TO 500               !QUIT, IF ERROR
      GO TO 200                             !GO BACK FOR MORE
C   
  500 RETURN
      END
