C$PROG CONV      - Sets up byte-swap conversion mask for spk-files
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE CONV
C   
      IMPLICIT INTEGER*4 (A-Z)
C   
C     ------------------------------------------------------------------
      COMMON/SSS/ IDAT(512),HMSK(32),LOHED(256),ILO,IHI,NID
C     ------------------------------------------------------------------
      INTEGER*4 DMSK(512)
C   
      DATA IFIR/1/
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IDA=IFIR
C   
      DO 10 I=1,512
      DMSK(I)=2
   10 CONTINUE
C   
      DO 200 K=IDA,NID
C   
      IF(LOHED(K).GT.IHI) GO TO 300
      HIHED=LOHED(K)+31
C   
      IF(LOHED(K).GE.ILO.AND.LOHED(K).LE.IHI) GO TO 20
      IF(HIHED   .GE.ILO.AND.HIHED   .LE.IHI) GO TO 20
      GO TO 200
C   
   20 IFIR=K                            !SET NEXT HEADER INDEX TO TST
      IOF=LOHED(K)-ILO                  !GET HEADER OFFSET IN IDAT
C   
      IF(IOF.LT.0)   THEN               !TST FOR PART BELOW
                     FDW=1
                     LDW=32+IOF
                     FMW=-IOF+1
                     GO TO 100
                     ENDIF
C   
      IF(IOF.EQ.0)   THEN               !TST FOR START AT IDAT(1)
                     FDW=1
                     LDW=32
                     FMW=1
                     GO TO 100
                     ENDIF
C   
      IF(IOF.LT.480) THEN               !TST FOR ALL WITHIN
                     FDW=IOF+1
                     LDW=IOF+32
                     FMW=1
                     GO TO 100
                     ENDIF
C   
      IF(IOF.EQ.480) THEN               !TST FOR END AT IDAT(512)
                     FDW=481
                     LDW=512
                     FMW=1
                     GO TO 100
                     ENDIF
C   
      IF(IOF.GT.480) THEN               !TST FOR PART ABOVE
                     FDW=IOF+1
                     LDW=512
                     FMW=1
                     GO TO 100
                     ENDIF
C   
      GO TO 200
C   
  100 N=FMW
      DO 110 I=FDW,LDW
      DMSK(I)=0
      CALL SWAPPER(HMSK(N),IDAT(I))
      N=N+1
  110 CONTINUE
C   
  200 CONTINUE
C   
  300 DO 310 I=1,512
      CALL SWAPPER(DMSK(I),IDAT(I))
  310 CONTINUE
      RETURN
      END
