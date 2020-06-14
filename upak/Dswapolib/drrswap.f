C$PROG DRRSWAP   - Routine to byte-swap drr-file & replace on disk
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE DRRSWAP(KMD)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/AAA/ ILO(8500),IHI(8500),NHW(8500),NHIS,LENT
C     ------------------------------------------------------------------
      COMMON/BBB/ LUS,LUD,LUH,LUF
C     ------------------------------------------------------------------
      INTEGER*4    IOF(8500),NCH(8500),DIRF(32),STAT
C
      INTEGER*2    DIRH(64),TMPH(2)
C
      CHARACTER*4  BYORD,KMD
C
      INTEGER*4    TMPF,TMXF
C   
      EQUIVALENCE (TMPH,TMXF)
      EQUIVALENCE (IOF(1),ILO(1)),(NCH(1),IHI(1)),(DIRF(1),DIRH(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IREC=1
      CALL DREAD(LUD,DIRF,IREC,128,STAT)    !READ 1ST DRR-RECORD
      IF(STAT.NE.0) RETURN
C   
      NHIS=DIRF(4)                          !SAVE # OF HISTOGRAMS
      LENT=DIRF(5)                          !TOTAL FILE-LENGTH 1/2WDS
C
      BYORD='GOOD'                          !Init byte-order flag
      IF(NHIS.GT.8000) BYORD='BAD '         !Test compatibility
      IF(NHIS.LT.0)    BYORD='BAD '         !Test compatibility
C
      IF(BYORD.EQ.'BAD ') THEN
      CALL SWAPPER(2,NHIS)
      CALL SWAPPER(2,LENT)
      ENDIF
C
      IF(KMD.EQ.'SWAP') GO TO 100           !Is it TEST or SWAP request
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
  100 CALL SWAPB(DIRH,7,24)                 !SWAP BYTES
      CALL SWAPH(DIRH,7,24)                 !SWAP HALF-WDS
C   
      CALL DRITE(LUD,DIRF,IREC,128,STAT)    !Write to disk
      IF(STAT.NE.0) RETURN
C   
      DO 120 N=1,NHIS                       !LOOP ON # HISTOGRAMS
      IREC=IREC+1
      CALL DREAD(LUD,DIRF,IREC,128,STAT)    !READ DRR-ENTRY
C
CX    IF(STAT.NE.0) RETURN
C   
      TMPF   =DIRF(12)                      !Save for byte-swapping
      TMPH(1)=DIRH(1)
      TMPH(2)=DIRH(2)
C
      IF(BYORD.NE.'GOOD') THEN
      CALL SWAPPER(2,TMPF)
      CALL SWAPPER(1,TMXF)
      ENDIF
C
      IOF(N)=TMPF                           !SAVE DISK HALF-WD OFFSET
      NHW(N)=TMPH(2)                        !SAVE HALF-WDS/CHANNEL
      NDIM  =TMPH(1)                        !SAVE HIS-DIMENSIONALITY
C   
      NCP   =1                              !INIT #-CHANS PRODUCT
      DO 110 I=1,NDIM                       !LOOP ON #-DIMENSIONS
      TMPH(1)=DIRH(I+18)
      TMPH(2)=DIRH(I+14)
      IF(BYORD.NE.'GOOD') CALL SWAPPER(1,TMXF)
      LD=TMPH(1)-TMPH(2)+1                  !#CHANS FOR THIS DIMENSION
      NCP=NCP*LD                            !ACCUMULATE PRODUCT
  110 CONTINUE
C   
      NCH(N)=NCP                            !# CHANNELS FOR THIS HIS
C   
      CALL SWAPB(DIRH, 1,24)                !SWAP BYTES
      CALL SWAPH(DIRH,23,24)                !SWAP HALF-WDS
C   
      CALL DRITE(LUD,DIRF,IREC,128,STAT)
C
CX    IF(STAT.NE.0)  RETURN
C
  120 CONTINUE
C   
      NDO=(NHIS+31)/32                      !NUMBER OF ID# RECORDS
C   
      DO 140 I=1,NDO                        !LOOP ON # OF RECORDS
      IREC=IREC+1
      CALL DREAD(LUD,DIRF,IREC,128,STAT)    !READ IN ID-RECORD
C
CX    IF(STAT.NE.0) RETURN
C   
      CALL SWAPB(DIRH,1,64)                 !SWAP BYTES
      CALL SWAPH(DIRH,1,64)                 !SWAP HALF-WDS
C   
      CALL DRITE(LUD,DIRF,IREC,128,STAT)
C
CX    IF(STAT.NE.0) RETURN
C   
  140 CONTINUE
C   
C     *************************************************************
C     ACCUMULATE HISTOGRAM HALF-WD LIMITS
C     *************************************************************
C   
      DO 150 I=1,NHIS                       !LOOP ON # HISTOGRAMS
      ILO(I)=IOF(I)+1                       !FIRST HALF-WD NUMBER
      IHI(I)=ILO(I)+NHW(I)*NCH(I)-1         !LAST  HALF-WD NUMBER
  150 CONTINUE
      RETURN
      END
