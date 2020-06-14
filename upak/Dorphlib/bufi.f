C$PROG BUFI      - Disk-file input routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE BUFI(LU,IBUF,IREC,NBY,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      INTEGER*4 IBUF(1)
      integer*4 NB_GOT,NB_SUM
C
      SAVE
C
C     ------------------------------------------------------------------
C     xxxxxxxxxx CHANGES FOR UNIX xxxxxxxxxxx
C     Replaced qio call in VAX version with a call to a C routine
C     HIS_READ. Chose to specify offset for beginning of read in
C     bytes rather than 512 byte records. This results in some
C     trivial logic changes.
C     xxxxxxxxxx END UNIX NOTE xxxxxxxxxxxxxx
C     ------------------------------------------------------------------
C
      IERR=0
      NBYL=NBY
      LBYT=(IREC-1)*512
      NDX=1
      NB_SUM=0
C
   10 NB=NBYL
      IF(NB.GT.65024) NB=65024
C
      CALL HIS_READ(LU,IBUF(NDX),LBYT,NB,NB_GOT,IERR)
C
      IF(IERR.NE.0) THEN
              CALL IOSERR(IERR)
              WRITE(6,20)IERR
              IERR=1
              RETURN
      ENDIF
C
C
      NBYL=NBYL-NB
      IF(NBYL.LE.0) RETURN
      NDX=NDX+16256
      LBYT = LBYT + NB
      GO TO 10
C
  20  FORMAT(1H ,'BUFI ERROR - STAT =',I8)
      END
