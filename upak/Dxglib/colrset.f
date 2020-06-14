C$PROG COLRSET   - Sets up color mapping
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE COLRSET(LU,IWD,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/XLJJ/ IRED(40),IGRE(40),IBLU(40),KOLRSET
      INTEGER*4    IRED,    IGRE,    IBLU
      CHARACTER*4                             KOLRSET
C     ------------------------------------------------------------------
      INTEGER*4 LU,IERR,I,J,N,NDO,ISTAT,NF,KIND,NL,NTER,KV
C
      REAL*4    XV
C
      INTEGER*4 KOLR(120),NAMF(20),IV(3)
C
      INTEGER*4 JWD(20),IWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4 LST(10)
C
      INTEGER*4    GRAS,INIT,BLANK
      CHARACTER*4  CGRAS, CINIT, CBLANK
      EQUIVALENCE  (CGRAS,GRAS), (CINIT,INIT), (BLANK,CBLANK)
C
      DATA         CGRAS,CINIT,CBLANK/'GRAS','INIT','    '/
C
      CHARACTER*80 CNAMF
C
      EQUIVALENCE (CNAMF,NAMF)
C
      SAVE
C     ------------------------------------------------------------------
C
      DATA (KOLR(I),I= 1,30)/
     &     0,     0,     0, !01 BLACK
     & 65535, 65535, 65535, !02 1-D COL1 - FIT DATA 
     & 65535,     0,     0, !03 1-D COL2 
     &     0, 65535,     0, !04 1-D COL3 - FIT CALC
     &     0,     0, 65535, !05 1-D COL4 
     & 65535, 65535,     0, !06 1-D COL5 - FIT BACK
     & 65535,     0, 65535, !07 1-D COL6 
     &     0, 65535, 65535, !08 1-D COL7
     & 65535, 65535,     0, !09 1-D COL8
     & 65535, 65535,     0/ !10 NOT USED   
C
      DATA (KOLR(I),I=31,60)/
     &     0,     0, 32767, !11 2-D COLOR
     &     0,     0, 65535, !12 2-D COLOR
     & 32767,     0, 32767, !13 2-D COLOR
     & 65535,     0, 65535, !14 2-D COLOR
     & 32767,     0,     0, !15 2-D COLOR
     & 65535,     0,     0, !16 2-D COLOR
     & 32767, 32767,     0, !17 2-D COLOR
     & 65535, 65535,     0, !18 2-D COLOR
     & 32767, 32767, 32757, !19 2-D COLOR
     & 65535, 65535, 65535/ !20 2-D COLOR
C
      DATA (KOLR(I),I=61,90)/
     & 15000, 15000, 15000, !21 2-D GRAY
     & 20600, 20600, 20600, !22 2-D GRAY
     & 26200, 26200, 26200, !23 2-D GRAY
     & 31800, 31800, 31800, !24 2-D GRAY
     & 37400, 37400, 37400, !25 2-D GRAY
     & 43000, 43000, 43000, !26 2-D GRAY
     & 48600, 48600, 48600, !27 2-D GRAY
     & 54200, 54200, 54200, !28 2-D GRAY
     & 59800, 59800, 59800, !29 2-D GRAY
     & 65535, 65535, 65535/ !30 2-D GRAY
C
      DATA (KOLR(I),I=91,120)/
     & 32767, 32767, 32767,!31 NOT USED
     & 32767, 65535, 65535, !32 NOT USED
     & 65535, 65535, 65535, !33 CUSTOM WIND CURSOR
     & 65535, 65535, 65535, !34 GCOR1
     &     0, 65535, 65535, !35 GCOR2 SAM PK, FIT VAR
     & 65535,     0, 65535, !36 GCOR3 1D PK, BAN, EXP
     &     0, 65535, 65535, !37 GCOR4 1D REG-MK, FIT VAR 
     &     0,     0, 65535, !38 GCOR5 FULL SCR CURSOR
     & 65535,     0, 65535, !39 GCOR6
     &     0, 65535, 65535/ !40 GCOR7
C
C     ************************************************************
C     SETS UP COLOR SPECIFICATION ARRAYS - IRED,IGRE,IBLU
C     FROM THE PRE-DEFINED ARRAY KOLR OR FROM A DISK FILE
C     ************************************************************
C
      IERR=0
C
      IF(IWD(1).EQ.GRAS)  GO TO 300
      IF(IWD(1).EQ.INIT)  GO TO 100
      IF(IWD(2).EQ.BLANK) GO TO 100
                          GO TO 200
C
C     ************************************************************
C     LOAD COLOR ARRAY SPECIFICATIONS FROM DEFAULT TABLE
C     ************************************************************
C
  100 I=0
      DO 110 J=1,40
      I=I+1
      IRED(J)=KOLR(I)
      I=I+1
      IGRE(J)=KOLR(I)
      I=I+1
      IBLU(J)=KOLR(I)
  110 CONTINUE
      KOLRSET='YES '
      RETURN
C
C     ************************************************************
C     OPEN COLOR-MAP DATA FILE AND RE-DEFINE COLOR ARRAYS
C     ************************************************************
C
  200 CLOSE(UNIT=LU)
C
      CALL FINAME(IWD,5,80,NAMF,IERR)
      IF(IERR.NE.0) GO TO 540
C
      OPEN(UNIT    = LU,
     &     FILE    = CNAMF,
     &     STATUS  = 'OLD',
     &     IOSTAT  = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL IOFERR(ISTAT)
                     IERR=1
                     GO TO 250
                     ENDIF
C
      REWIND LU
C
      NL=0
  220 READ(LU,230,END=250,ERR=550)JWD
  230 FORMAT(20A4)
      CALL GREAD(JWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 560
C
      DO 240 I=1,3
      CALL MILV(LWD(1,I),IV(I),XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 560
  240 CONTINUE
      NL=NL+1
      IF(NL.GT.40) GO TO 250
      IRED(NL)=IV(1)
      IGRE(NL)=IV(2)
      IBLU(NL)=IV(3)
      GO TO 220
C
  250 CLOSE(UNIT=LU)
      KOLRSET='YES '
      RETURN
C
  300 CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      NDO=NF
      IF(NDO.GT.10) NDO=10
      DO 310 I=1,NDO
      CALL MILV(LWD(1,I),KV,XV,KIND,IERR)
      IF(XV.LT.0.0)   XV=0.0
      IF(XV.GT.100.0) XV=100.0
      LST(I)=655.0*XV
  310 CONTINUE
C
      N=20
      DO 320 I=1,NDO
      N=N+1
      IRED(N)=LST(I)
      IGRE(N)=LST(I)
      IBLU(N)=LST(I)
  320 CONTINUE
      RETURN
C
  540 WRITE(CMSSG,545)
  545 FORMAT('SYNTAX ERROR IN COLOR-MAP FILENAME - CMD IGNORED')
      GO TO 700
C
  550 WRITE(CMSSG,555)
  555 FORMAT('ERROR READING COLOR-MAP FILE - CMD IGNORED')
      GO TO 700
C
  560 WRITE(CMSSG,565)
  565 FORMAT('SYNTAX ERROR IN COLOR-MAP FILE - CMD IGNORED')
      GO TO 700
C
  700 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      KOLRSET='NO  '
      RETURN
      END
