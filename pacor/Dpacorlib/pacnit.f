C$PROG PACNIT
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 01/02/2003
C     ************************************************************
C
      SUBROUTINE PACNIT(KLIS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      COMMON/III/  LIN,LCM,LCI
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PACM/ NAMPAC(20)
C
      CHARACTER*80 CARG(2),NAMFIL,NAMLOG,NAMTMP,NAMTAB,NAMPOB
C
      CHARACTER*80 CNAMINQ,FFULL
C
      INTEGER*4    NARG(20,2)
C
      EQUIVALENCE (NARG,CARG),(CNAMINQ,NAMPAC)
C
      character*4 cLISTYP
      equivalence (cLISTYP, LISTYP)
      DATA ISORL,IEXPL,cLISTYP,NERR/0,0,'ALL ',0/
C
      DATA LCI,LIN,LCM/1,1,1/
C
      INTEGER*4  RECLVALU
C
      character*4 cNAMPROG(2)
      equivalence (cNAMPROG, NAMPROG)
      DATA       cNAMPROG/'PACO','R   '/
C
      SAVE
C
C     *************************************************************
C     INITIALIZE SOME STUFF AND OPEN FILES
C     *************************************************************
C
C
      CMSSG=' '
      LOGUT=6
      LOGUP=7
      LISFLG='LON '
      MSGF='    '
C
      NUMARG=IARGC()
      CARG(1)=' '
      CARG(2)=' '
      IF(NUMARG.GT.2) GO TO 100
      CALL GETARG(1,CARG(1))
      IF(NUMARG.EQ.2) CALL GETARG(2,CARG(2))
C
      KLIS=NARG(1,2)
      CALL KASUP4(KLIS)
C
      IA=1
      IB=NXBL(NARG(1,1),IA,79)-1
      NAMFIL=CARG(1)
      NAMFIL(IB+1:)='.pac'
C
      CALL STRIPATH(NARG(1,1),80)
      IB=NXBL(NARG(1,1),IA,79)-1
C
      NAMLOG=CARG(1)
      NAMTMP=CARG(1)
      NAMTAB=CARG(1)
      NAMPOB=CARG(1)
      NAMLOG(IB+1:)='.lst'
      NAMTMP(IB+1:)='.tmp'
      NAMTAB(IB+1:)='.tab'
      NAMPOB(IB+1:)='.pob'
C
      OPEN(UNIT            = 1,
     &     STATUS          = 'OLD',
     &     ACCESS          = 'SEQUENTIAL',
     &     FILE            = NAMFIL,
     &     ERR             = 200)
C
      INQUIRE(UNIT=1,NAME=CNAMINQ)                !GET SOURCE FILENAME
C
      IF(CNAMINQ(1:1).NE.'/') THEN
      CALL GETCWD(FFULL)
      CALL STRAPPEND(FFULL,'/')
      CALL STRAPPEND(FFULL,CNAMINQ)
      CNAMINQ=FFULL
      ENDIF
C
CX    WRITE(6,777)CNAMINQ
CX777 FORMAT(1H ,A)
C
c     OPEN(UNIT      = 7,                         !OPEN/DELETE LOG-FILE
c    &     FILE      = NAMLOG,
c    &     STATUS    = 'UNKNOWN',
c    &     IOSTAT    = IOS)
C
c     OPEN(UNIT      = 8,                         !OPEN/DELETE TMP-FILE
c    &     FILE      = NAMTMP,
c    &     STATUS    = 'UNKNOWN',
c    &     IOSTAT    = IOS)
C
c     OPEN(UNIT      = 9,                         !OPEN/DELETE TAB-FILE
c    &     FILE      = NAMTAB,
c    &     STATUS    = 'UNKNOWN',
c    &     IOSTAT    = IOS)
C
c     OPEN(UNIT      = 10,                        !OPEN/DELETE OUT-FILE
c    &     FILE      = NAMPOB,
c    &     STATUS    = 'UNKNOWN',
c    &     IOSTAT    = IOS)
C
c     CLOSE(UNIT=7, DISP='DELETE')
      call unlink(NAMLOG);
c     CLOSE(UNIT=8, DISP='DELETE')
      call unlink(NAMTMP);
c     CLOSE(UNIT=9, DISP='DELETE')
      call unlink(NAMTAB);
c     CLOSE(UNIT=10,DISP='DELETE')
      call unlink(NAMPOB);
C
      OPEN(UNIT      = 7,                         !CREATE NEW LOG-FILE
     &     FILE      = NAMLOG,
     &     STATUS    = 'NEW',
     &     IOSTAT    = IOS)
C
      OPEN(UNIT      = 8,                         !CREATE NEW TMP-FILE
     &     FILE      = NAMTMP,
     &     STATUS    = 'NEW',
     &     IOSTAT    = IOS)
C
      OPEN(UNIT      = 9,                         !CREATE NEW TAB-FILE
     &     FILE      = NAMTAB,
     &     STATUS    = 'NEW',
     &     IOSTAT    = IOS)
C
      OPEN(UNIT       = 10,                        !CREATE NEW OUT-FILE
     &     FILE       = NAMPOB,
     &     STATUS     = 'NEW',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(262144),
     &     FORM       = 'UNFORMATTED',
     &     IOSTAT     = IOS)
C
      RETURN
C
  100 WRITE(6,105)
  105 FORMAT(1H ,'ERROR IN FILE SPECIFICATION')
      STOP 1
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'ERROR OPENING PAC-FILE')
      STOP 1
      END
