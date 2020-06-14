C$PROG PACSAVF
*
*   Revisions:
*
*    12/18/96   MCSQ   Fixed PAC file dump were PAC file is greater
*                      than 20 line.
*
*
      SUBROUTINE PACSAVF(IERR)
C
      implicit none
C
      integer*4  ierr
C
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
      integer*4    lin,kfi,lino,lou,kfo,louo,ltx,lml,lmlo,
     1             kmds,luci,luco
C
      COMMON/LM05/ IBUF(16384)
      integer*4    ibuf
C
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      integer*4    npar,lstl,lnby,maxip,nskip,iswab,iswah,lform

      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C
      COMMON/LM13/ NUMINT,NUMOUT(3),NOUT(3),MTIN(6),MTOUT(6,3),NOSTR
      integer*4    numint,numout,nout,mtin,mtout,nostr

      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF,INFOP,OUFOP
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      integer*4    iwd,lwd,ityp,nf,nter
C
      INTEGER*4 HEDF(64),TITL(20),HEDN,mon,day,year
      character*16 datetime,str
      character*80 ciwd,ctitl,fbuf(20)
*
      integer*4  i,ia,ib,nxnb,lsnb,istat
*
      EQUIVALENCE (iwd,ciwd)
      equivalence (titl,ctitl),(hedf(9),datetime),(ibuf,fbuf)
*
      data ctitl /'PAC file: '/
C
      DATA HEDN/999001/
C
      character*4 cHEDF(64)
      equivalence (cHEDF, HEDF)
      DATA (cHEDF(I),I=1,12)/'HHIR','F   ','L003','    ',
     &                      'LIST',' DAT','A   ','    ',
     &                      '    ','    ','    ','    '/
      DATA (HEDF(I),I=33,64)/32*0/
C
      IERR=0
C
      CLOSE(UNIT=LTX)
      IA=NXNB(IWD,5,80)
      IF(IA.LE.0) GO TO 600
      IB=LSNB(IWD,IA,80)
      IF(IB.LE.0) GO TO 600
C
      OPEN(UNIT       = LTX,
     1     FILE       = CIWD(IA:IB),
     1     STATUS     = 'OLD',
     1     ACCESS     = 'SEQUENTIAL',
     1     FORM       = 'FORMATTED',
     1     IOSTAT     = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL IOFERR(ISTAT)
                     GO TO 610
                     ENDIF
C
      ctitl(11:) = ciwd(ia:ib)
C
      i = 0
  100 read(ltx,101,end=110) fbuf(1)
101   format (80a)
      i = i + 1
      go to 100
C
  110 hedf(38) = i/20
      if (hedf(38)*20 .ne. i) hedf(38) = hedf(38) + 1
      rewind(ltx)
  300 DO 310 I=1,20
      HEDF(I+12)=TITL(I)
  310 CONTINUE
***      call idate(mon,day,year)
***      write(datetime,301)mon,day,year
***301   format(i2.2,'/',i2.2,'/',i2.2)
***      call time(str)
***      datetime(10:14) = str(1:5)
      HEDF(33)=HEDN
      HEDF(41)=1600
C
      WRITE(CMSSG,315)(TITL(I),I=1,15),HEDN
  315 FORMAT(15A4,' - ',I12)
      CALL MESSLOG(LOGUT,LOGUP)
C
      call ldfwrit(LUOUF,'HEAD',256,hedf,ierr)
      if (ierr .ne. 0) return
      HEDN=HEDN+1
C
400   i = 1
410   read(ltx,101,end=420) fbuf(i)
      i = i + 1
      if (i .le. 20) go to 410
      call ldfwrit(LUOUF,'PAC ',1600,ibuf,ierr)
      if (ierr .ne. 0) return
      go to 400
C
420   fbuf(i) = ' '
      i = i + 1
      if (i .le. 20) go to 420
      call ldfwrit(LUOUF,'PAC ',1600,ibuf,ierr)
      if (ierr .ne. 0) return
      close(ltx)
      RETURN
C
600   write(cmssg,*) 'SYNTAX ERROR IN FILENAME SPECIFICATION'
      go to 700
C
610   write(cmssg,*) 'ERROR OPENING PAC-FILE'
      go to 700
C
  700 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
