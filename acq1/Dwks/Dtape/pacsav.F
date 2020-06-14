C$PROG PACSAV
*
*   Revisions:
*
*    12/18/96   MCSQ   Fixed PAC file dump were PAC file is greater
*                      than 20 line.
*
*
      SUBROUTINE PACSAV(IERR)
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
C
      COMMON/LM13/ NUMINT,NUMOUT(3),NOUT(3),MTIN(6),MTOUT(6,3),NOSTR
      integer*4    numint,numout,nout,mtin,mtout,nostr

      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      integer*4    iwd,lwd,ityp,nf,nter
C
      INTEGER*4 HEDF(64),TITL(20),HEDN,mon,day,year
      character*16 datetime,str
      character*80 ciwd,ctitl,fbuf(20)
      character*112 cmssg
#ifndef  __ultrix
      integer*8 offset
#endif
*
      EQUIVALENCE (iwd,ciwd)
      equivalence (titl,ctitl),(hedf(9),datetime),(ibuf,fbuf)
      equivalence (mssg,cmssg)
*
      data ctitl /'PAC file: '/
C
      DATA HEDN/999001/
C
      DATA (HEDF(I),I=1,12)/'HHIR','F   ','L003','    ',
     &                      'LIST',' DAT','A   ','    ',
     &                      '    ','    ','    ','    '/
      DATA (HEDF(I),I=33,64)/32*0/
C
      SAVE TITL,HEDN,HEDF
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
     1CARRIAGECONTROL = 'LIST',
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
101   format (a)
      i = i + 1
      go to 100
C
  110 hedf(38) = i/20
      if (hedf(38)*20 .ne. i) hedf(38) = hedf(38) + 1
      rewind(ltx)
  300 DO 310 I=1,20
      HEDF(I+12)=TITL(I)
      call idate(mon,day,year)
      write(datetime,301)mon,day,year
301   format(i2.2,'/',i2.2,'/',i2.2)
      call time(str)
      datetime(10:14) = str(1:5)
  310 CONTINUE
      HEDF(33)=HEDN
      HEDF(41)=1600
C
      ENCODE(112,315,MSSG)(TITL(I),I=1,15),HEDN
  315 FORMAT(15A4,' - ',I12)
      CALL MESSLOG(LOGUT,LOGUP)
C
#ifdef  __ultrix
      CALL MT_WRITE(LUCO(1),HEDF,256,NBWT,IERR)
      if (ierr .ne. 0) then
         CALL IOERR(IERR,ISTAT)
         return
      endif
#else
      CALL AIOWRITE(LUCO(1),HEDF,256,OFFSET,IERR)
      CALL AIOERLOG('WRIT',IERR)
      CALL AIOWRWAIT(LUCO(1),HEDF,NBWT,IERR)
      CALL AIOERLOG('WWAI',IERR)
      if (ierr .ne. 0) then
         CALL IOERR(IERR,ISTAT)
         return
      endif
#endif
      HEDN=HEDN+1
C
400   i = 1
410   read(ltx,101,end=420) fbuf(i)
      i = i + 1
      if (i .le. 20) go to 410
#ifdef  __ultrix
      call mt_write(luco(1),ibuf,1600,nbwt,ierr)
      if (ierr .ne. 0) then
         CALL IOERR(IERR,ISTAT)
         return
      endif
#else
      CALL AIOWRITE(LUCO(1),ibuf,1600,OFFSET,IERR)
      CALL AIOERLOG('WRIT',IERR)
      CALL AIOWRWAIT(LUCO(1),ibuf,NBWT,IERR)
      CALL AIOERLOG('WWAI',IERR)
      if (ierr .ne. 0) then
         CALL IOERR(IERR,ISTAT)
         return
      endif
#endif
      go to 400
C
420   fbuf(i) = ' '
      i = i + 1
      if (i .le. 20) go to 420
#ifdef  __ultrix
      call mt_write(luco(1),ibuf,1600,nbwt,ierr)
#else
      CALL AIOWRITE(LUCO(1),ibuf,1600,OFFSET,IERR)
      CALL AIOERLOG('WRIT',IERR)
      CALL AIOWRWAIT(LUCO(1),ibuf,NBWT,IERR)
      CALL AIOERLOG('WWAI',IERR)
#endif
      CALL IOERR(IERR,ISTAT)
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
