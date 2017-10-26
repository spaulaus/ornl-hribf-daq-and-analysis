*****************************************************************************
*
*      Special version of LEMOR routine COPYTOF for data acquisition use
*
*   4/29/99
*****************************************************************************
*******************************************************************************
*
*  9/10/95   Cleanup, comment and add writing scalers to tape.
*            MCSQ.
*
*  8/11/96   Add commands to enable/disable automatic file marks
*
* 12/ 3/96   Add command TON which is like TRUN except that it
*            does not start VME acquisition.
*
*  5/11/03   Linux version.
*
* 10/20/03   RLV.  Add code to log event counters for the DOE
*                  bean counters.
*
* 10/20/17   RLV   Remove the DOE bean counter code
*******************************************************************************
      subroutine copyipc(kmd,nfnf,ierr)
*
      implicit none
*
      include  "acq_vme_ctrl.for"
*
      integer*4       nfnf,ierr
      character*4 kmd
      integer*4   mssg,namprog,logut,logup,lisflg
      character*4 msgf
      integer*4   luinf,luouf,infop,oufop
      integer     mlen
*
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
*
*     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
      character*4  ckmd, ckmx
      equivalence  (ckmd,lwd(1,1)), (ckmx, lwd(1,2))
*     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
      character*80 ciwdraw
      equivalence  (ciwdraw,iwdraw)
*
*     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
*
*     ------------------------------------------------------------------
      COMMON/LM26/ LDFONAM,LDFINAM
      CHARACTER*80 LDFONAM,LDFINAM
*     ------------------------------------------------------------------

      integer*4  dt(32)
      character*128 deadtime
      equivalence (dt,deadtime)

*     Beam status, read from TRUN BON or TRUN BOFF
*     Get status from the argument to TRUN
      integer beamstat
*
*       The protocol can lose data, so we count events and compare
*       with the total sent by the front end.
*
      real*8     strbufevt,numbufevts,buf_num,lastevt
      real*8     sumevts,beginevt
      real*8     evtslost
*
      common/orphas/ strbufevt,numbufevts,buf_num,lastevt,sumevts,
     &              beginevt
      common/autofile/ nextfile
      integer*4  nextfile
*
*       Scaler data stuff.
*
      integer*4 scatbuf,nscat,scatdmp,scatclr,scaterr,scnt
      common/scat0/ scatbuf(8000,2),nscat,scatdmp,scatclr,scaterr
*
*       Character string for some easier manipulations
*
      character*112 cmssg
      equivalence (cmssg, mssg(1))
*
      integer*4  nrtf,nrcop,nfcop,nfile
      data nrtf,nrcop,nfcop,nfile/4*0/
*
      integer*4  bytes,nbyt,jerr,stat,istop,ipass
      character*4 cstat
      character*80 cfile
      equivalence (stat,cstat)
      integer*4  vstat
      integer*4  j,jdx,evbuf(8192)
      integer*4  reclim /10000/
      integer*4  autofile /1/
      integer*4  maxrec /65500/
*
      INTEGER*4    FWRECL,NHED,NREC,NT,NMAR
*
      EQUIVALENCE (FWRECL,OUDIR(1)),          !Full-word block-size
     &            (NHED,  OUDIR(4)),          !Number of Header records
     &            (NREC,  OUDIR(2))           !Total number of records
*     ------------------------------------------------------------------
*     ****************************************************************
*     COPY N (FILES)  OR  CREC N (RECORDS) FROM INPUT TO OUTPUT-1
*     **************************************************************
*
      ierr=0
      call ldfook(ierr)       !Tst for ldf-output file write-ready
      if (ierr .ne. 0) return !If not, RETURN
*
*   Automatic file mark commands
*
      if (kmd .eq. 'AFON') then
        autofile = 1
        return
      endif
      if (kmd .eq. 'AFOF') then
        autofile = 0
        return
      endif
*
*    Check file size.  If too large just exit.
*
      if (nrec .ge. maxrec) then
        write(cmssg,*) 'LDF file already maximum size'
        go to 1200
      endif
      istop = 0
      ipass = 0
      sumevts = 0.
      beginevt = 0.
      lastevt = 0.
*
*     Get status of the VME acquisition system
*     Errors status is always negative

      call acq_vme_ctrl(vstat,"status")
      if (vstat .le. ETHERNET_ERROR) then
        write(cmssg,*) 'TAPE: VME Ethernet Connection Failure'
        go to 1200                !VME system error return
      endif
      if (vstat .eq. ACQ_UNINIT) then
        write(cmssg,*) 'VME Acquisition not initialized'
        go to 1200                !VME system error return
      endif
*
      if(kmd .eq. 'TCON') then
*
*  Backup one file mark
*
        call filmar(luouf,0,1,ierr)
        if (ierr .ne. 0) go to 1000       !File error return
        go to 495
      else if (kmd .eq. 'TRUN' .or. kmd .eq. 'TON ') then
*
*   Always start with a header for TRUN or TON commands
*
        call hedman('HOUT', ierr)
        if(ierr .ne. 0)      go to 1000       !File error return

*     Save the beam status, bon or boff, which came from the command line
        if (ckmx .eq. "BON ") then
           beamstat = 1
        else if (ckmx .eq. "BOFF") then
           beamstat = 0
        else
           beamstat = 2
        endif
           

      endif
*
      nfile=1
*
      nfcop=0
      nrcop=0
      nrtf=0
*
*  We should start the acquisition here, incase we stopped it
*
  495 continue
*
*  Start the VME acquisition if it is stopped and not TON command
*
      if (kmd .ne. 'TON ' .and. vstat .eq. ACQ_STOP) then
         call acq_vme_ctrl(vstat,"start")
      endif
*
*   Initialize the scaler readout timer
*
      call scatman('INIT')
      jdx = 1

*   *************************************************************************
*     The main COPY Loop
*   *************************************************************************
500   continue
*
*   Get a buffer from shared memory
*
      call readipc(evbuf(jdx),32768,nbyt,stat,msgf)
      if (msgf .ne. '    ') then
        msgf='    '
        istop = 1
        if (jdx .eq. 1) go to 530
        dowhile (jdx .lt. 8192)
          evbuf(jdx) = -1
          jdx = jdx + 1
        enddo
        go to 520
      elseif (stat .ne. 0) then
        cstat = 'BAD '
        istop = 5
        go to 530
      endif
      jdx = jdx + nbyt/4
      if (ipass .eq. 0) then
*
*   If this is the first one for the file, save the event number of the
*   first event in this buffer.
*
        ipass = 1
        beginevt = strbufevt
      endif
*
*   Increment our event counter  and compute the event number of the
*   last event in the buffer.
*
      sumevts = sumevts + numbufevts
      lastevt = strbufevt + numbufevts
      if (jdx .lt. 8192) go to 500
*
*   Write the buffer.
*
520   nrcop=nrcop+1
      nrtf=nrtf+1
      call ldfwrit(luouf,'DATA',32768,evbuf,stat)
      jdx = 1
*
*  Error on write?
*
      if (stat .ne. 0) then
        istop = 2
        go to 530
      endif
      if (istop .ne. 0) go to 530
*
*  Is it time for scaler data ?
*
      call scatman('CHEK')
      if (nscat .ne. 0) then
        scnt = 1
        dowhile (nscat .gt. 0)

        call ldfwrit(luouf,'SCAL',32000,scatbuf(1,scnt),stat)
*
*  Error on write?
*
        if (stat .ne. 0) then
          istop = 2
          go to 530
        endif
        nscat = nscat - 1
        scnt = scnt + 1
        enddo
      endif
      if (istop .eq. 0) then
*
*  Check for max file size
*
        if (nrec .ge. maxrec) istop = 3
*
*  Is it time for an AUTOFILE mark ?
*
        if (autofile .ne. 0 .and. mod(nrcop,reclim) .eq. 0) istop = 4
      endif
*
  530 if (istop .ne. 0) then
        evtslost = (lastevt-beginevt-sumevts)*100./
     &                                  (lastevt-beginevt + 1.)
        if (evtslost .lt. 0. .or. evtslost .gt. 100.) evtslost = 100.
*
*  Stop the VME acquisition system, release
*  all shared memory buffers.
*
        call acq_vme_ctrl(vstat,'stop')
*
        call scatman('FLUS')
        if (nscat .ne. 0) then
          scnt = 1
          dowhile (nscat .gt. 0)

          call ldfwrit(luouf,'SCAL',32000,scatbuf(1,scnt),stat)
*
*  Error on write?
*
          if (stat .ne. 0) then
            istop = 2
            go to 535
          endif
          nscat = nscat - 1
          scnt = scnt + 1
        enddo
        endif

535     if (istop .eq. 1) then
          write(cmssg,540) 'File copy stopped by user'
        else if (istop .eq. 2) then
          write(cmssg,540) 'File copy stopped on file ERROR!'
        else if (istop .eq. 3) then
          write(cmssg,540) 'File copy stopped on MAX LDF file size!'
        else if (istop .eq. 4) then
          write(cmssg,540) 'Automatic filemark and header'
        else if (istop .eq. 5) then
          write(cmssg,540) 'SHM input error'
        endif
540     format('>>> ',A)
        call messlog(logut,logup)
        write(cmssg,571) lastevt-beginevt,sumevts, evtslost
571     format('Front-end Events: ',f12.0,' File Events: ',f12.0,
     $                                    ' Percent loss: ',f6.2)
        call messlog(logut, logup)

        write(cmssg,555) nrtf, nrcop
555     format('RECORDS-THIS-FILE, TOTAL-RECORDS COPIED =',2I8)
        call messlog(logut,logup)
        call messlog(logut,logup)

        write(deadtime,572) lastevt-beginevt,sumevts
572     format('$DEADTIME: Front-end events: ',f12.0,
     $  ' File events: ',f12.0)

        if (istop .eq. 2) then 
*
*   File write error.  Clear error, write two file marks and exit.
*
          call filmar(luouf,2,1,ierr)
          if (ierr .ne. 0) go to 1000              !File error return
          return                                   !Error return
        endif
*
*  Write deadtime info record.
*
        call ldfwrit(luouf,'DEAD',128,dt,stat)
        if (stat .ne. 0) go to 1000                !File error return
        if (istop .eq. 4) then
*
*  Time for an Autofile mark.
*  Write one Filemark to close file and keep on trucking
*
          call filmar(luouf,1,0,ierr)
          if (ierr .ne. 0) go to 1000              !Tape error return
*
*  Write new Header
*
          call hedman('HOUT', ierr)
          if(ierr .ne. 0)      go to 1000          !Tape error return
*
*   Start the VME acquisition system
*
          call acq_vme_ctrl(vstat,'start')
          ipass = 0
          sumevts = 0.
          beginevt = 0.
          lastevt = 0.
          istop = 0
          go to 500
        endif
        if (istop .eq. 3) then
          call filmar(luouf,2,1,ierr)
          if (ierr .ne. 0) go to 1000              !File error return
          cfile = ldfonam
          nextfile = nextfile + 1
          do j=1,80
            if (cfile(j:j) .eq. '.' .or. cfile(j:j) .eq. '-') then
              cfile(j:) = ' '
              go to 600
            endif
          enddo
600       continue
          if (nextfile .lt. 10) then
            write(cfile(j:),9000) nextfile
9000        format ('-',i1)
          elseif (nextfile .lt. 100) then
            write(cfile(j:),9010) nextfile
9010        format ('-',i2)
          endif
          call strappend(cfile,'.ldf')
          ciwdraw = 'ouf '
          ciwdraw(5:) = cfile
*****          write(ciwdraw,*) 'ouf ',cfile
          ckmd = 'OUF '
          write(cmssg,*) 'Open LDF file : ',cfile
          call messlog(logut,logup)
          call ldfopen
          if (nrec .ne. 1) then
            write(cmssg,*) "Can't continue using old LDF file!"
            call beep
            go to 1000
          endif
          call hedman('HOUT', ierr)
          if(ierr .ne. 0)      go to 1000          !Tape error return
*
*   Start the VME acquisition system
*
          call acq_vme_ctrl(vstat,'start')
          ipass = 0
          sumevts = 0.
          beginevt = 0.
          lastevt = 0.
          istop = 0
          nrtf = 0
          nrcop = 0
          go to 500
        endif
        if (istop .eq. 1 .or. istop .eq. 3) then
*
*  User stop, record count done or input error.  Write two file marks
*  and exit.
*
          call filmar(luouf,2,1,ierr)
          if (ierr .ne. 0) go to 1000             !File error return
        endif
        return
      endif
      go to 500
*
 1000 ierr=1
      return
C
 1200 call messlog(logut,logup)
      ierr=1
      return
      end
********************************************************************
********************************************************************
      subroutine beep

      implicit none

      byte        ibell/07/
      character*1 bell
      equivalence (ibell, bell)
      integer*4   mssg,namprog,logut,logup,lisflg
      character*4  msgf
*
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF

      msgf = ' '
10    write(*,9000) bell
9000  format(a1,$)
      call wait(500,1,0)
      write(*,9000) bell
      call wait(5,2,0)
      if (msgf .eq. '    ') go to 10
      return
      end
