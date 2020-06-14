******************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1997-2003
*
*
*                         IMPORTANT NOTICE TO USER
*
*     All statements, technical information and recommendations contained
*     herein are based on tests we believe to be reliable, but the accuracy
*     or completeness thereof is not guaranteed, and the following  is
*     made in lieu of all warranties expressed or implied:
*
*     Our only obligation shall be to correct or replace, at our convenience,
*     any part of the product proven to be defective.  We shall not be liable
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME Data Acquisition System
*
*    File:         /usr/users/mcsq/Dacq/modu_setup.f
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    9/22/97    MCSQ
*
*    6/ 2/99    MCSQ        Add LeCroy 3377 TDC
*
*    9/ 1/99    MCSQ        Add shaper amplifier for microball
*
*    7/ 2/03    MCSQ        Add CAEN ADC and TDC modules(VME modules)
*
*   11//26/03   MCSQ        The CAEN TDC manual claims that the full range
*                           is linear from 140 to 1200 ns. IT IS NOT. 
*                           Paul Hausladen found that it is 1/x function.
*                           Changed this code to Hausladens formula for
*                           register value computation.
*****************************************************************************/
      implicit none
      include  'modu_setup.for'

      integer*4     err,i,ln_num,strlen
      integer*4     mt_code,ftype,next
      integer*4     iargc,debug,mterr
      character*16  mt
      character*80  infile
      character*80  outfile
      character*64  message

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      ln_num = 1
      if (iargc() .lt. 1) then
        write(*,9000)
9000    format(' Need filename for setup data file')
        call exit(99)
      endif
      call getarg(1,infile)
      open(unit=1,file=infile,access='sequential',status='old',
     1     err=910)
*
      i = index(infile,'.')
      if (i .ne. 0) then
        outfile = infile(1:i-1)
      else
        outfile = infile
      endif
      call strappend(outfile,'.log')
      debug = 0
      if (iargc() .eq. 2) debug = 1
      mterr = 0
      mt_line = 1
      next = 1
10    call get_field(next,mt,ftype,err)
      next = 1
      if (err .eq. READ_EOF) go to 900
      if (err .eq. READ_MT) go to 14
      mterr=ERR_READ_MT
      go to 10
*
14    if (mterr .ne. 0) then
        call error_msg(mterr,ln_num)
        err = 0
      endif
15    call get_field(next,mt,ftype,err)
      if (err .eq. READ_EOF) go to 900
      if (err .eq. READ_MT) go to 10
      mt_line = line_num
      type 9010,line_num,mt(1:strlen(mt))
9010  format(' $$ Line #',i4,':  Processing Module  ',a)
      call mt_match(mt,mt_code,err)
      if (err .ne. 0) go to 100
      err = 0
      if (mt_code .eq. PHIL) then
        call read_phil(err)
      else if (mt_code .eq. SILENA) then
        call read_silena(err)
      else if (mt_code .eq. LRS_4300) then
        call read_lrs4300(err)
      else if (mt_code .eq. HHIRF_ADC) then
        call read_hhirf_adc(err)
      else if (mt_code .eq. LRS_4508) then
        call read_lrs4508(err)
      else if (mt_code .eq. LRS_4418) then
        call read_lrs4418(err)
      else if (mt_code .eq. UE_CLOCK) then
        call read_ue_clock(err)
      else if (mt_code .eq. PHIL_7106) then
        call read_phil7106(err)
      else if (mt_code .eq. LRS_4415A) then
        call read_lrs4415a(err)
      else if (mt_code .eq. LRS_3511) then
        call read_lrs3511(err)
      else if (mt_code .eq. LRS_4413) then
        call read_lrs4413(err)
      else if (mt_code .eq. LRS_4416B) then
        call read_lrs4416b(err)
      else if (mt_code .eq. LRS_3377) then
        call read_lrs3377(err)
      else if (mt_code .eq. SHAPER) then
        call read_shaper(err)
      else if (mt_code .eq. CAEN_775) then
        call read_caen775(err)
      else if (mt_code .eq. CAEN_785) then
        call read_caen785(err)
      endif
100   call error_msg(err,ln_num)
      call get_field(0,mt,ftype,err)
      if (err .eq. READ_MT) go to 15
      go to 10
*
900   continue
      if (err_count .ne. 0) call exit(99)
      call format_silena()
      if (debug .ne. 0) then
        open(unit=2,file=outfile,access='sequential',
     1        status='unknown',err=920)
        call list_modu()
      endif
      err = 0
      call inhibit('save')
      call load_phil(err)
      call clear_silena(err)
      call load_silena(err)
      call load_hhirf_adc(err)
      call load_ue_clock(err)
      call load_lrs4508(err)
      call load_lrs4418(err)
      call load_lrs4300(err)
      call load_phil7106(err)
      call load_lrs4415a(err)
      call load_lrs3511(err)
      call load_lrs4413(err)
      call load_lrs4416b(err)
      call load_lrs3377(err)
      call load_shaper(err)
      call load_caen775(err)
      call load_caen785(err)
      call inhibit('restore')
      if (err .ne. 0) call exit(99)
      go to 999
*
910   write(*,9020) infile
9020  format(' Can''t open input file - ',a)
      call exit(99)
*
920   write(*,9030)
9030  format(' Can''t open log file - modu_setup.log')
*
999   end
*******************************************************************************
*******************************************************************************
      subroutine mt_match(mt,code,err)
      implicit none
      include  'modu_setup.for'

      character*(*) mt
      integer*4     code,err

      character*16  mt_types(23) /'phil_7164','phil_7166','phil_7167',
     1  'phil_7186','phil_7187','lrs_4300','silena_4418','lrs_4508',
     2  'lrs_4418','hhirf_adc','ue_clock','phil_7106','lrs_4415a',
     3  'lrs_3511','lrs_4413','lrs_4416b','lrs_3351','lrs_3377',
     4  'shaper','caen_775','caen_785','mcsq',' '/
      integer*4     mt_codes(23) /PHIL,PHIL,PHIL,PHIL,PHIL,LRS_4300,
     1   SILENA,LRS_4508,LRS_4418,HHIRF_ADC,UE_CLOCK,PHIL_7106,
     2   LRS_4415A,LRS_3511,LRS_4413,LRS_4416B,SILENA,LRS_3377,
     3   SHAPER,CAEN_775,CAEN_785,MCSQ,0/
      integer*4     i,j,len,match,strlen,slen,tlen

      code = 0
      err = 0
      len = strlen(mt)
      j = 0
      match = 0
      i = 1
      do while (mt_types(i) .ne. ' ')
        tlen = strlen(mt_types(i))
        if (tlen .lt. len) then
          slen = tlen
        else
          slen = len
        endif
        if (mt(1:slen) .eq. mt_types(i)(1:slen)) then
          j = j + 1
          match = i
        endif
        i = i + 1
      enddo
      if (j .eq. 0) then
        err = ERR_MOD_UNKN
      else if (j .gt. 1) then
        err = ERR_MOD_AMBIG
      else
        code = mt_codes(match)
      endif
      end
*******************************************************************************
*******************************************************************************
      subroutine dat_match(dat,code,err)
      implicit none
      include  'modu_setup.for'

      character*(*) dat
      integer*4     code,err

      character*16  dat_types(16) /'lower_threshold','upper_threshold',
     1  'offset_memory','pedestals','common_threshold','enables',
     2  'ge_only','bgo_only','anticoinc','delays','mode','overflow',
     3  'conversion_gain','maximum_time','gain',' '/
      integer*4     dat_codes(16) /LOW,UP,OFF,PED,COM,ENABLE,
     1   GE_ONLY,BGO_ONLY,ANTI,DELAY,MODE,OVFL,MODE,UP,GAINS,0/

      integer*4     i,j,len,match,strlen,slen,tlen

      code = 0
      err = 0
      len = strlen(dat)
      j = 0
      match = 0
      i = 1
      dowhile (dat_types(i) .ne. ' ')
        tlen = strlen(dat_types(i))
        if (tlen .lt. len) then
          slen = tlen
        else
          slen = len
        endif
        if (dat(1:slen) .eq. dat_types(i)(1:slen)) then
          j = j + 1
          match = i
        endif
        i = i + 1
      enddo
      if (j .eq. 0) then
        err = ERR_DAT_UNKN
      else if (j .gt. 1) then
        err = ERR_DAT_AMBIG
      else
        code = dat_codes(match)
      endif
      end
*******************************************************************************
*
*  Get command string
*
*  RETURNS:  modutype - 1 means module type in first field
*                       0 means data fields
*            endflag  - 1 means end of file
*******************************************************************************
      subroutine readcmd(modutype,endflag)
      implicit none

      integer*4     i,j,k,strlen
      integer*4     modutype,endflag
      integer*4     line_num,mt_line,err_count
      character*256 line
      integer*4     indx(2,10)
      integer*4     itype(10),nf,inext,cur
      common /cmd/  line,indx,itype,nf,inext,cur
      common /line_num/  line_num,mt_line,err_count

      endflag = 0
      modutype = 0
5     continue
      if (inext .eq. 0) then
*
*  Read new line
*
10      read(1,1000,err=20,end=20) line
1000    format(a)
        line_num = line_num + 1
        call strlower(line)
        call strparse(line,indx,itype,nf,inext)
*
*  Ignore blank lines
*
        if (nf .eq. 0) go to 10
        cur = 1
        if (line(indx(1,1):indx(2,1)) .eq. 'mt') then
          modutype = 1
        endif
      else
*
*  Get additional fields from old line
*
        i = inext
        call strparse(line(i:),indx,itype,nf,inext)
        cur = 1
        if (inext .ne. 0) inext = inext + i - 1
        do j=1,nf
          indx(1,j) = indx(1,j) + i -1
          indx(2,j) = indx(2,j) + i -1
        enddo
      endif
      do j=1,nf
*
*  Comments begin with semicolon or asterisk
*
        if ((line(indx(1,j):indx(1,j)) .eq. ';') .or. 
     1                    (line(indx(1,j):indx(1,j)) .eq. '*')) then
          inext = 0
          if (j .eq. 1) go to 5
          nf = j -1
          return
        endif
      enddo
      return
*
20    endflag = 1
      end
*******************************************************************************
*
*Call:    next  - int*4  0 means get current field again
*                        nonzero means get next field
*
*Returns: field - character*(*)  ascii string
*         ftype - int*4 - field type, 1 means alpha and 2 means numeric
*         err   - 0 means normal field
*                 READ_MT - module type field
*                 READ_EOF - end-of-file
*******************************************************************************
      subroutine get_field(next,field,ftype,err)
      implicit none
      include  'modu_setup.for'

      character*(*) field
      integer*4     next,ftype,err
      integer*4     modutype,endflag
      integer*4     hex,l,minus,temp,strlen
      character*32  string

      character*256 line
      integer*4     indx(2,10)
      integer*4     itype(10),nf,inext,cur
      common /cmd/  line,indx,itype,nf,inext,cur

      err = 0
      if (next .ne. 0) cur = cur + 1
      if (cur .ne. 0 .and. cur .le. nf) then
        field = line(indx(1,cur):indx(2,cur))
        ftype = itype(cur)
      else
        call readcmd(modutype,endflag)
        field = line(indx(1,cur):indx(2,cur))
        ftype = itype(cur)
      endif
      if (ftype .eq. 2) then
        hex = 0
        minus = 0
        string = field
        if (string(1:1) .eq. '-') then
          string = string(2:)
          minus = 1
        endif
        l = strlen(string)
        if (l .lt. 2) then
        else if (string(2:2) .eq. 'x' .or. string(2:2) .eq. 'X') then
          string = string(3:)
          hex = 1
        else if (string(l:l) .eq. 'h' .or. string(l:l) .eq. 'H') then
          string = string(1:l-1)
          hex = 1
        endif
        if (hex .ne. 0) then
          read(string,1000) temp
1000      format(z)
          if (minus .ne. 0) temp = -temp
          write(field,9000) temp
9000      format(i8)
        endif
      endif
      if (cur .eq. 1 .and. modutype .ne. 0) err = READ_MT
      if (endflag .ne. 0) err = READ_EOF
      end
*******************************************************************************
*
*Call:    next  - int*4  0 means get current field again
*                        nonzero means get next field
*
*Returns: num   - int*4  number if numeric field
*         err   - int*4  ERR_NAN if alphanumeric field
*                        ERR_CONV if FORTRAN conversion error
*******************************************************************************
      subroutine  get_numeric(next,num,err)
      implicit none
      include  'modu_setup.for'

      integer*4     i,err,ftype,next,num
      character*16  field

      err = 0
      call get_field(next,field,ftype,err)
      if (err .ne. 0) return
      if (ftype .ne. 2) then
        err = ERR_NAN
        return
      endif
      read(field,1000,err=10) num
1000  format(i)
      return
*
10    err = ERR_CONV
      return
*
      end
*******************************************************************************
*
*Call:    next  - int*4  0 means get current field again
*                        nonzero means get next field
*
*Returns: num   - int*4  number if numeric field
*         err   - int*4  ERR_NO_C if alphanumeric field
*                        ERR_CONV if FORTRAN conversion error
*******************************************************************************
      subroutine  get_crate(next,num,err)
      implicit none
      include  'modu_setup.for'

      integer*4     i,err,ftype,next,num
      character*16  field

      err = 0
      call get_field(next,field,ftype,err)
      if (err .ne. 0) return
      if (ftype .ne. 2) then
        err = ERR_NO_C
        return
      endif
      read(field,1000,err=10) num
1000  format(i)
      if (num .lt. 0 .or. num .gt. 17) then
        err = ERR_C_VAL
        return
      endif
      if (num .eq. 8 .or. num .eq. 9) then
        err = ERR_C_VAL
        return
      endif
      return
*
10    err = ERR_CONV
      return
*
      end
*******************************************************************************
*******************************************************************************
      subroutine  get_slot(n1,n2,err)
      implicit none
      include  'modu_setup.for'

      integer*4     i,j,err,n1,n2,index
      integer*4     ftype
      character*16  field

      err = 0
      call get_field(1,field,ftype,err)
      if (ftype .ne. 2) then
        err = ERR_NO_N
        return
      endif
      j = index(field,'-')
      if (j .eq. 0) then
        read(field,1000,err=10) n1
1000    format(i)
        n2 = n1
      else
        read(field(1:j-1),1000,err=10) n1
        read(field(j+1:),1000,err=10) n2
      endif
      if (n1 .lt. 1 .or. n1 .gt. 23) then
        err = ERR_N_VAL
        return
      endif
      if (n2 .lt. 1 .or. n2 .gt. 23) then
        err = ERR_N_VAL
        return
      endif
      if (n2 .lt. n1) then
        err = ERR_2N
        return
      endif
      return
*
10    err = ERR_CONV
      return
*
      end
*******************************************************************************
*******************************************************************************
      subroutine error_msg(err,ln_num)
      implicit none
      include  'modu_setup.for'

      integer*4  err,ln,ln_num,strlen
      integer*4  val,ftype
      character*4 field

      integer*4     line_num,mt_line,err_count
      character*256 line
      integer*4     indx(2,10)
      integer*4     itype(10),nf,inext,cur
      common /cmd/  line,indx,itype,nf,inext,cur
      common /line_num/  line_num,mt_line,err_count

      if (err .le. 0) return
      err_count = err_count + 1
      ln = line_num
      call get_field(0,field,ftype,val)
      if (val .eq. READ_MT) ln = ln -1
      if (mt_line .ne. ln) then
        if (val .ne. READ_MT) then
          write(*,9000) mt_line,ln,line(1:strlen(line))
        else
          write(*,9000) mt_line,ln
        endif
9000    format(' ************** ERROR in lines ',i4,' thru ',i4,
     1      ' **************', /,'    ',a)
      else
        write(*,9005) mt_line,line(1:strlen(line))
9005    format(' **** ERROR at line number ',i4,
     1      ' ****', /,'    ',a)
      endif
      if (err .eq. ERR_C_VAL) then
        write(*,9010)
9010    format(' ERR: Invalid CAMAC crate number')
      else if (err .eq. ERR_N_VAL) then
        write(*,9020)
9020    format(' ERR: Invalid CAMAC slot number')
      else if (err .eq. ERR_2N) then
        write(*,9030)
9030    format(' ERR: Second CAMAC slot number less than first')
      else if (err .eq. ERR_MOD_UNKN) then
        write(*,9040)
9040    format(' ERR: Unknown module type')
      else if (err .eq. ERR_MOD_AMBIG) then
        write(*,9050) 
9050    format(' ERR: Ambiguous module type specification')
      else if (err .eq. ERR_DAT_UNKN) then
        write(*,9060)
9060    format(' ERR: Unknown data type')
      else if (err .eq. ERR_DAT_AMBIG) then
        write(*,9070)
9070    format(' ERR: Ambiguous data type specification')
      else if (err .eq. ERR_CONV) then
        write(*,9080)
9080    format(' ERR: Conversion error for numeric field')
      else if (err .eq. ERR_NAN) then
        write(*,9090)
9090    format(' ERR: Expected number, found alphanumeric field')
      else if (err .eq. ERR_DATA_TYPE) then
        write(*,9100)
9100    format(' ERR: Invalid data type for this module')
      else if (err .eq. ERR_DATA_VAL) then
        write(*,9110)
9110    format(' ERR: Data value out_of_range')
      else if (err .eq. ERR_DUP_DATA) then
        write(*,9120)
9120    format(' ERR: Duplicate data specified for this data type')
      else if (err .eq. ERR_TOO_FEW) then
        write(*,9130)
9130    format(' ERR: Insufficient data')
      else if (err .eq. ERR_PRIOR_MOD_TYPE) then
        write(*,9140) ln_num
9140    format(' ERR: Module type conflict.  See line # ',i4)
      else if (err .eq. ERR_PRIOR_LOW) then
        write(*,9150) ln_num
9150    format(' ERR: Lower Thresholds previously set at line ',i4)
      else if (err .eq. ERR_PRIOR_UP) then
        write(*,9160) ln_num
9160    format(' ERR: Upper Thresholds previously set at line ',i4)
      else if (err .eq. ERR_PRIOR_OFF) then
        write(*,9170) ln_num
9170    format(' ERR: Offset memory previously set at line ',i4)
      else if (err .eq. ERR_PRIOR_PED) then
        write(*,9180) ln_num
9180    format('ERR: Pedestals previously set at line ',i4)
      else if (err .eq. ERR_PRIOR_COM) then
        write(*,9190) ln_num
9190    format(' ERR: Common threshold previously set at line ',i4)
      else if (err .eq. ERR_PRIOR_ENABLE) then
        write(*,9200) ln_num
9200    format('ERR: Enables previously set at line ',i4)
      else if (err .eq. ERR_PRIOR_GEN) then
        write(*,9210) ln_num
9210    format(' ERR: Generate function previously set at line ',i4)
      else if (err .eq. ERR_PRIOR_DELAY) then
        write(*,9220) ln_num
9220    format(' ERR: Delays previously set at line ',i4)
      else if (err .eq. ERR_MULTI_C) then
        write(*,9230) 
9230    format(' ERR: Multiple CAMAC Crate statements')
      else if (err .eq. ERR_MULTI_N) then
        write(*,9240) 
9240    format(' ERR: Multiple CAMAC Slot statements')
      else if (err .eq. ERR_NO_C) then
        write(*,9250) 
9250    format(' ERR: NO CAMAC Crate statement')
      else if (err .eq. ERR_NO_N) then
        write(*,9260) 
9260    format(' ERR: NO CAMAC Slot statement')
      else if (err .eq. ERR_TOO_MANY) then
        write(*,9270) 
9270    format(' ERR: Too many data')
      else if (err .eq. ERR_UNKN_KEY) then
        write(*,9280) 
9280    format(' ERR: Unknown parameter')
      else if (err .eq. ERR_DATA_ILL) then
        write(*,9290) 
9290    format(' ERR: Illegal data value')
      else if (err .eq. ERR_READ_MT) then
        write(*,9300) 
9300    format(' ERR: No module type statement found - mt=')
      else
        write(*,9310) err
9310    format(' ERR: ',i4,' Unknown error code')
      endif
      write(*,9320)
9320  format(' ')
      end
*******************************************************************************
*******************************************************************************
      subroutine read_phil(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           /16, 0, 4095,     ! lower threshold
     2            16, 0, 4095,     ! upper threshold
     3             0, 0, 0,        ! offset memory
     4            16, -4096, 4095, ! pedestals
     5             0, 0, 0,        ! common threshold
     6             0, 0, 0,        ! enable
     7             0, 0, 0,        ! ge_only
     8             0, 0, 0,        ! bgo_only
     9             0, 0, 0,        ! anticoinc
     1             0, 0, 0,        ! delay
     2             0, 0, 0,        ! mode
     3             0, 0, 0/        ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.low_set = 0
      temp_modu.up_set = 0
      temp_modu.ped_set = 0
      temp_modu.mt_type = PHIL
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
          if (k .eq. LOW) then
            if (temp_modu.low_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.low(i) = temp(i)
            enddo
            temp_modu.low_set = mt_line 
          else if (k .eq. UP) then
            if (temp_modu.up_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.up(i) = temp(i)
            enddo
            temp_modu.up_set = mt_line
          else if (k .eq. PED) then
            if (temp_modu.ped_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.ped(i) = temp(i)
            enddo
            temp_modu.ped_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_silena(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           /8,    0, 1000,  ! lower threshold
     2            8, 8500, 10000, ! upper threshold
     3            8, -123,  123,  ! offset memory
     4            0, 0, 0,        ! pedestals
     5            1, 0, 1200,     ! common threshold
     6            0, 0, 0,        ! enable
     7            0, 0, 0,        ! ge_only
     8            0, 0, 0,        ! bgo_only
     9            0, 0, 0,        ! anticoinc
     1            0, 0, 0,        ! delay
     2            1, 0, 32767,    ! mode
     3            1, 0, 1/        ! overflow

      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.low_set = 0
      temp_modu.up_set = 0
      temp_modu.off_set = 0
      temp_modu.com_set = 0
      temp_modu.mode_set = 0
      temp_modu.ovfl_set = 0
      temp_modu.mt_type = SILENA
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .lt. 0) go to 150
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .ne. 0) go to 150
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
          if (k .eq. LOW) then
            if (temp_modu.low_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.low(i) = temp(i)
            enddo
            temp_modu.low_set = mt_line
          else if (k .eq. UP) then
            if (temp_modu.up_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.up(i) = temp(i)
            enddo
            temp_modu.up_set = mt_line
          else if (k .eq. OFF) then
            if (temp_modu.off_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.off(i) = temp(i)
            enddo
            temp_modu.off_set = mt_line
          else if (k .eq. COM) then
            if (temp_modu.com_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.com = temp(1)
            temp_modu.com_set = mt_line
          else if (k .eq. MODE) then
            if (temp_modu.mode_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.mode = temp(1)
            temp_modu.mode_set = mt_line
          else if (k .eq. OVFL) then
            if (temp_modu.ovfl_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.ovfl = temp(1)
            temp_modu.ovfl_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_lrs4300(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4            16, 0, 255,   ! pedestals
     5             0, 0, 0,     ! common threshold
     6             0, 0, 0,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             1, 0, 65535, ! mode
     3             1, 0, 1/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.ped_set = 0
      temp_modu.mode_set = 0
      temp_modu.ovfl_set = 0
      temp_modu.mt_type = LRS_4300
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
          if (k .eq. PED) then
            if (temp_modu.ped_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.ped(i) = temp(i)
            enddo
            temp_modu.ped_set = mt_line
          else if (k .eq. MODE) then
            if (temp_modu.mode_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.mode = temp(1)
            temp_modu.mode_set = mt_line
          else if (k .eq. OVFL) then
            if (temp_modu.ovfl_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.ovfl = temp(1)
            temp_modu.ovfl_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_hhirf_adc(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             0, 0, 0,     ! common threshold
     6            16, 0, 1,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.enable_set = 0
      temp_modu.mt_type = HHIRF_ADC
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
          if (temp_modu.enable_set .ne. 0) then
            err = ERR_DUP_DATA
            return
          endif
          do i=1,dat_params(1,k)
            temp_modu.enable(i) = temp(i)
          enddo
          temp_modu.enable_set = mt_line
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_lrs4418(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             0, 0, 0,     ! common threshold
     6             0, 0, 0,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1            16, 0, 128,   ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.delay_set = 0
      temp_modu.mt_type = LRS_4418
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
          if (temp_modu.delay_set .ne. 0) then
            err = ERR_DUP_DATA
            return
          endif
          do i=1,dat_params(1,k)
            temp_modu.delay(i) = temp(i)
          enddo
          temp_modu.delay_set = mt_line
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_lrs4508(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             0, 0, 0,     ! common threshold
     6             0, 0, 0,     ! enable
     7             1, 0, 0,     ! ge_only
     8             1, 0, 0,     ! bgo_only
     9             1, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.gen_set = 0
      temp_modu.mt_type = LRS_4508
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .gt. 0) return
          if (err .lt. 0) go to 100
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        if (dat_end .ne. 1) then
          err = ERR_DUP_DATA
          return
        endif
        if (temp_modu.gen_set .eq. 0) then
          temp_modu.gen_type = dat_type(1)
          temp_modu.gen_set = mt_line
        else
          err = ERR_DUP_DATA
          return
        endif
        next = 0
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
      end
*******************************************************************************
*******************************************************************************
      subroutine read_ue_clock(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             0, 0, 0,     ! common threshold
     6             0, 0, 0,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             1, 0, 1,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*16  mode0 /'mode_0'/
      character*16  mode1 /'mode_1'/
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.mode_set = 0
      temp_modu.mt_type = UE_CLOCK
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          if (field .eq. mode0)  go to 100
          if (field .eq. mode1) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          if (k .eq. MODE) then
            call get_field(next,field,ftype,err)
            if (err .ne. 0) go to 150
            if (ftype .eq. 1) then
              temp(1) = -1
              if (field .eq. mode0) temp(1) = 0
              if (field .eq. mode1) temp(1) = 1
              next = 1
              if (temp(1) .ge. 0) go to 110
            endif
            next = 0
          endif
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
110       if (k .eq. MODE) then
            if (temp_modu.mode_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.mode = temp(1)
            temp_modu.mode_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_phil7106(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             1, 10, 1033, ! common threshold
     6            16, 0, 1,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.com_set = 0
      temp_modu.enable_set = 0
      temp_modu.mt_type = PHIL_7106
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            temp(i) = abs(temp(i))
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
          if (k .eq. COM) then
            if (temp_modu.com_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.com = temp(i) - 10
            enddo
            temp_modu.com_set = mt_line
          else if (k .eq. ENABLE) then
            if (temp_modu.enable_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.enable(i) = temp(i)
            enddo
            temp_modu.enable_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_lrs4415a(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,len,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             0, 0, 0,     ! common threshold
     6            16, 0, 1,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             1, 0, 1,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      character*32  test /'test'/
      character*32  normal /'normal'/
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.enable_set = 0
      temp_modu.mode_set = 0
      temp_modu.mt_type = LRS_4415A
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          len = strlen(field)
          if (field(1:len) .eq. test(1:len))  go to 100
          if (field(1:len) .eq. normal(1:len)) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          if (k .eq. MODE) then
            call get_field(next,field,ftype,err)
            if (err .ne. 0) go to 150
            if (ftype .eq. 1) then
              len = strlen(field)
              temp(1) = -1
              if (field(1:len) .eq. test(1:len)) temp(1) = 1
              if (field(1:len) .eq. normal(1:len)) temp(1) = 0
              next = 1
              if (temp(1) .ge. 0) go to 110
            endif
            next = 0
          endif
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
110       if (k .eq. ENABLE) then
            if (temp_modu.enable_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.enable(i) = temp(i)
            enddo
            temp_modu.enable_set = mt_line
          else if (k .eq. MODE) then
            if (temp_modu.mode_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.mode = temp(1)
            temp_modu.mode_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_lrs3511(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,len,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             1, 0, 65535, ! offset memory
     4             0, 0, 0,     ! pedestals
     5             0, 0, 0,     ! common threshold
     6             0, 0, 0,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             1, 250, 8000, ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      integer*4     cg(6)   /8000,4000,2000,1000,500,250/
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.off_set = 0
      temp_modu.mode_set = 0
      temp_modu.mt_type = LRS_3511
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          len = strlen(field)
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
110       if (k .eq. OFF) then
            if (temp_modu.off_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.off(1) = temp(1)
            temp_modu.off_set = mt_line
          else if (k .eq. MODE) then
            if (temp_modu.mode_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,6
              if (cg(i) .eq. temp(1)) go to 130
            enddo
            err = ERR_DATA_ILL
            return
*
130         temp_modu.mode = i*256
            temp_modu.mode_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_lrs4413(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             1, 0, 1023,  ! common threshold
     6            16, 0, 1,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.com_set = 0
      temp_modu.enable_set = 0
      temp_modu.mt_type = LRS_4413
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            temp(i) = abs(temp(i))
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
          if (k .eq. COM) then
            if (temp_modu.com_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.com = temp(i)
            enddo
            temp_modu.com_set = mt_line
          else if (k .eq. ENABLE) then
            if (temp_modu.enable_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.enable(i) = temp(i)
            enddo
            temp_modu.enable_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_lrs4416b(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,len,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             1, 0, 1023,  ! common threshold
     6            16, 0, 1,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.enable_set = 0
      temp_modu.com_set = 0
      temp_modu.mt_type = LRS_4416B
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            temp(i) = abs(temp(i))
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
110       if (k .eq. ENABLE) then
            if (temp_modu.enable_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.enable(i) = temp(i)
            enddo
            temp_modu.enable_set = mt_line
          else if (k .eq. COM) then
            if (temp_modu.mode_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.com = temp(1)
            temp_modu.com_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_lrs3377(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             1, 0, 32767, ! full scale time
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             1, 0, 1,     ! common mode(stop/start)
     6             0, 0, 0,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*16  start /'start'/
      character*16  stop /'stop'/
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.up_set = 0
      temp_modu.com_set = 0
      temp_modu.mt_type = LRS_3377
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          if (field .eq. start)  go to 100
          if (field .eq. stop) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          if (k .eq. COM) then
            call get_field(next,field,ftype,err)
            if (err .ne. 0) go to 150
            if (ftype .eq. 1) then
              temp(1) = -1
              if (field .eq. start) temp(1) = 0
              if (field .eq. stop) temp(1) = 1
              next = 1
              if (temp(1) .ge. 0) go to 110
            endif
            next = 0
          endif
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
110       if (k .eq. COM) then
            if (temp_modu.com_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.com = temp(1)
            temp_modu.com_set = mt_line
          else if (k .eq. UP) then
            if (temp_modu.up_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.up(i) = temp(i)
            enddo
            temp_modu.up_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine read_shaper(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 0, 0, 0,     ! lower threshold
     2             0, 0, 0,     ! upper threshold
     3             0, 0, 0,     ! offset memory
     4            16, 0, 255,   ! pedestals/gains
     5             0, 0, 0,     ! common threshold
     6             0, 0, 0,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(16)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.ped_set = 0
      temp_modu.mode_set = 0
      temp_modu.ovfl_set = 0
      temp_modu.mt_type = SHAPER
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (c .lt. 0) then
          err = ERR_NO_C
          return
        endif
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   CAMAC Crate number ?
*
      if (field .eq. 'c' .or. field .eq. 'crate') then
        if (c .ge. 0) then
          err = ERR_MULTI_C
          return
        endif
        call get_crate(next,c,err)
        if (err .ne. 0) return
*
*   CAMAC Slot numbers ?
*
      else if (field .eq. 'n' .or. field .eq. 'slot') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
          if (k .eq. GAINS) then
            if (temp_modu.ped_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.ped(i) = temp(i)
            enddo
            temp_modu.ped_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end

*******************************************************************************
*******************************************************************************
      subroutine read_caen775(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 32,   0, 2047, ! lower threshold
     2              1, 140, 1200, ! full scale time
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             1, 0, 1,     ! common mode(stop/start)
     6             0, 0, 0,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(32)
      character*16  start /'start'/
      character*16  stop /'stop'/
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.low_set = 0
      temp_modu.up_set = 0
      temp_modu.com_set = 0
      temp_modu.mt_type = CAEN_775
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   TDC numbers ?
*
      if (field .eq. 'n' .or. field .eq. 'tdc') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          if (field .eq. start)  go to 100
          if (field .eq. stop) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          if (k .eq. COM) then
            call get_field(next,field,ftype,err)
            if (err .ne. 0) go to 150
            if (ftype .eq. 1) then
              temp(1) = -1
              if (field .eq. start) temp(1) = 0
              if (field .eq. stop) temp(1) = 1
              next = 1
              if (temp(1) .ge. 0) go to 110
            endif
            next = 0
          endif
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
110       if (k .eq. COM) then
            if (temp_modu.com_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            temp_modu.com = temp(1)
            temp_modu.com_set = mt_line
          else if (k .eq. LOW) then
            if (temp_modu.low_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.low(i) = temp(i)
            enddo
            temp_modu.low_set = mt_line
          else if (k .eq. UP) then
            if (temp_modu.up_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.up(i) = temp(i)
            enddo
            temp_modu.up_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end

*******************************************************************************
*******************************************************************************
      subroutine read_caen785(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,code,strlen
      integer*4     ftype,num,c,n1,n2,next
      integer*4     dat_type(11),dat_end
      integer*4     dat_params(3,12)
     1           / 32,   0, 2047, ! lower threshold
     2             0, 0, 0,       ! upper threshold
     3             0, 0, 0,     ! offset memory
     4             0, 0, 0,     ! pedestals
     5             0, 0, 0,     ! common mode
     6             0, 0, 0,     ! enable
     7             0, 0, 0,     ! ge_only
     8             0, 0, 0,     ! bgo_only
     9             0, 0, 0,     ! anticoinc
     1             0, 0, 0,     ! delay
     2             0, 0, 0,     ! mode
     3             0, 0, 0/     ! overflow
      integer*4     dat_cnt,temp(32)
      character*32  field
      record /modu_data/ temp_modu

      integer*4     line_num,mt_line,err_count
      common /line_num/  line_num,mt_line,err_count

      c = -1
      n1 = -1
      n2 = 0
      temp_modu.low_set = 0
      temp_modu.mt_type = CAEN_785
      temp_modu.mt_line = mt_line
      next = 1
10    call get_field(next,field,ftype,err)
      if (err .ne. 0) then
        if (n1 .lt. 0) then
          err = ERR_NO_N
          return
        endif
        call dat_save(c,n1,n2,temp_modu,err)
        return
      endif
*
*   ADC numbers ?
*
      if (field .eq. 'n' .or. field .eq. 'adc') then
        if (n1 .gt. 0) then
          err = ERR_MULTI_N
          return
        endif
        call get_slot(n1,n2,err)
        if (err .ne. 0) return
*
*   Data specification
*
      else if (field .eq. 'data') then
        do i=1,11
          call get_field(next,field,ftype,err)
          if (err .ne. 0) go to 150
          if (ftype .eq. 2) go to 100
          call dat_match(field,code,err)
          if (err .ne. 0) return
          if (dat_params(1,code) .le. 0) then
            err = ERR_DATA_TYPE
            return
          endif
          dat_type(i) = code
        enddo
100     continue
        dat_end = i - 1
        if (dat_end .eq. 0) then
          err = ERR_DATA_TYPE
          return
        endif
        next = 0
        do j=1,dat_end
          k = dat_type(j)
          do i=1,dat_params(1,k)
            call get_numeric(next,temp(i),err)
            if (err .lt. 0) go to 150
            if (err .ne. 0) return
            if (temp(i) .lt. dat_params(2,k) .or.
     1                         temp(i) .gt. dat_params(3,k)) then
              err = ERR_DATA_VAL
              return
            endif
            next = 1
          enddo
110       if (k .eq. LOW) then
            if (temp_modu.low_set .ne. 0) then
              err = ERR_DUP_DATA
              return
            endif
            do i=1,dat_params(1,k)
              temp_modu.low(i) = temp(i)
            enddo
            temp_modu.low_set = mt_line
          endif
        enddo
      else
        if (ftype .eq. 1) then
          err = ERR_UNKN_KEY
        else
          err = ERR_TOO_MANY
        endif
        return
      endif
      go to 10
*
150   err = ERR_TOO_FEW
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine dat_save(c,n1,n2,temp_modu,err)
      implicit none
      include  'modu_setup.for'

      integer*4 c,n1,n2,err

      integer*4 i,j,k,new

      integer*4 m_index
      record /modu_data/ modus(200),temp_modu
      common /modu/  m_index,modus

      do i=n1,n2
        new = -1
        do j=1,m_index
          if (c .eq. -1) then
            if (modus(j).n .eq. i .and. modus(j).mt_type .eq.
     &            temp_modu.mt_type) then
              new = j
              go to 10
            endif
          else
            if (modus(j).c .eq. c .and. modus(j).n .eq. i) then
              new = j
              go to 10
            endif
          endif
        enddo
10      if (new .lt. 0) then
          m_index = m_index + 1
          modus(m_index) = temp_modu
          modus(m_index).c = c
          modus(m_index).n = i
        else
          if (temp_modu.mt_type .ne. modus(new).mt_type) then
            call error_msg(ERR_PRIOR_MOD_TYPE,modus(new).mt_line)
            return
          endif
          if (temp_modu.low_set .ne. 0) then
            if (modus(new).low_set .ne. 0) then
              call error_msg(ERR_PRIOR_LOW,modus(new).low_set)
              return
            endif
            do k=1,16
              modus(new).low(k) = temp_modu.low(k)
            enddo
            modus(new).low_set = temp_modu.low_set
          endif
          if (temp_modu.up_set .ne. 0) then
            if (modus(new).up_set .ne. 0) then
              call error_msg(ERR_PRIOR_UP,modus(new).up_set)
              return
            endif
            do k=1,16
              modus(new).up(k) = temp_modu.up(k)
            enddo
            modus(new).up_set = temp_modu.up_set
          endif
          if (temp_modu.ped_set .ne. 0) then
            if (modus(new).ped_set .ne. 0) then
              call error_msg(ERR_PRIOR_PED,modus(new).ped_set)
              return
            endif
            do k=1,16
              modus(new).ped(k) = temp_modu.ped(k)
            enddo
            modus(new).ped_set = temp_modu.ped_set
          endif
          if (temp_modu.off_set .ne. 0) then
            if (modus(new).off_set .ne. 0) then
              call error_msg(ERR_PRIOR_OFF,modus(new).off_set)
              return
            endif
            do k=1,16
              modus(new).off(k) = temp_modu.off(k)
            enddo
            modus(new).off_set = temp_modu.off_set
          endif
          if (temp_modu.com_set .ne. 0) then
            if (modus(new).com_set .ne. 0) then
              call error_msg(ERR_PRIOR_COM,modus(new).com_set)
              return
            endif
            modus(new).com = temp_modu.com
            modus(new).com_set = temp_modu.com_set
          endif
          if (temp_modu.enable_set .ne. 0) then
            if (modus(new).enable_set .ne. 0) then
              call error_msg(ERR_PRIOR_ENABLE,modus(new).enable_set)
              return
            endif
            do k=1,16
              modus(new).enable(k) = temp_modu.enable(k)
            enddo
            modus(new).enable_set = temp_modu.enable_set
          endif
          if (temp_modu.gen_set .ne. 0) then
            if (modus(new).gen_set .ne. 0) then
              call error_msg(ERR_PRIOR_GEN,modus(new).gen_set)
              return
            endif
            modus(new).gen_type = temp_modu.gen_type
            modus(new).gen_set = temp_modu.gen_set
          endif
          if (temp_modu.delay_set .ne. 0) then
            if (modus(new).delay_set .ne. 0) then
              call error_msg(ERR_PRIOR_DELAY,modus(new).delay_set)
              return
            endif
            do k=1,16
              modus(new).delay(k) = temp_modu.delay(k)
            enddo
            modus(new).delay_set = temp_modu.delay_set
          endif
        endif
      enddo   
      return
*
      end
*******************************************************************************
*******************************************************************************
      subroutine list_modu()
      implicit none
      include  'modu_setup.for'

      integer*4 i,j,strlen
      character*16  mesx
      character*16  mtype(17) /'phillips','silena','LRS_4300',
     1  'HHIRF_ADC','LRS_4508','LRS_4418','ue_clock',
     2  'phil_7106','LRS_4415A','LRS_3511','LRS_4413',
     3  'LRS_4416B','LRS_3377','SHAPER','CAEN_775','CAEN_785',
     4  'MCSQ'/

      integer*4 m_index
      record /modu_data/ modus(200),temp_modu
      common /modu/  m_index,modus

      do i=1,m_index
        write(2,9000) i
9000    format('************ module # ',i3)
        mesx = mtype(modus(i).mt_type)
        write(2,9010) modus(i).mt_type,'  module type  ',
     1                                     mesx(1:strlen(mesx))
9010    format(i,a,a)
        write(2,9010) modus(i).mt_line,'  module line #'
        write(2,9010) modus(i).mt_error,'  module error'
        write(2,9010) modus(i).c,'  module crate #'
        write(2,9010) modus(i).n,'  module slot #'
        if (modus(i).low_set .ne. 0) then
          write(2,9010) modus(i).low_set,'  low line # and values'
          write(2,9020) modus(i).low
9020      format(8i6)
        endif
        if (modus(i).up_set .ne. 0) then
          write(2,9010) modus(i).up_set,'  upper line # and values'
          write(2,9020) modus(i).up
        endif
        if (modus(i).ped_set .ne. 0) then
          write(2,9010) modus(i).ped_set,'  peds line # and values'
          write(2,9020) modus(i).ped
        endif
        if (modus(i).off_set .ne. 0) then
          write(2,9010) modus(i).off_set,'  offset line # and values'
          write(2,9020) modus(i).off
        endif
        if (modus(i).com_set .ne. 0) then
          write(2,9010) modus(i).com_set,'  common line # and value'
          write(2,9020) modus(i).com
        endif
        if (modus(i).enable_set .ne. 0) then
          write(2,9010) modus(i).enable_set,'  enable line # and values'
          write(2,9020) modus(i).enable
        endif
        if (modus(i).gen_set .ne. 0) then
          write(2,9010) modus(i).gen_set,'  gen line # and value'
          write(2,9020) modus(i).gen_type
        endif
        if (modus(i).delay_set .ne. 0) then
          write(2,9010) modus(i).delay_set,'  delay line # and values'
          write(2,9020) modus(i).delay
        endif
        if (modus(i).mode_set .ne. 0) then
          write(2,9010) modus(i).mode_set,'  mode line # and value'
          write(2,9020) modus(i).mode
        endif
        if (modus(i).ovfl_set .ne. 0) then
          write(2,9010) modus(i).ovfl_set,'  overflow line # and value'
          write(2,9020) modus(i).ovfl
        endif
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine inhibit(func)
      implicit none
      include  'modu_setup.for'

      character*(*)  func
      integer*4  i,j,stat
      integer*4  crates(0:7)
      integer*2  inhib(0:7)
      integer*4 m_index
      record /modu_data/ modus(200),temp_modu
      common /modu/  m_index,modus

      do i=0,7
        crates(i) = -1
      enddo
      do i=1,m_index
        if (modus(i).c .ne. -1) crates(modus(i).c) = 1
      enddo
      if (func .eq. 'save') then
        do i=0,7
          inhib(i) = -1
          if (crates(i) .ge. 0) then
            call camacio(0,i,30,0,1,inhib(i),1,stat)
            inhib(i) = iand(inhib(i),4)
            if (stat .ne. 0) call exit(99)
          endif
        enddo
        do i=0,7
          if (inhib(i) .eq. 0) call camacio(1,i,30,0,17,4,1,stat)
        enddo
      else
        do i=0,7
          if (inhib(i) .eq. 0) call camacio(1,i,30,0,17,0,1,stat)
        enddo
      endif
      end
*******************************************************************************
*******************************************************************************
      subroutine format_silena()
      implicit none
      include  'modu_setup.for'

      integer*4 i,j,k
      real*4    val

      integer*4 m_index
      record /modu_data/ modus(200),temp_modu
      common /modu/  m_index,modus

      do j=1,m_index
        if (modus(j).mt_type .eq. SILENA) then
          if (modus(j).low_set .ne. 0) then
            do k=1,8
              val = modus(j).low(k)
              val = val * 0.255 +0.5
              modus(j).low(k) = val
              if (modus(j).low(k) .lt. 0) modus(j).low(k) = 0
              if (modus(j).low(k) .gt. 255) modus(j).low(k) = 255
            enddo
          else
            do k=1,8
              modus(j).low(k) = 0
            enddo
            modus(j).low_set = modus(j).mt_line
          endif
          if (modus(j).up_set .ne. 0) then
            do k=1,8
              val = modus(j).up(k)
              val = val - 8500
              val = val * 0.17 + 0.5
              modus(j).up(k) = val
              if (modus(j).up(k) .lt. 0) modus(j).up(k) = 0
              if (modus(j).up(k) .gt. 255) modus(j).up(k) = 255
            enddo
          else
            do k=1,8
              modus(j).up(k) = 255
            enddo
            modus(j).up_set = modus(j).mt_line
          endif
          if (modus(j).off_set .ne. 0) then
            do k=1,8
              val = modus(j).off(k)
              val = val + 122.88
              val = (val * 255.0)/245.76 + 0.5
              modus(j).off(k) = val
              if (modus(j).off(k) .lt. 0) modus(j).off(k) = 0
              if (modus(j).off(k) .gt. 255) modus(j).off(k) = 255
            enddo
          else
            do k=1,8
              modus(j).off(k) = 128
            enddo
            modus(j).off_set = modus(j).mt_line
          endif
          if (modus(j).com_set .ne. 0) then
            val = modus(j).com
            val = val * 0.2125 + 0.5
            modus(j).com = val
            if (modus(j).com .lt. 0) modus(j).com = 0
            if (modus(j).com .gt. 255) modus(j).com = 255
          else
            modus(j).com = 21
            modus(j).com_set = modus(j).mt_line
          endif
        endif
      enddo   
      return
*
      end
*******************************************************************************
*******************************************************************************
      subroutine clear_silena(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,strlen
      integer*4     crates(0:7),slot(0:7)

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      integer*4     cc(400),cn(400),ca(400),cf(400),st(400)
      integer*2     rd(400),wr(400)
      common /camac/ cc,cn,ca,cf,rd,wr,st
*
      do i=0,7
        crates(i) = -1
      enddo
      do i=1,m_index
        if (modus(i).mt_type .eq. SILENA) then
          crates(modus(i).c) = 1
          slot(modus(i).c) = modus(i).n
        endif
      enddo
      do i=0,7
        if (crates(i) .gt. 0) then
          do j=1,12
            cc(j) = i
            cn(j) = slot(i)
            ca(j) = 0
            cf(j) = 0
            wr(j) = 0
          enddo
          cn(1) = 30
          cn(12) = 30
          cf(1) = 17
          cf(12) = 17
          wr(12) = 4
          call camlist(cc,cn,ca,cf,0,wr,12,st)
        endif
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine cam_error(c,n,a,f,stat)

      implicit none

      include 'cam_fast.for'
   
      integer*4 stat,c,n,a,f
      integer*4 i,strlen
      character*64 mess
      character*20 msg(10)/
     1 'GOOD X   BAD Q','GOOD Q   BAD X','BAD  Q   - BAD X',
     2 'Timeout',
     3 'Illegal Crate','Illegal Module','Illegal Address',
     4 'Illegal Function','Crate Off_Line','Unknown error code'/

*  
      if(stat.eq.0) return     !return if good
*
      if (stat .eq. NOQ) then
         i = 1
      elseif (stat .eq. NOX) then
         i = 2
      elseif (stat .eq. NOX_NOQ) then
         i = 3
      elseif (stat .eq. TIMEOUT) then
         i = 4
      elseif (stat .eq. ILL_CRATE) then
         i = 5
      elseif (stat .eq. ILL_MODULE) then
         i = 6
      elseif (stat .eq. ILL_ADDRESS) then
         i = 7
      elseif (stat .eq. ILL_FUNCTION) then
         i = 8
      elseif (stat .eq. CRATE_OFF_LINE) then
         i = 9
      else
         i = 10
      endif

      write(mess,10) c,n,a,f,msg(i)
10    format(' CAM ERROR-C,N,A,F=',4I3,'  'A)

20    write(*,9000) mess(1:strlen(mess))       !send error message
9000  format(a)
      return
      end
*******************************************************************************
*******************************************************************************
*
*
      subroutine CAMERR(ipb,stat)

      implicit none

      include 'cam_fast.for'

      integer*4 ipb(5),stat,c,n,a,f
      integer*4 i,strlen
      character*64 mess
      character*20 msg(10)/
     1 'GOOD X   BAD Q','GOOD Q   BAD X','BAD  Q   - BAD X',
     2 'Timeout',
     3 'Illegal Crate','Illegal Module','Illegal Address',
     4 'Illegal Function','Crate Off_Line','Unknown error code'/

*
      if(stat.eq.0) return     !return if good
*
*  X = 1 and Q = 0 is ignored in this error handler
*
      if(stat.eq.NOQ) return   !return if bad Q only
*
      call ilbyte(c,ipb,0)     !otherwise, pick up c,n,a,f
      call ilbyte(n,ipb,1)
      call ilbyte(a,ipb,2)
      call ilbyte(f,ipb,3)
*
      if (stat .eq. NOQ) then
         i = 1
      elseif (stat .eq. NOX) then
         i = 2
      elseif (stat .eq. NOX_NOQ) then
         i = 3
      elseif (stat .eq. TIMEOUT) then
         i = 4
      elseif (stat .eq. ILL_CRATE) then
         i = 5
      elseif (stat .eq. ILL_MODULE) then
         i = 6
      elseif (stat .eq. ILL_ADDRESS) then
         i = 7
      elseif (stat .eq. ILL_FUNCTION) then
         i = 8
      elseif (stat .eq. CRATE_OFF_LINE) then
         i = 9
      else
         i = 10
      endif

      write(mess,10) c,n,a,f,msg(i)
10    format(' CAM ERROR-C,N,A,F=',4I3,'  'A)

20    write(6,9000) mess(1:strlen(mess))       !send error message
9000  format(a)
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine verify_error(c,n,a,f,wr,rd)

      implicit none

      integer*4 c,n,a,f
      integer*2 wr,rd,diff

      diff = ieor(wr,rd)
      write(*,9000) c,n,a,f,wr,rd,diff
9000  format(' VERIFY ERROR-C,N,A,F=',4I3,'  WRITE,READ,DIFF=',3z6)
      return
      end
*******************************************************************************
*******************************************************************************
      subroutine load_phil(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen
      integer*2     mode_wd

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      integer*4     cc(400),cn(400),ca(400),cf(400),st(400)
      integer*2     rd(400),wr(400)
      common /camac/ cc,cn,ca,cf,rd,wr,st
*
      do i=1,m_index
        if (modus(i).mt_type .ne. PHIL) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup Phillips Module: C = ',i1,', N = ',i2)
        j = 1
        mode_wd = 0
        do k=1,400
          cc(k) = modus(i).c
          cn(k) = modus(i).n
          ca(k) = 0
          cf(k) = 20
        enddo
        cf(j) = 9
        j = j + 1
        if (modus(i).low_set .ne. 0) then
          ca(j) = 1
          cf(j) = 17
          wr(j) = 0
          j = j + 1
          do k=1,16
            ca(j) = k -1
            wr(j) = modus(i).low(k)
            j = j + 1
          enddo
          mode_wd = ior(mode_wd,2)
        endif
        if (modus(i).up_set .ne. 0) then
          ca(j) = 2
          cf(j) = 17
          wr(j) = 0
          j = j + 1
          do k=1,16
            ca(j) = k -1
            wr(j) = modus(i).up(k)
            j = j + 1
          enddo
          mode_wd = ior(mode_wd,4)
        endif
        if (modus(i).ped_set .ne. 0) then
          ca(j) = 0
          cf(j) = 17
          wr(j) = 0
          j = j + 1
          do k=1,16
            ca(j) = k -1
            wr(j) = iand(modus(i).ped(k),'1FFF'x)
            j = j + 1
          enddo
          mode_wd = ior(mode_wd,1)
        endif
        call camlist(cc,cn,ca,cf,0,wr,j-1,st)
        err_count = 5
        do k=1,j-1
          if (st(k) .ne. 0) then
            err = 1
            call cam_error(cc(k),cn(k),ca(k),cf(k),st(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
        do k = 1,j-1
          if (cf(k) .eq. 20) cf(k) = 1
        enddo
        err_count = 5
        call camlist(cc,cn,ca,cf,0,rd,j-1,st)
        do k=1,j-1
          if (st(k) .ne. 0) then
            err = 1
            call cam_error(cc(k),cn(k),ca(k),cf(k),st(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
        err_count = 5
        do k=1,j-1
          if (cf(k) .eq. 1) then
            if (rd(k) .ne. wr(k)) then
              err = 1
              call verify_error(cc(k),cn(k),ca(k),cf(k),wr(k),rd(k))
              err_count = err_count - 1
              if (err_count .le. 0) go to 100
            endif
          endif
        enddo
        cf(1) = 19
        ca(1) = 0
        wr(1) = mode_wd
        cf(2) = 6
        ca(2) = 0
        call camlist(cc,cn,ca,cf,0,wr,2,st)
        if (st(1) .ne. 0) then
          call cam_error(cc(1),cn(1),ca(1),cf(1),st(1))
        endif
        if (st(2) .ne. 0) then
          call cam_error(cc(2),cn(2),ca(2),cf(2),st(2))
        endif
        if (mode_wd .ne. iand(wr(2),15)) then
          err = 1
          call verify_error(cc(2),cn(2),ca(2),cf(2),wr(1),wr(2))
        endif
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_silena(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen
      integer*4     mode_wd

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      integer*4     cc(400),cn(400),ca(400),cf(400),st(400)
      integer*2     rd(400),wr(400)
      common /camac/ cc,cn,ca,cf,rd,wr,st
*
      do i=1,m_index
        if (modus(i).mt_type .ne. SILENA) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup Silena Module: C = ',i1,', N = ',i2)
        if (modus(i).mode_set .ne. 0) then
          mode_wd = iand(modus(i).mode,'7EFF'x)
        else
          mode_wd = 'A00'x
        endif
        if (modus(i).ovfl_set .ne. 0) then
          if (modus(i).ovfl .ne. 0) then
            mode_wd = iand(mode_wd,'76ff'x)
          else
            mode_wd = ior(mode_wd,'800'x)
          endif
        endif
        j = 1
        do k=1,400
          cc(k) = modus(i).c
          cn(k) = modus(i).n
          ca(k) = 0
        enddo
        cf(j) = 9
        j = j + 1
        if (modus(i).low_set .ne. 0) then
          do k=1,8
            ca(j) = k + 7
            cf(j) = 17
            wr(j) = modus(i).low(k)
            j = j + 1
          enddo
        endif
        if (modus(i).up_set .ne. 0) then
          do k=1,8
            ca(j) = k -1
            cf(j) = 17
            wr(j) = modus(i).up(k)
            j = j + 1
          enddo
        endif
        if (modus(i).off_set .ne. 0) then
          do k=1,8
            ca(j) = k -1
            cf(j) = 20
            wr(j) = modus(i).off(k)
            j = j + 1
          enddo
        endif
        if (modus(i).com_set .ne. 0) then
          ca(j) = 9
          cf(j) = 20
          wr(j) = modus(i).com
          j = j + 1
        endif
        call camlist(cc,cn,ca,cf,0,wr,j-1,st)
        err_count = 5
        do k=1,j-1
          if (st(k) .ne. 0) then
            err = 1
            call cam_error(cc(k),cn(k),ca(k),cf(k),st(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
        do k = 1,j-1
          if (cf(k) .eq. 20) cf(k) = 4
          if (cf(k) .eq. 17) cf(k) = 1
        enddo
        err_count = 5
        call camlist(cc,cn,ca,cf,0,rd,j-1,st)
        do k=1,j-1
          if (st(k) .ne. 0) then
            err = 1
            call cam_error(cc(k),cn(k),ca(k),cf(k),st(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
        err_count = 500
        do k=1,j-1
          if (cf(k) .eq. 1 .or. cf(k) .eq. 4) then
            if (rd(k) .ne. wr(k)) then
              err = 1
              call verify_error(cc(k),cn(k),ca(k),cf(k),wr(k),rd(k))
              err_count = err_count - 1
              if (err_count .le. 0) go to 100
            endif
          endif
        enddo
        cf(1) = 20
        ca(1) = 14
        wr(1) = mode_wd
        cf(2) = 4
        ca(2) = 14
        call camlist(cc,cn,ca,cf,0,wr,2,st)
        if (st(1) .ne. 0) then
          call cam_error(cc(1),cn(1),ca(1),cf(1),st(1))
        endif
        if (st(2) .ne. 0) then
          call cam_error(cc(2),cn(2),ca(2),cf(2),st(2))
        endif
        if (mode_wd .ne. iand(wr(2),'feff'x)) then
          err = 1
          call verify_error(cc(2),cn(2),ca(2),cf(2),wr(1),wr(2))
        endif
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_hhirf_adc(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen
      integer*2     enables,mask
      integer*4     c,n,stat

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      do i=1,m_index
        if (modus(i).mt_type .ne. HHIRF_ADC) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup HHIRF_ADC Module: C = ',i1,', N = ',i2)
        c = modus(i).c
        n = modus(i).n
        call camacio(0,c,n,0,9,0,1,stat)
        if (stat .ne. 0) err = 1
        enables = 0
        mask = 1
        if (modus(i).enable_set .ne. 0) then
          do k=1,16
            if (modus(i).enable(k) .ne. 0) enables = ior(enables,mask)
            mask = mask * 2
          enddo
        endif
        call camacio(0,c,n,0,17,enables,1,stat)
        if (stat .ne. 0) err = 1
        call camacio(0,c,n,0,9,0,1,stat)
        if (stat .ne. 0) err = 1
        call camacio(0,c,n,0,9,0,1,stat)
        if (stat .ne. 0) err = 1
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_ue_clock(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen
      integer*2     mode_wd,temp
      integer*4     c,n,stat

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      do i=1,m_index
        if (modus(i).mt_type .ne. UE_CLOCK) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup UE_clock Module: C = ',i1,', N = ',i2)
        c = modus(i).c
        n = modus(i).n
*
*   Clear data register and R2 of the control register
*
        call camacio(0,c,n,0,9,0,1,stat)
        if (stat .ne. 0) err = 1
*
*   If mode was not specified, the default is Mode 1
*
        if (modus(i).mode_set .ne. 0) then
          mode_wd = modus(i).mode
        else
          mode_wd = 1
        endif
        call camacio(0,c,n,0,17,mode_wd,1,stat)
        if (stat .ne. 0) err = 1
        call camacio(0,c,n,0,1,temp,1,stat)
        if (stat .ne. 0) err = 1
        temp = iand(temp,1)
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_lrs4300(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen
      integer*4     mode_wd

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      integer*4     cc(400),cn(400),ca(400),cf(400),st(400)
      integer*2     rd(400),wr(400)
      common /camac/ cc,cn,ca,cf,rd,wr,st
*
      do i=1,m_index
        if (modus(i).mt_type .ne. LRS_4300) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup LRS_4300 Module: C = ',i1,', N = ',i2)
        if (modus(i).mode_set .ne. 0) then
          mode_wd = modus(i).mode
        else
          mode_wd = 0
        endif
        j = 1
        do k=1,400
          cc(k) = modus(i).c
          cn(k) = modus(i).n
          ca(k) = 0
        enddo
        cf(j) = 9
        j = j + 1
        if (modus(i).ped_set .ne. 0) then
          do k=1,16
            ca(j) = k -1
            cf(j) = 17
            wr(j) = modus(i).ped(k)
            j = j + 1
          enddo
          mode_wd = ior(mode_wd,'900'x)   !turn on pedestal subtraction
        else
          mode_wd = iand(mode_wd, 'f6ff'x) !turn off pedestal subtraction
        endif
        if (modus(i).ovfl_set .ne. 0) then
          mode_wd = iand(mode_wd,'7fff'x)
          if (modus(i).ovfl .ne. 0) mode_wd = ior(mode_wd,'8000'x)
        endif
        call camlist(cc,cn,ca,cf,0,wr,j-1,st)
        err_count = 5
        do k=1,j-1
          if (st(k) .ne. 0) then
            err = 1
            call cam_error(cc(k),cn(k),ca(k),cf(k),st(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
        do k = 1,j-1
          if (cf(k) .eq. 17) cf(k) = 1
        enddo
        err_count = 5
        call camlist(cc,cn,ca,cf,0,rd,j-1,st)
        do k=1,j-1
          if (st(k) .ne. 0) then
            err = 1
            call cam_error(cc(k),cn(k),ca(k),cf(k),st(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
        err_count = 5
        do k=1,j-1
          if (cf(k) .eq. 1) then
            if (rd(k) .ne. wr(k)) then
              err = 1
              call verify_error(cc(k),cn(k),ca(k),cf(k),wr(k),rd(k))
              err_count = err_count - 1
              if (err_count .le. 0) go to 100
            endif
          endif
        enddo
        cf(1) = 16
        ca(1) = 0
        wr(1) = mode_wd
        cf(2) = 0
        ca(2) = 0
        call camlist(cc,cn,ca,cf,0,wr,2,st)
        if (wr(1) .ne. wr(2)) then
          err = 1
          call verify_error(cc(2),cn(2),ca(2),cf(2),wr(1),wr(2))
        endif
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_lrs4418(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      integer*4     cc(400),cn(400),ca(400),cf(400),st(400)
      integer*2     rd(400),wr(400)
      common /camac/ cc,cn,ca,cf,rd,wr,st
*
      do i=1,m_index
        if (modus(i).mt_type .ne. LRS_4418) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup LRS_4418 Module: C = ',i1,', N = ',i2)
        j = 1
        do k=1,400
          cc(k) = modus(i).c
          cn(k) = modus(i).n
          ca(k) = 0
        enddo
        if (modus(i).delay_set .ne. 0) then
          do k=1,16
            ca(j) = k -1
            cf(j) = 16
            wr(j) = modus(i).delay(k)/8
            if (wr(j) .gt. 15) wr(j) = 15
            j = j + 1
          enddo
        else
          do k=1,16
            ca(j) = k -1
            cf(j) = 16
            wr(j) = 0
            j = j + 1
          enddo
        endif
        call camlist(cc,cn,ca,cf,0,wr,j-1,st)
        err_count = 5
        do k=1,j-1
          if (st(k) .ne. 0) then
            err = 1
            call cam_error(cc(k),cn(k),ca(k),cf(k),st(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_phil7106(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen
      integer*4     fmode,mask,ena_wd

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      integer*4     cc(400),cn(400),ca(400),cf(400),st(400)
      integer*2     rd(400),wr(400)
      common /camac/ cc,cn,ca,cf,rd,wr,st
*
      do i=1,m_index
        if (modus(i).mt_type .ne. PHIL_7106) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup phil_7106 Module: C = ',i1,', N = ',i2)
        j = 1
        do k=1,32
          cc(k) = modus(i).c
          cn(k) = modus(i).n
        enddo
        if (modus(i).com_set .ne. 0) then
          ca(j) = 0
          cf(j) = 17
          wr(j) = modus(i).com              !set threshold DAC
          j = j + 1
          ca(j) = 0
          cf(j) = 26                        !set mode to Remote
          j = j + 1
          fmode = 0
        else
          ca(j) = 0
          cf(j) = 24                        !set mode to Local
          j = j + 1
          fmode = '8000'x
        endif
        if (modus(i).enable_set .ne. 0) then
          mask = 1
          ena_wd = 0
          do k=1,16
            if (modus(i).enable(k) .ne. 0) ena_wd = ior(ena_wd,mask)
            mask = mask * 2
          enddo
        else
          ena_wd = 'FFFF'x
        endif
        ca(j) = 0
        cf(j) = 16
        wr(j) = ena_wd                      !set Mask register
        j = j + 1
        ca(j) = 1
        cf(j) = 17                          !start threshold ADC conv
        wr(j) = 0
        j = j - 1
        do k=1,j
        enddo
        call camlist(cc,cn,ca,cf,0,wr,j,st)
        err_count = 5
        do k=1,j
          if (st(k) .ne. 0) then
            err = 1
            call cam_error(cc(k),cn(k),ca(k),cf(k),st(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
        ca(1) = 1
        cf(1) = 17                          !start threshold ADC conv
        wr(1) = 0
        j = j - 1
        call camlist(cc,cn,ca,cf,0,wr,1,st)
        if (st(1) .ne. 0) then
          call cam_error(cc(1),cn(1),ca(1),cf(1),st(1))
          err_count = err_count - 1
        endif
        ca(1) = 1
        cf(1) = 1                           !read threshold and mode
        call camlist(cc,cn,ca,cf,0,rd,1,st)
        if (st(1) .ne. 0) then
          call cam_error(cc(1),cn(1),ca(1),cf(1),st(1))
          err_count = err_count - 1
        endif
        mask = rd(1)
        mask = iand(mask,'8000'x)
        if (mask .ne. fmode) then
          if (mask .eq. 0) then
            write(*,9010)
9010        format('  MODE ERROR: Should be Local but is Remote')
          else
            write(*,9020)
9020        format('  MODE ERROR: Should be Remote but is Local')
          endif
        endif
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_lrs4415a(err)
      implicit none
      include  'modu_setup.for'
      include  'cam_fast.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen
      integer*2     enables,mask,imode
      integer*4     c,n,stat

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      do i=1,m_index
        if (modus(i).mt_type .ne. LRS_4415A) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup lrs_4415a Module: C = ',i1,', N = ',i2)
        c = modus(i).c
        n = modus(i).n
        enables = 0
        mask = 1
        if (modus(i).enable_set .ne. 0) then
          do k=1,16
            if (modus(i).enable(k) .eq. 0) enables = ior(enables,mask)
            mask = mask * 2
          enddo
        endif
        imode = 0
        if (modus(i).mode_set .ne. 0) then
          if (modus(i).mode .ne. 0) imode = 1
        endif
        call camacio(0,c,n,0,17,imode,1,stat)     !set mode
        if (stat .ne. 0) err = 1
        call camacio(0,c,n,0,16,enables,1,stat)   !write mask register
        if (stat .ne. 0) err = 1
        call camacio(0,c,n,0,27,enables,1,stat)   !test mode switch
        if (stat .eq. NOQ) then
          write(*,9010) modus(i).c,modus(i).n
9010      format(' ** WARNING ** lrs_4415a Module: C = ',i1,', N = '
     1       i2,' is in LOCAL mode')
        else if (stat .ne. 0) then
          err = 1
        endif
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_lrs3511(err)
      implicit none
      include  'modu_setup.for'
      include  'cam_fast.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen
      integer*2     imode,temp
      integer*4     c,n,stat
      integer*4     gain(6) /8192,4096,2048,1024,512,256/

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      do i=1,m_index
        if (modus(i).mt_type .ne. LRS_3511) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup lrs_3511 Module: C = ',i1,', N = ',i2)
        c = modus(i).c
        n = modus(i).n
        imode = 0
        if (modus(i).mode_set .ne. 0) then
          imode = ior(imode,modus(i).mode)
        else
          imode = ior(imode,'100'x)
        endif
        if (modus(i).off_set .ne. 0) then
          temp = gain(imode/256)
          temp = (modus(i).off(1)/temp)*temp
          imode = ior(imode,temp/256)
        endif
        call camacio(0,c,n,0,16,imode,1,stat)     !set mode
        if (stat .ne. 0) err = 1
        call camacio(0,c,n,0,26,0,0,stat) !Some 3511s need to be enabled
        if (stat .ne. 0) err = 1
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_lrs4413(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen,local
      integer*4     mask,ena_wd

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      integer*4     cc(400),cn(400),ca(400),cf(400),st(400)
      integer*2     rd(400),wr(400)
      common /camac/ cc,cn,ca,cf,rd,wr,st
*
      do i=1,m_index
        if (modus(i).mt_type .ne. LRS_4413) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup lrs_4413 Module: C = ',i1,', N = ',i2)
        local = 1
        j = 1
        do k=1,32
          cc(k) = modus(i).c
          cn(k) = modus(i).n
        enddo
        ca(j) = 0
        cf(j) = 26            !set remote mode
        j = j + 1
        if (modus(i).com_set .ne. 0) then
          local = 0
          if (modus(i).com .eq. 0) then
            wr(j) = 1024
          else
            wr(j) = modus(i).com
          endif
        else
          wr(j) = 1024
        endif
        ca(j) = 0
        cf(j) = 17
        j = j + 1
        if (modus(i).enable_set .ne. 0) then
          local = 0
          mask = 1
          ena_wd = 0
          do k=1,16
            if (modus(i).enable(k) .eq. 0) ena_wd = ior(ena_wd,mask)
            mask = mask * 2
          enddo
        else
          ena_wd = 0
        endif
        ca(j) = 0
        cf(j) = 16
        wr(j) = ena_wd                      !set Mask register
        do k=1,j
        enddo
        call camlist(cc,cn,ca,cf,0,wr,j,st)
        err_count = 5
        do k=1,j
          if (st(k) .ne. 0) then
            err = 1
            call cam_error(cc(k),cn(k),ca(k),cf(k),st(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
        j = 1
        ca(j) = 0
        cf(j) = 1                           !read threshold
        j = j + 1
        ca(j) = 0
        cf(j) = 0                           !read mask
        j = j + 1
        if (local .eq. 1) then
          ca(j) = 0
          cf(j) = 24
          j = j + 1
        endif
        err_count = 5
        call camlist(cc,cn,ca,cf,0,rd,j-1,st)
        if (st(1) .ne. 0) then
          call cam_error(cc(1),cn(1),ca(1),cf(1),st(1))
          err_count = err_count - 1
        endif
        do k=1,2
          if (rd(k) .ne. wr(k)) then
            err = 1
            call verify_error(cc(k),cn(k),ca(k),cf(k),wr(k),rd(k))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_lrs4416b(err)
      implicit none
      include  'modu_setup.for'
      include  'cam_fast.for'

      integer*4     err,err_count
      integer*4     i,j,k,strlen
      integer*2     enables,mask,temp,thresh
      integer*4     c,n,stat

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      do i=1,m_index
        if (modus(i).mt_type .ne. LRS_4416B) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup lrs_4416b Module: C = ',i1,', N = ',i2)
        c = modus(i).c
        n = modus(i).n
        enables = 0
        mask = 1
        if (modus(i).enable_set .ne. 0) then
          do k=1,16
            if (modus(i).enable(k) .eq. 0) enables = ior(enables,mask)
            mask = mask * 2
          enddo
        endif
        call camacio(0,c,n,0,16,enables,1,stat)   !write mask register
        if (stat .ne. 0) err = 1
        call camacio(0,c,n,0,1,temp,1,stat)       !read mask register
        if (stat .ne. 0) err = 1
        if (enables .ne. temp) then
          call verify_error(c,n,0,1,enables,temp)
          err = 1
        endif
        if (modus(i).com_set .ne. 0) then
          if (modus(i).com .eq. 0) then
            thresh = 1024
          else
            thresh = modus(i).com
          endif
        else
          thresh = 1024
        endif
        call camacio(0,c,n,0,17,thresh,1,stat)   !write threshold
        if (stat .ne. 0) err = 1
        call camacio(0,c,n,0,27,enables,1,stat)   !test mode switch
        if (stat .eq. NOQ) then
          write(*,9010) modus(i).c,modus(i).n
9010      format(' ** WARNING ** lrs_4416b Module: C = ',i1,', N = '
     1       i2,' is in LOCAL mode')
        else if (stat .ne. 0) then
          err = 1
        endif
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_lrs3377(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,strlen

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      integer*4     cc(400),cn(400),ca(400),cf(400),st(400)
      integer*2     rd(400),wr(400)
      common /camac/ cc,cn,ca,cf,rd,wr,st
*
      do i=1,m_index
        if (modus(i).mt_type .ne. LRS_3377) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup LeCroy 3377 Module: C = ',i1,', N = ',i2)
        j = 1
        do k=1,400
          cc(k) = modus(i).c
          cn(k) = modus(i).n
          ca(k) = 0
          cf(k) = 0
        enddo
        cf(j) = 9
        j = j + 1
        cf(j) = 30
        j = j + 1
        if (modus(i).com_set .eq. 0) modus(i).com = 0
        if (modus(i).com .eq. 0) then
          cf(j) = 23           !Mode 3, common start, double word mode
        else
          cf(j) = 22           !Mode 2, common stop, double word mode
        endif
        j = j + 1
        cf(j) = 25
        j = j + 1
        call camlist(cc,cn,ca,cf,0,wr,j-1,st)
        call sleep(2)
        j = 1
        cf(j) = 9
        j = j + 1
        cf(j) = 24
        ca(j) = 1
        j = j + 1
        call camlist(cc,cn,ca,cf,0,wr,j-1,st)
        j = 1
        if (modus(i).up_set .eq. 0) modus(i).up(1) = 32767
        if (modus(i).com .eq. 0) then
           cf(j) = 17           !Common start mode
           ca(j) = 0
           wr(j) = 'e000'x !rising edge only, single buffer, skip header
           j = j + 1
           cf(j) = 17
           ca(j) = 1
           wr(j) = 0            !no MPI
           j = j + 1
           cf(j) = 17
           ca(j) = 2
           wr(j) = 0            !Max 16 hits per channel
           j = j + 1
           cf(j) = 17
           ca(j) = 3
           if (modus(i).up(1) .ge. 10000) then  !compute request dlytime
             wr(j) = 0
           else
             wr(j) = 10 - modus(i).up(1)/1000
             wr(j) = wr(j)/2 + 1
           endif
           j = j + 1
           cf(j) = 17
           ca(j) = 4
           wr(j) = modus(i).up(1)/50
           j = j + 1
           cf(j) = 17
           ca(j) = 5
           wr(j) = 0            !test mode disabled
           j = j + 1
        else
           cf(j) = 17           !Common stop mode
           ca(j) = 0
           wr(j) = 'a000'x !rising edge only, single buffer, skip header
           j = j + 1
           cf(j) = 17
           ca(j) = 1
           wr(j) = 2            !trigger outputs width 50 nanoseconds
           j = j + 1
           cf(j) = 17
           ca(j) = 2
           wr(j) = (modus(i).up(1)/8)*16
           j = j + 1
           cf(j) = 17
           ca(j) = 3
           if (modus(i).up(1) .ge. 10000) then  !compute request dlytime
             wr(j) = 0
           else
             wr(j) = 10 - modus(i).up(1)/1000
             wr(j) = wr(j)/2 + 1
           endif
           j = j + 1
        endif
        cf(j) = 26
        ca(j) = 1
        j = j + 1
        call camlist(cc,cn,ca,cf,0,wr,j-1,st)
        do k=1,j-2
          cf(k) = 1
        enddo
        call camlist(cc,cn,ca,cf,0,rd,j-2,st)
        rd(2) = iand(rd(2),'1fff'x)
        if (modus(i).com .eq. 0) rd(3) = iand(rd(3),'000f'x)
        do k=1,j-2
          if (rd(k) .ne. wr(k)) then
            err = 1
            call verify_error(cc(k),cn(k),ca(k),cf(k),wr(k),rd(k))
          endif
        enddo
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_shaper(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,k,l,strlen
      integer*4     value,dat

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      integer*4     cc(400),cn(400),ca(400),cf(400),st(400)
      integer*2     rd(400),wr(400)
      common /camac/ cc,cn,ca,cf,rd,wr,st
*
      do i=1,m_index
        if (modus(i).mt_type .ne. SHAPER) go to 100
        write(*,9000) modus(i).c,modus(i).n
9000    format(' $$ Setup Shaper Module: C = ',i1,', N = ',i2)
        if (modus(i).ped_set .eq. 0) then
          do k=1,16
            modus(i).ped(k) = 255
          enddo
        endif
        do k = 1,400
          cc(k) = modus(i).c
          cn(k) = modus(i).n
          ca(k) = 1
          cf(k) = 16
        enddo
        k = 1
        do l = 16,1,-2
          wr(k) = 4
          k = k + 1
          wr(k) = 6
          k = k + 1
          wr(k) = 4
          k = k + 1
          value = modus(i).ped(l)
          do j = 1,8
            if (iand(value,'80'x) .eq. 0) then
              dat = 4
            else
              dat = 5
            endif
            wr(k) = dat
            k = k + 1
            wr(k) = dat + 2
            k = k + 1
            value = value * 2
          enddo
          value = modus(i).ped(l-1)
          do j = 1,8
            if (iand(value,'80'x) .eq. 0) then
              dat = 4
            else
              dat = 5
            endif
            wr(k) = dat
            k = k + 1
            wr(k) = dat + 2
            k = k + 1
            value = value * 2
          enddo
        enddo
        wr(k) = 0
****      do j=1,k
****        write (*,9010) cc(j),cn(j),ca(j),cf(j),wr(j)
****9010    format(' cnaf = ',4i3,' data = ',i4)
****      enddo
****      type 8000,k
****8000  format(' Num CAMAC operations = ',i4)
        call camlist(cc,cn,ca,cf,0,wr,k-1,st)
        err_count = 5
        do j=1,k
          if (st(j) .ne. 0) then
            err = 1
            call cam_error(cc(j),cn(j),ca(j),cf(j),st(j))
            err_count = err_count - 1
            if (err_count .le. 0) go to 100
          endif
        enddo
100     continue
      enddo
      end

*******************************************************************************
*******************************************************************************
      subroutine load_caen775(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,strlen
      integer*4     temp(32),range,wmode,xrange,xmode
      real*4        a,b,ftmp
      character*80  errmsg

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

*
      a = 35660.3774
      b = 0.2830
      do i=1,m_index
        if (modus(i).mt_type .ne. CAEN_775) go to 100
        write(*,9000) modus(i).n
9000    format(' $$ Setup CAEN 775 VME TDC Module: Num = ',i2)
        if (modus(i).com_set .eq. 0) modus(i).com = 1
        if (modus(i).low_set .eq. 0) then
          do j=1,32
            modus(i).low(j) = 0
          enddo
        endif
        wmode = modus(i).com
        if (modus(i).up_set .eq. 0) modus(i).up(1) = 1200
        ftmp = modus(i).up(1)
        ftmp = a/ftmp + b
        range = nint(ftmp)
****          write(*,*) modus(i).up(1),a,b,ftmp,range
        do j=1,32
          ftmp = modus(i).low(j)
          ftmp = ftmp/16.0
          modus(i).low(j) = nint(ftmp)
        enddo
        call caen_tdc_write(modus(i).n,modus(i).low,range,wmode,err)
        if (err .ne. 0) then
          call caen_error(err,errmsg)
          write(*,*) errmsg(1:strlen(errmsg))
****          return
        endif
        call caen_tdc_read(modus(i).n,temp,xrange,xmode,err)
        if (err .ne. 0) then
          call caen_error(err,errmsg)
          write(*,*) errmsg(1:strlen(errmsg))
****          return
        endif
        if (xrange .ne. range) write(*,9010) modus(i).n,range,xrange
9010    format(' CAEN TDC Num = ',i2,' range: wrote = ',i3,
     1         ' read = ',i3)
        if (xmode .ne. wmode) then
          err = 1
          if (wmode .ne. 0) then
            write(*,9020) modus(i).n
9020        format(' CAEN TDC Num = ',i2,' wrote common stop -- ',
     1             ' read common start')
          else
            write(*,9030) modus(i).n
9030        format(' CAEN TDC Num = ',i2,' wrote common start -- ',
     1             ' read common stop')

          endif
        endif
        err_count = 5
        do j=1,32
          if (modus(i).low(j) .ne. temp(j)) then
            err = 1
            write(*,9040) modus(i).n,j,modus(i).low(j),temp(j)
9040        format(' CAEN TDC Num = ',i2,' Channel = ',i4, ' wrote = ',
     1             i4,' read = ',i4)
            err_count = err_count -1
            if (err_count .le. 0) go to 100
          endif
        enddo
100     continue
      enddo
      end

*******************************************************************************
*******************************************************************************
      subroutine load_caen785(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err,err_count
      integer*4     i,j,strlen
      integer*4     temp(32)
      real*4        ftmp
      character*80  errmsg

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

*
      do i=1,m_index
        if (modus(i).mt_type .ne. CAEN_785) go to 100
        write(*,9000) modus(i).n
9000    format(' $$ Setup CAEN 785 VME ADC Module: Num = ',i2)
        if (modus(i).low_set .eq. 0) then
          do j=1,32
            modus(i).low(j) = 0
          enddo
        endif
        do j=1,32
          ftmp = modus(i).low(j)
          ftmp = ftmp/16.0
          modus(i).low(j) = nint(ftmp)
        enddo
        call caen_adc_write(modus(i).n,modus(i).low,err)
        if (err .ne. 0) then
          call caen_error(err,errmsg)
          write(*,*) errmsg(1:strlen(errmsg))
****          return
        endif
        call caen_adc_read(modus(i).n,temp,err)
        if (err .ne. 0) then
          call caen_error(err,errmsg)
          write(*,*) errmsg(1:strlen(errmsg))
****          return
        endif
        err_count = 5
        do j=1,32
          if (modus(i).low(j) .ne. temp(j)) then
            err = 1
            write(*,9040) modus(i).n,j,modus(i).low(j),temp(j)
9040        format(' CAEN ADC Num = ',i2,' Channel = ',i4, ' wrote = ',
     1             i4,' read = ',i4)
            err_count = err_count -1
            if (err_count .le. 0) go to 100
          endif
        enddo
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      subroutine load_lrs4508(err)
      implicit none
      include  'modu_setup.for'

      integer*4     err
      integer*4     i,j,k,strlen
      integer*4     c,n
      character*4   cfunc

      integer*4     m_index
      record /modu_data/ modus(200)
      common /modu/  m_index,modus

      do i=1,m_index
        if (modus(i).mt_type .ne. LRS_4508) go to 100
        c = modus(i).c
        n = modus(i).n

        if (modus(i).gen_set .ne. 0) then
          if (modus(i).gen_type .eq. GE_ONLY) then
            cfunc = 'GE'
          else if (modus(i).gen_type .eq. BGO_ONLY) then
            cfunc = 'BGO'
          else if (modus(i).gen_type .eq. ANTI) then
            cfunc = 'ANTI'
          endif
        else
          cfunc = 'ANTI'
        endif
        call m4508(c,n,cfunc)
100     continue
      enddo
      end
*******************************************************************************
*******************************************************************************
      SUBROUTINE m4508(c,n,cfunc)
      implicit none

      integer*4 c,n
      character*4 cfunc
      integer*4 addr,b,dat,func,erro,l,status
      integer*4 array(256),iarray(256)
      character*4 function(3)
      data function/'GE','BGO','ANTI'/
*
      b = 0
      type 9000,c,n,cfunc
      do 100 func=1,3
      if (cfunc .eq. function(func)) go to 110
100   continue
*
      type 9010,cfunc
      return
*
110   call gen(array,func,erro)
      if (erro .ne. 0) return
      call camacio(0,c,n,2,9,0,0,status)        !CLEAR MEM ADDR
      if (iand(status,'FFFF'X) .ne. 0) then
         write(*,9020) status
         return
      endif
      call camacio(1,c,n,0,18,array,256,status) !LOAD MEM
      if (iand(status,'FFFF'X) .ne. 0) then
         write(*,9030)status
         return
      endif
      call camacio(0,c,n,2,9,0,0,status)        !CLEAR MEM ADDR
      if (iand(status,'FFFF'X) .ne. 0) then
         write(*,9020) status
         return
      endif
      call camacio(1,c,n,2,2,iarray,256,status) !READ MEM
      if(iand(status,'FFFF'X) .ne. 0) then
         write(*,9031) status
         return
      endif
*
      do 120 l=0,255
      addr=iarray(L+1)/256
      if (addr .ne. l) type 9060,l,addr
      dat=iand(iarray(l+1),'FF'X)
      if (dat .ne. array(l+1)) type 9070,addr,array(l+1),iarray(l+1)
120   continue
*
      if (func .EQ. 4) call gen(array,func,erro)
      if (erro .ne. 0) return
      call camacio(0,c,n,3,9,0,0,status)        !CLEAR MEM ADDR
      if (iand(status,'FFFF'X) .ne. 0) then
         write(*,9020) status
         return
      endif
      call camacio(1,c,n,1,18,array,256,status) !LOAD MEM
      if (iand(status,'FFFF'X) .ne. 0) then
         write(*,9032)status
         return
      endif
      call camacio(b,c,n,3,9,0,0,status)        !CLEAR MEM ADDR
      if (iand(status,'ffff'x) .ne. 0) then
         write(*,9020) status
         return
      endif
      call camacio(1,c,n,3,2,iarray,256,status) !READ MEM
      if (iand(status,'FFFF'X) .ne. 0) then
         write(*,9033) status
         return
      endif
*
      do 130 l=0,255
      addr=iarray(l+1)/256
      if (addr .ne. l) type 9060,l,addr
      dat=iand(iarray(l+1),'FF'X)
      if (dat .ne. array(l+1)) type 9070,addr,array(l+1),iarray(l+1)
130   continue
      type 9080
*
9000  FORMAT(" Loading LeCroy 4508 at C=",I2," N=",I2," Function=",1XA4)
9010  FORMAT ("    4508 Function Specification Error - '",A4,"'")
9020  FORMAT ("    ABORTING ON CAMAC ERROR - F(9)A(2) - STATUS",Z9)
9030  FORMAT ("    ABORTING ON CAMAC ERROR - F(18)A(0) - STATUS",Z9)
9031  FORMAT ("    ABORTING ON CAMAC ERROR - F(2)A(2) - STATUS",Z9)
9032  FORMAT ("    ABORTING ON CAMAC ERROR - F(18)A(1) - STATUS",Z9)
9033  FORMAT ("    ABORTING ON CAMAC ERROR - F(2)A(3) - STATUS",Z9)
9060  FORMAT (" ADDRESS ERROR: EXPECTED ADDR=",I3," READ ADDR=",I3)
9070  FORMAT (" DATA COMPARE ERROR: ADDR=",I3," WROTE ",Z2," READ ",Z4)
9080  FORMAT (" *** LeCroy 4508 loaded and verified ***")
      END
*******************************************************************************
*******************************************************************************
      subroutine gen(a,func,erro)
      implicit  none

      integer*4 func,erro
      integer*4 k,l,m
*
      integer*4 A(*),IN(8),OUT(8)
*
      do 140 m=0,255
      l=1
      do 120 k=1,8
      in(k)=iand(m,l)/l
120   l=l*2
      goto (121,122,123) func
      pause " FUNCTION SPECIFICATION ERROR 'GEN'"
*
121   call lige(in,out)
      go to 125
*
122   call lbgo(in,out)
      go to 125
*
123   call lanti(in,out)
      go to 125
*
125   l=1
      a(m+1)=0
      do 130 k=1,8
      a(m+1)=a(m+1) + iand(out(k),1)*l
130   l=l*2
140   continue
      end
*******************************************************************************
*******************************************************************************
      subroutine lanti(in,out)
      implicit  none
*
      integer*4 in(*),out(*)
*
      out(1)=iand(not(in(1)),in(5))
      out(2)=iand(not(in(2)),in(6))
      out(3)=iand(not(in(3)),in(7))
      out(4)=iand(not(in(4)),in(8))
      out(5)=iand(in(1),in(5))
      out(6)=iand(in(2),in(6))
      out(7)=iand(in(3),in(7))
      out(8)=iand(in(4),in(8))
      end
*******************************************************************************
*******************************************************************************
      subroutine lbgo(in,out)
      implicit  none
*
      integer*4 in(*),out(*)
*
      out(1)=in(1)
      out(2)=in(2)
      out(3)=in(3)
      out(4)=in(4)
      out(5)=0
      out(6)=0
      out(7)=0
      out(8)=0
      end
*******************************************************************************
*******************************************************************************
      subroutine lige(in,out)
      implicit  none
*
      integer*4 in(*),out(*)
*
      out(1)=in(5)
      out(2)=in(6)
      out(3)=in(7)
      out(4)=in(8)
      out(5)=0
      out(6)=0
      out(7)=0
      out(8)=0
      end
