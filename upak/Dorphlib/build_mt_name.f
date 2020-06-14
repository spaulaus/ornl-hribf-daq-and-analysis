C$PROG BUILD_MT_NAME - Builds mag-tape device name
C
C     ******************************************************************
C     BY J.R. Beene AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
       subroutine build_mt_name(cin,cout,length)
c
       implicit integer (a-z)
       character*(*) cin
       character*32 cout,temp1
       character*32 mt_default
       character*1 suffix(3),unit
       character*6 prefix
       logical tsuffix,tunit,tprefix,tnorew,tulast
       data suffix/'h','m','l'/
       data prefix/'/dev/n'/
       data mt_default/'/dev/nrmt0h'/,iwhole/11/,iunit/10/
       data iprefix/1/,isuffix/11/
C
       SAVE
C
C     ------------------------------------------------------------------
c      JRB Routine to build valid ULTRIX device name for tape unit.
c      o   Returns length=-1 if error
c      o   Returns length=length otherwise
C      o   The routine features name completion. The minimum
c        allowed name is a one digit integer. If the density suffix
c        (l,m or h) is not present "h" is assumed. Leading characters
c        preceeding the integer may be omitted, but if any of the path
c        is present (/dev/) the full name through the unit number must
c        present.
c      o    Sample valid names for "/dev/nrmt0h" are:
c                    0
c                   t0
c                  mt0
c                 rmt0
c                nrmt0
c           /dev/nrmt0
c                    0h
c                   t0h
c                  mt0h
c                  etc.
c
C     ------------------------------------------------------------------
C
       tsuffix=.FALSE.
       tprefix=.FALSE.
       tunit=.FALSE.
       tnorew=.FALSE.
       tulast=.FALSE.
       loute=iwhole
       loutb=1
       len=-1
       cout(1:1)=char(0)
c
c   delete leading and trailing white space and count charcters
c
       call white_out(cin,temp1,len1)
c
c   length zero string is error
c
       if(len1.eq.0)then
          length=len1
          cout=' '
          return
       endif
c
c   look for "density" suffix.
c
       if(temp1(len1:len1).eq.'h'.or.
     1    temp1(len1:len1).eq.'m'.or.
     2    temp1(len1:len1).eq.'l')  tsuffix=.TRUE.
c
c   get unit number (only one digit allowed in name)
c                   and must be last or next to last character.
c
       ind=find_any(temp1,"01234")
       if(ind.gt.0)then
          if(ind.eq.len1)then
             loute=iwhole-1
          else if(ind.eq.(len1-1))then
             loute=iwhole
             if(.not.tsuffix)then
                length=-1
                return
             endif
          else
             length=-1
             return
          endif
          tunit=.TRUE.
          unit=temp1(ind:ind)
       else
          length=-1
          return
       endif
C
C  check if prefix (path) present.
C        also checks for "no rewind" flag, but this info not
C        used as yet.
C
       ipre=index(temp1,'/dev/')
       ipre1=index(temp1,'/dev/n')
       if(ipre.eq.1)then
          tprefix=.TRUE.
          if(ipre1.eq.1) then
             tnorew=.TRUE.
          else if(ipre.gt.1)then
             length=-1
             return
          endif
       else if(temp1(1:1).eq.'n')then
          tnorew=.TRUE.
       endif
C
C   build name for default path, if path not specified.
C
       if(.not.tprefix)then
           loutb=loute-len1+1
           cout(1:iwhole)=mt_default
           cout(loutb:loute)=temp1
           cout(iwhole+1:iwhole+1)=char(0)
           length=iwhole
       else
           length=min(len1,31)
           cout(1:length)=temp1(1:length)
           if(.not.tsuffix)then
              length=min((len1+1),31)
              cout(length:length)='h'
           endif
           cout(length+1:length+1)=char(0)
       endif
       return
       end
C
