*******************************************************************************
*
*     File:   /usr/users/mcsq/Dwks/unpackbb.f
*
*   Routine returns one event from the current input buffer.  The array IDBUF
*   is a list of parameter IDs in the order found in the input buffer.  NPARAM
*   is the number of parameters for this event.  Hence, only IDBUF(1) thru
*   IDBUF(NPARAM) are valid.  The array EVBUF is the parameter data.  Use the
*   parameter ID as the index to this array.  For example, dataum for parameter
*   17 is stored in EVBUF(17).  If EVBUF(I) is zero, either this event did
*   not have that parameter or the dataum was zero.
*
*   The arrays IDBUF and EVBUF should be dimensioned as large as the maximum
*   parameter ID expected. 
*
*   This routine is slower than unpackaa.f.  However, if you are doing
*   multiparameter gating in your histogramming, that process will be faster.
*
*        CALL  UNPACKBB(IBUF,NHW,IDBUF,EVBUF,MAXID,NPARAM,IERR,*)
*
*  Call arguments:
*        INTEGER*2  IBUF - raw event data buffer
*        INTEGER*4  NHW  - number of INT*2 words in IBUF
*        INTEGER*4  MAXID - Max ID. IDBUF and EVBUF must be dimensioned
*                           at least as great as MAXID
*  Returns:
*        INTEGER*4  IDBUF()  - List of IDs in this event
*        INTEGER*4  EVBUF(id) - Event data for parameter id
*        INTEGER*4  NPARAM - Number of parameters in this event.
*        INTEGER*4  IERR  -  0 means OK.  Nonzero means too many parameters.
*  Alternate return:
*        Return here when input buffer is empty or a L003 format error
*        occurs.  IERR = 0 means end of input buffer and IERR = 1 means
*        a L003 format error.
*******************************************************************************
      subroutine unpackbb(ibuf,nhw,idbuf,evbuf,maxid,nparam,ierr,*)

      implicit none
      integer*2 ibuf(*)
      integer*4 nhw,nparam,ierr,evbuf(*),idbuf(*),maxid
      integer*4 data,i,j,id,indb/1/,param/0/
      save  indb,param

*
*  Erase the last event in the EVBUF aray.
*
      do i = 1,param
         evbuf(idbuf(i)) = 0
         idbuf(i) = 0
      enddo
      ierr = 0
      nparam = 0
      param = 0
*
*  indb points to first parameter in new event
*
      do i = indb, nhw, 2
*
*  First number is ID followed by the data
*
         id = ibuf(i)
         data = ibuf(i+1)
         if (id .eq. -1) then
            if (data .eq. -1) then
*
*                Found end-of-event
*
               if (nparam .gt. 0) then
                 indb = i + 2       !move index to start of next event
                 param = nparam
                 return             !return one event
               else
                 continue
               endif
            else
*
*  L003 format error.  Take the alternate return with ierr set nonzero.
*
               param = nparam
               ierr = 1
               return 1
            endif
         else
            id = iand(id,'7fff'x)
            if (id .eq. 0 .or. id .gt. maxid) then
*
*  Too much data for the event buffer array.  Normal return with ierr
*  nonzero.
*
              param = nparam
              ierr = 2
              return
            endif
            nparam = nparam + 1
            idbuf(nparam) = id
            evbuf(id) = iand(data,'ffff'x)
*
*  One parameter extraction completed
*
         endif
*
      enddo
*
*   When we get here, we have reached the end of ibuf.  Reset the index
*   for ibuf to the beginning of the buffer for the next call.  Then
*   take the alternate return to indicate that a new buffer is needed.
*
      indb = 1
      return 1        !request new buffer.
*
      end
