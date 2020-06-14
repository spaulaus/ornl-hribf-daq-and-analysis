*******************************************************************************
*
*    File:  /usr/users/mcsq/Dwks/unpackaa.f
*
*   Routine returns one event from the current input buffer.  Parameter ID
*   and datum are returned in the two dimensional array EVBUF.  EVBUF(1,I)
*   is the ID for the Ith parameter and EVBUF(2,I) is the datum.  NPARAM
*   is the number of parameters in this event.  Hence, only EVBUF(*,1)
*   thru EVBUF(*,NPARAM) are valid.  Parameters are stored in EVBUF is
*   order found in the input buffer.
*
*   This routine is faster than unpackbb.f.  If multiparameter gating
*   is used in your histogramming, the overall performance may be better
*   if unpackbb.f is used.
*
*        CALL UNPACKAA(IBUF,NHW,EVBUF,NPARAM,EVSIZE,IERR,*)
*
*  Call arguments:
*        INTEGER*2  IBUF - raw event data buffer
*        INTEGER*4  NHW  - number of INT*2 words in IBUF
*        INTEGER*4  EVSIZE - Second dimension of array EVDAT
*  Returns:
*        INTEGER*4  EVBUF(2,*) - Event data. EVBUF(1,*) is the parameter ID
*                                EVBUF(2,*) is the data.
*        INTEGER*4  NPARAM - Number of parameters in this event.
*        INTEGER*4  IERR  -  0 means OK.  Nonzero means too many parameters.
*  Alternate return:
*        Return here when input buffer is empty or a L003 format error
*        occurs.  IERR = 0 means end of input buffer and IERR = 1 means
*        a L003 format error.
*******************************************************************************
      subroutine unpackaa(ibuf,nhw,evbuf,nparam,evsize,ierr,*)

      implicit none
      integer*2 ibuf(*)
      integer*4 nhw,nparam,evsize,ierr,evbuf(2,*)
      integer*4 data,i,id,indb/1/
      save  indb
*
*  indb points to first parameter in new event
*
      ierr = 0
      nparam = 0
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
                 return             !return one event
               else
                 continue
               endif
            else
*
*  L003 format error.  Take the alternate return with ierr set nonzero.
*
               ierr = 1
               return 1
            endif
         else
            nparam = nparam + 1
            if (nparam .gt. evsize) then
*
*  Too much data for the event buffer array.  Normal return with ierr
*  nonzero.
*
              ierr = 2
              return
            endif
            evbuf(1,nparam) = iand(id,'7fff'x)
            evbuf(2,nparam) = iand(data,'ffff'x)
*
*  One parameter extraction completed
*
         endif

      enddo
*
*   When we get here, we have reached the end of ibuf.  Reset the index
*   for ibuf to the beginning of the buffer for the next call.  Then
*   take the alternate return to indicate that a new buffer is needed.
*
      indb = 1
      return 1        !request new buffer.

      end
