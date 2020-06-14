C$PROG REBUF     - Example routine which constructs a new event-buffer
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     A DO-NOTHING REBUF ROUTINE WHICH JUST COPIES PBUF TO NULIST
C
C     PBUF   - CONTAINS RAW PACKED EVENT
C     NP     = NUMBER OF WORDS IN PBUF NOT INCLUDING THE FFFF
C     NULIST - IS THE NEW OUTPUT BUFFER WHICH YOU LOAD
C     NN     = NUMBER OF WORDS LOADED INTO OUTPUT BUFFER
C              I RESET NN TO ZERO WHENEVER A BUFFER IS PROCESSED
C
C     YOU MUST SUPPLY THE FFFF TO THE OUTPUT STREAM. I COULD DO IT
C     BUT IF YOU DO IT, THEN YOU HAVE THE OPTION OF CREATING MORE
C     THAN ONE EVENT. IF YOU THINK THAT YOU MAY SIGNIFICANTLY
C     INCREASE THE ABOUNT OF DATA (IN GOING FROM INPUT TO OUTPUT),
C     I WILL NEED TO KNOW ABOUT IT.
C     ------------------------------------------------------------------
C
      SUBROUTINE REBUF(PBUF,NP,NULIST,NN)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4   NP,NN,I
C
      INTEGER*2   NULIST(16384),PBUF(2000)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 I=1,NP
      NN=NN+1
      NULIST(NN)=PBUF(I)
   10 CONTINUE
      NN=NN+1
      NULIST(NN)=-1
      RETURN
      END
