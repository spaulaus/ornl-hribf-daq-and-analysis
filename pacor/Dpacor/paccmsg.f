C$PROG PACCMSG    - Displays "PACC obsolete" message
C
      CALL DINGER(3)
C
      WRITE(6,10)
      WRITE(6,20)
      WRITE(6,30)
      WRITE(6,40)
      WRITE(6,50)
      WRITE(6,10)
      WRITE(6,60)
      WRITE(6,70)
      WRITE(6,80)
      WRITE(6,90)
      WRITE(6,100)
      WRITE(6,110)
      WRITE(6,120)
      WRITE(6,10)
C
   10 FORMAT(' ',
     &'===============================================================')
   20 FORMAT(' PACC is obsolete - Use PACOR instead')
   30 FORMAT(' ')
   40 FORMAT(' ',
     &'Please note that the pacor readout delay specification is ')
   50 FORMAT(' ',
     &'different from pac - pacor specifications are shown below')
   60 FORMAT(' ',
     &'$dla uncondit DT ;Delay-time for unconditional  CAMAC   readout')
   70 FORMAT(' ',
     &'$dla condit   DT ;Delay-time for conditional    CAMAC   readout')
   80 FORMAT(' ',
     &'$dla camac    DT ;Delay-time for block-transfer CAMAC   readout')
   90 FORMAT(' ',
     &'$dla fastbus  DT ;Delay-tine for                Fastbus readout')
  100 FORMAT(' ',
     &'$dla fera     DT ;Delay-time for                Fera    readout')
  110 FORMAT(' ',
     &'$dla vme      DT ;Delay-time for                VME     readout')
  120 FORMAT(' ',
     &'                 ;Valid DT range is 0 to 255 microseconds')
C
      CALL DINGER(3)
C
      STOP
C
      END
