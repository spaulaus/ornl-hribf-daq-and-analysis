!
! brem.pac
! PAC file for readout of the BaF array at SUNY. 
!
!  1) the crate numbers are 0 to 5 for CAMAC, 8 for FastBus
!
!--------------------------------------------------------
!   Run time initialization of the CAMAC
!--------------------------------------------------------
!
!      Clear all the modules and reset busy
$ini cnaf  0  4 0 9
$ini cnaf  0  5 0 9
$ini cnaf  0  6 0 9
$ini cnaf  0  7 0 9
$ini cnaf  0  9 0 9
!
$ini cnaf  0 11 0 9
$ini cnaf  0 12 0 9
$ini cnaf  0 13 0 9
$ini cnaf  0 14 0 9
$ini cnaf  0 15 0 9
$ini cnaf  0 16 0 9
$ini cnaf  0 17 0 9
$ini cnaf  0 18 0 9
$ini cnaf  0 19 0 9
$ini cnaf  0 20 0 9
!
!clear the ADC of monitor
$ini cnaf  1 4 12 11
!
$ini cnaf  1 10 0 9
$ini cnaf  1 11 0 9
$ini cnaf  1 12 0 9
$ini cnaf  1 13 0 9
$ini cnaf  1 14 0 9
$ini cnaf  1 15 0 9
$ini cnaf  1 16 0 9
$ini cnaf  1 17 0 9
$ini cnaf  1 19 0 9
$ini cnaf  1 20 0 9
!
!     ADCs set for Pedestal subtraction
!                CSR=0100H
!     PPAC'S
$ini cnaf  0  4 0 16 0100h
$ini cnaf  0  5 0 16 0100h
$ini cnaf  0  6 0 16 0100h
$ini cnaf  0  7 0 16 0100h
!     Cosmics
$ini cnaf  0  9 0 16 0100h
!     BaF's
$ini cnaf  0 11 0 16 0100h
$ini cnaf  0 12 0 16 0100h
$ini cnaf  0 13 0 16 0100h
$ini cnaf  0 14 0 16 0100h
$ini cnaf  0 15 0 16 0100h
$ini cnaf  0 16 0 16 0100h
$ini cnaf  0 17 0 16 0100h
$ini cnaf  0 18 0 16 0100h
$ini cnaf  0 19 0 16 0100h
$ini cnaf  0 20 0 16 0100h
!
$ini cnaf  1 10 0 16 0100h
$ini cnaf  1 11 0 16 0100h
$ini cnaf  1 12 0 16 0100h
$ini cnaf  1 13 0 16 0100h
$ini cnaf  1 14 0 16 0100h
$ini cnaf  1 15 0 16 0100h
$ini cnaf  1 16 0 16 0100h
$ini cnaf  1 17 0 16 0100h
$ini cnaf  1 19 0 16 0100h
$ini cnaf  1 20 0 16 0100h
!
!    Used PPAC ADC channels
$ini cnaf  0  4  0 17 028h
$ini cnaf  0  4  1 17 022h
$ini cnaf  0  4  2 17 02ah
$ini cnaf  0  4  3 17 024h
$ini cnaf  0  5  0 17 030h
$ini cnaf  0  5  1 17 01ah
$ini cnaf  0  5  2 17 024h
$ini cnaf  0  5  3 17 01eh
$ini cnaf  0  6  0 17 02eh
$ini cnaf  0  6  1 17 02ch
$ini cnaf  0  6  2 17 028h
$ini cnaf  0  6  3 17 02ch
$ini cnaf  0  7  0 17 028h
$ini cnaf  0  7  1 17 028h
$ini cnaf  0  7  2 17 02eh
$ini cnaf  0  7  3 17 028h
!  Unused PPAC ADC channels
$ini cnaf  0  4  4 17 0ffh
$ini cnaf  0  4  5 17 0ffh
$ini cnaf  0  4  6 17 0ffh
$ini cnaf  0  4  7 17 0ffh
$ini cnaf  0  4  8 17 0ffh
$ini cnaf  0  4  9 17 0ffh
$ini cnaf  0  4 10 17 0ffh
$ini cnaf  0  4 11 17 0ffh
$ini cnaf  0  4 12 17 0ffh
$ini cnaf  0  4 13 17 0ffh
$ini cnaf  0  4 14 17 0ffh
$ini cnaf  0  4 15 17 0ffh
!
$ini cnaf  0  5  4 17 0ffh
$ini cnaf  0  5  5 17 0ffh
$ini cnaf  0  5  6 17 0ffh
$ini cnaf  0  5  7 17 0ffh
$ini cnaf  0  5  8 17 0ffh
$ini cnaf  0  5  9 17 0ffh
$ini cnaf  0  5 10 17 0ffh
$ini cnaf  0  5 11 17 0ffh
$ini cnaf  0  5 12 17 0ffh
$ini cnaf  0  5 13 17 0ffh
$ini cnaf  0  5 14 17 0ffh
$ini cnaf  0  5 15 17 0ffh
!
$ini cnaf  0  6  4 17 0ffh
$ini cnaf  0  6  5 17 0ffh
$ini cnaf  0  6  6 17 0ffh
$ini cnaf  0  6  7 17 0ffh
$ini cnaf  0  6  8 17 0ffh
$ini cnaf  0  6  9 17 0ffh
$ini cnaf  0  6 10 17 0ffh
$ini cnaf  0  6 11 17 0ffh
$ini cnaf  0  6 12 17 0ffh
$ini cnaf  0  6 13 17 0ffh
$ini cnaf  0  6 14 17 0ffh
$ini cnaf  0  6 15 17 0ffh
!
$ini cnaf  0  7  4 17 0ffh
$ini cnaf  0  7  5 17 0ffh
$ini cnaf  0  7  6 17 0ffh
$ini cnaf  0  7  7 17 0ffh
$ini cnaf  0  7  8 17 0ffh
$ini cnaf  0  7  9 17 0ffh
$ini cnaf  0  7 10 17 0ffh
$ini cnaf  0  7 11 17 0ffh
$ini cnaf  0  7 12 17 0ffh
$ini cnaf  0  7 13 17 0ffh
$ini cnaf  0  7 14 17 0ffh
$ini cnaf  0  7 15 17 0ffh
!
!    Unused Cosmics channels
$ini cnaf  0  9  0 17 0ffh
$ini cnaf  0  9  1 17 0ffh
$ini cnaf  0  9  2 17 0ffh
$ini cnaf  0  9  3 17 0ffh
$ini cnaf  0  9  4 17 0ffh
$ini cnaf  0  9  5 17 0ffh
$ini cnaf  0  9  6 17 0ffh
$ini cnaf  0  9  7 17 0ffh
$ini cnaf  0  9  8 17 0ffh
$ini cnaf  0  9  9 17 0ffh
$ini cnaf  0  9 10 17 0ffh
$ini cnaf  0  9 11 17 0ffh
$ini cnaf  0  9 12 17 0ffh
$ini cnaf  0  9 13 17 0ffh
$ini cnaf  0  9 14 17 0ffh
$ini cnaf  0  9 15 17 0ffh
!
! UNUSED Hi ADC CHANNELS' PEDESTALS SET TO 255
!
!                 PED=00FFH
$ini cnaf  1 11 14 17 00ffh
$ini cnaf  1 16  6 17 00ffh
$ini cnaf  1 17  5 17 00ffh
$ini cnaf  1 19 15 17 00ffh
$ini cnaf  1 20 14 17 00ffh
$ini cnaf  1 20 15 17 00ffh
!
! UNUSED Fa ADC CHANNELS' PEDESTALS SET TO 255
!
!                 PED=00FFH
$ini cnaf  0 12 14 17 00ffh
$ini cnaf  0 17  6 17 00ffh
$ini cnaf  0 18  5 17 00ffh
$ini cnaf  0 19 15 17 00ffh
$ini cnaf  0 20 14 17 00ffh
$ini cnaf  0 20 15 17 00ffh
!
!-----------------------------------------------------
!   Define various coincidence registers
!$cam c03 n13 f00 a00 trigger:00 id100   ! before/after inhibit trigger bits
!$cam c03 n13 f00 a01 trigger:00 id101   ! reduced trigger bits
$lat c03 n13 f00 a00 trigger:01 id100    ! inhibit trigger bits
$lat c03 n13 f00 a01 triggerr:01 id101   ! reduced trigger bits
!
$kil any trigger(1),22H
$kil none triggerr(1),11H
!
!$cam c02 n01 f00 a00-02 bank:01,01 id01,01 ! Bank 1-3 bits
!$cam c02 n02 f00 a00-02 bank:04,01 id04,01 ! Bank 4-6 bits
!$cam c04 n02 f00 a00-02 bank:07,01 id07,01 ! Bank 7-9 bits
!$cam c04 n03 f00 a00    bank:10    id10    ! Bank 10  bits
!
!-----------------------------------------------------
!
!    Defination of the ortec811 ADC
$cam c01 n04 f00 a00-03  mone:01,1  id2304,1  fc11 ac12       ! Monitor 
!$cam c01 n04 f00 a00  MonE:00,01  id2304,01  fc11 ac12 dt100 ! Monitor 
!-----------------------------------------------------
!
!    Definition of the FERA ADCS and TDCs
!
!     PPAC ADC'S
$fer c00 n04   a00-15 pp01:00,01 id00256,01 mt=LRS_4300
$fer c00 n05   a00-15 pp02:00,01 id00320,01 mt=LRS_4300
$fer c00 n06   a00-15 pp03:00,01 id00384,01 mt=LRS_4300
$fer c00 n07   a00-15 pp04:00,01 id00448,01 mt=LRS_4300
!
! Cosmics
! Will this work ?!
$fer c00 n09   a00-15 cs01:00,01 id00576,01 mt=LRS_4300
!
!     HI gain ADCs
!
$fer c01 n10   a00-15 hi01:00,01 id02688,01 mt=LRS_4300
$fer c01 n11   a00-15 hi02:00,01 id02752,01 mt=LRS_4300
$fer c01 n12   a00-15 hi03:00,01 id02816,01 mt=LRS_4300
$fer c01 n13   a00-15 hi04:00,01 id02880,01 mt=LRS_4300
$fer c01 n14   a00-15 hi05:00,01 id02944,01 mt=LRS_4300
$fer c01 n15   a00-15 hi06:00,01 id03008,01 mt=LRS_4300
$fer c01 n16   a00-15 hi07:00,01 id03072,01 mt=LRS_4300
$fer c01 n17   a00-15 hi08:00,01 id03136,01 mt=LRS_4300
$fer c01 n19   a00-15 hi09:00,01 id03264,01 mt=LRS_4300
$fer c01 n20   a00-15 hi10:00,01 id03328,01 mt=LRS_4300
!
!     FAst-light ADCs
!
$fer c00 n11   a00-15 fa01:00,01 id00704,01 mt=LRS_4300
$fer c00 n12   a00-15 fa02:00,01 id00768,01 mt=LRS_4300
$fer c00 n13   a00-15 fa03:00,01 id00832,01 mt=LRS_4300
$fer c00 n14   a00-15 fa04:00,01 id00896,01 mt=LRS_4300
$fer c00 n15   a00-15 fa05:00,01 id00960,01 mt=LRS_4300
$fer c00 n16   a00-15 fa06:00,01 id01024,01 mt=LRS_4300
$fer c00 n17   a00-15 fa07:00,01 id01088,01 mt=LRS_4300
$fer c00 n18   a00-15 fa08:00,01 id01152,01 mt=LRS_4300
$fer c00 n19   a00-15 fa09:00,01 id01216,01 mt=LRS_4300
$fer c00 n20   a00-15 fa10:00,01 id01280,01 mt=LRS_4300
!
!     FastbuS TDCs
!
$fas c01 n18   a00-63 tf01:00,01 id17536,01 mt=LRS_1875 
$fas c01 n20   a00-63 tf02:00,01 id17664,01 mt=LRS_1872 
$fas c01 n24   a00-63 tf03:00,01 id17920,01 mt=LRS_1872 
!
$pat trigpat = trigger(1)1,16
$did 98,99
!
        IFU(trigpat(6)) GOTO L1
           READ mone(1)
           READ mone(2)
           READ mone(3)
           READ mone(4)
L1      CONTINUE    ! no statement
