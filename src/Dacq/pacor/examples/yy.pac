$ini cnaf 3,10,0,16,377h
 
$ini cnaf 3,12,0,16,0h
 
$ini cnaf 7,12,0,16,100h

$dla latch    10
$dla uncondit 20
$dla condit   30
$dla camac    40
$dla fastbus  50
$dla fera     255

$dun cnaf 3,15,0,16,777h

$lat c00 n10 f00 a00    GLAT:1              fc09 ac00

$lat c03 n11 f00 a00    GLAT:2     id01     fc09 ac00

$lat c03 n12 f00 a00    GLAT:3     id02     fc09 ac00
 
$lat c03 n13 f00 a00    LATC:1          

$cam c01 n05 f00 a00-07 ADC1:1,1   id101,1  fc09 ac00 
$cam c01 n06 f00 a00-07 ADC1:9,1   id109,1  fc09 ac00
$cam c01 n07 f00 a00-07 ADC1:17,1  id117,1  fc09 ac00
$cam c01 n08 f00 a00-07 ADC1:25,1  id125,1  fc09 ac00

$cam c01 n09 f00 a00-07 ADC2:1,1   id201,1  fc09 ac00
$cam c01 n10 f00 a00-07 ADC2:9,1   id209,1  fc09 ac00
$cam c01 n11 f00 a00-07 ADC2:17,1  id217,1  fc09 ac00
$cam c01 n12 f00 a00-07 ADC2:25,1  id225,1  fc09 ac00
 
$cam c01 n13 f00 a00-07 ADC3:1,1   id301,1  fc09 ac00
$cam c01 n14 f00 a00-07 ADC3:9,1   id309,1  fc09 ac00
$cam c01 n15 f00 a00-07 ADC3:17,1  id317,1  fc09 ac00
$cam c01 n16 f00 a00-07 ADC3:25,1  id325,1  fc09 ac00
 
$cam c02 n08 f00 a00-15 TDC1:1,1   id401,1  fc09 ac00
$cam c02 n09 f00 a00-15 TDC1:17,1  id417,1  fc09 ac00
 
$cam c02 n10 f00 a00-15 TDC2:1,1   id501,1  fc09 ac00
$cam c02 n11 f00 a00-15 TDC2:17,1  id517,1  fc09 ac00
 
$cam c02 n12 f00 a00-15 TDC3:1,1   id601,1  fc09 ac00
$cam c02 n13 f00 a00-15 TDC3:17,1  id617,1  fc09 ac00

$xia c02 n21 v1 g6
$xia c02 n22 v2 g6
 
$fer c01 n08     a00-15 ADCTST_33:1,1 id701,1  mt=lrs_4300

$fas c01 n15     a01-75 tdc-xxx:1,1   id801,1  mt=phil_10c6

$did 1023 1024 1025

$rif GLAT(2),0F22H lrs_4300

$kil any GLAT(1) 0Fh
 
$kil none GLAT(2) 7000h
 
$pat PAT =  GLAT(1)1,16 
$pat PAT =  GLAT(2)1,16
$pat PAT =  LATC(1)1,16 
$pat PATTERN_2 = GLAT(1)1,2 GLAT(3)9,12
      IX=10
      MSK=[1,2,3,IX]+100H
      I=1
      KIFN(GLAT(I),MSK)       ;Tst crash-bits; GLAT(1) must have
                              ;been read in via $lat directive
      JJ=25
      READ ADC1(JJ)
      READ ADC3(JJ)
      READ ADC3(26)
 
      CNAF 2,15,0,16,0377H

      I=0                     ;Init pattern-word bit counter
      J=0                     ;Init ADC, TDC, index value
      MSK=12345678
L1    LOOP 3                  ;Loop over 3 "detectors"
      I=I+1                   ;Increment bit-counter
      J=J+1                   ;Increment ADC, TDC index
      IFN(GLAT(J),0ACFh)GOTO NEXT
      IFU(PAT(I))GOTO  END1   ;Tst pattern bit - skip read if not set
      READ ADC1(J)            ;Read ADC1
      READ ADC2(J)            ;Read ADC2
      READ TDC1(J)            ;Read TDC1
      READ TDC2(J)            ;Read TDC2
ENDX  CONTINUE
END1  ENDLOOP                 ;End-of-loop
 
NEXT  CONTINUE
      I=10                    ;Init pattern-word bit counter
      J=0                     ;Init ADC, TDC, index value
L2    LOOP 3                  ;Loop over 3 "detectors"
      I=I+1                   ;Increment bit-counter
      J=J+1                   ;Increment ADC, TDC index
      IFN(GLAT(J),MSK)GOTO  QUIT
      IFU(PAT(I))GOTO END2   ;Tst pattern bit - skip read if not set
      READ ADC1(J)            ;Read ADC1
      READ ADC2(J)            ;Read ADC2
      READ TDC1(J)            ;Read TDC1
      READ TDC2(J)            ;Read TDC2
END2  ENDLOOP                 ;End-of-loop

      J=1
      I=1
      IFA(GLAT(J),MSK)GOTO X1
      GOTO QUIT
X1    READ ADC3(I)
      READ TDC3(I)
      GOTO QUIT
      I=4
X2    READ ADC3(I)
      READ TDC3(I)
QUIT  CONTINUE
