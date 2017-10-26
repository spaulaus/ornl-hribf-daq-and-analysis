$init cnaf 1,14,0,9,0h            ;Clear module

$init cnaf 1,14,9,20,7fh          ;Set common threshold
;
$init cnaf 1,14,0,17,081h         ;Set upper threshold
$init cnaf 1,14,1,17,081h         ;Set upper threshold
$init cnaf 1,14,2,17,081h         ;Set upper threshold
$init cnaf 1,14,3,17,081h         ;Set upper threshold
$init cnaf 1,14,4,17,081h         ;Set upper threshold
$init cnaf 1,14,5,17,081h         ;Set upper threshold
$init cnaf 1,14,6,17,081h         ;Set upper threshold
$init cnaf 1,14,7,17,081h         ;Set upper threshold
;
$init cnaf 1,14,8,17,0h           ;Set lower threshold
$init cnaf 1,14,9,17,0h           ;Set lower threshold
$init cnaf 1,14,10,17,0h          ;Set lower threshold
$init cnaf 1,14,11,17,0h          ;Set lower threshold
$init cnaf 1,14,12,17,0h          ;Set lower threshold
$init cnaf 1,14,13,17,0h          ;Set lower threshold
$init cnaf 1,14,14,17,0h          ;Set lower threshold
$init cnaf 1,14,15,17,0h          ;Set lower threshold
;
$init cnaf 1,14,0,20,80h          ;Set zero offset
$init cnaf 1,14,1,20,80h          ;Set zero offset
$init cnaf 1,14,2,20,80h          ;Set zero offset
$init cnaf 1,14,3,20,80h          ;Set zero offset
$init cnaf 1,14,4,20,80h          ;Set zero offset
$init cnaf 1,14,5,20,80h          ;Set zero offset
$init cnaf 1,14,6,20,80h          ;Set zero offset
$init cnaf 1,14,7,20,80h          ;Set zero offset
;
$init cnaf 1,14,14,20,0a00H      ;Control/status register
;
$cam c01 n14 f00 a00-07 adc:1,01 fc09 ac00 id01,01 dt 125
;
