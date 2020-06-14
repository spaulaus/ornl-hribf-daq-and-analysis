!
!       data format L003
$init cnaf 0,16,1,17,0h
$init cnaf 0,16,0,20,20h
$init cnaf 0,16,1,20,20h
$init cnaf 0,16,2,20,20h
$init cnaf 0,16,3,20,20h
$init cnaf 0,16,4,20,20h
$init cnaf 0,16,5,20,20h
$init cnaf 0,16,6,20,20h
$init cnaf 0,16,7,20 20h
$init cnaf 0,16,8,20,20h
$init cnaf 0,16,9,20,20h
$init cnaf 0,16,10,20,20h
$init cnaf 0,16,11,20,20h
$init cnaf 0,16,12,20,20h
$init cnaf 0,16,13,20,20h
$init cnaf 0,16,14,20,20h
$init cnaf 0,16,15,20,20h
$init cnaf 0,16,2,17,0h
$init cnaf 0,16,0,20,0fe0h
$init cnaf 0,16,1,20,0fe0h
$init cnaf 0,16,2,20,0fe0h
$init cnaf 0,16,3,20,0fe0h
$init cnaf 0,16,4,20,0fe0h
$init cnaf 0,16,5,20,0fe0h
$init cnaf 0,16,6,20,0fe0h
$init cnaf 0,16,7,20 0fe0h
$init cnaf 0,16,8,20,0fe0h
$init cnaf 0,16,9,20,0fe0h
$init cnaf 0,16,10,20,0fe0h
$init cnaf 0,16,11,20,0fe0h
$init cnaf 0,16,12,20,0fe0h
$init cnaf 0,16,13,20,0fe0h
$init cnaf 0,16,14,20,0fe0h
$init cnaf 0,16,15,20,0fe0h
$init cnaf 0,16,0,19,06h
$run  cnaf 0,16,0,19,06h
;
$did 255,255,255
;
!$lat c00 n01 f00 a00 glat:01
$cam c00 n01 f00 a00 tst:01
$cam c00 n16     a00-15 tdc:1,01 id01,01 mt=phil_7186
;
$rgat gate(1) tdc(16) 1792,2304
$rgat gate(4) tdc(2) 1,200
$rgat gate(5) tdc(2) 2,99
$rgat gate(2) tdc(2) 200,3000
$rgat gate(3) tdc(16) 1,100
