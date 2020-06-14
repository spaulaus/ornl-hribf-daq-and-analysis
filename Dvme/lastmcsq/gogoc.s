	SECTION	9
	XDEF	gogo
gogo:
        MOVEQ   #-1,D0
        MOVE.L  D0,A6
        LINK    A6,#0
    	JSR     start
	jsr     main
        MOVE.L  errno,-(SP)
        JSR     exit
        UNLK    A6
	TRAP	#0
        DCB     20,0
*
*  This is a trick which causes highest loaded address to be greater
*  than the end of the bss section (i.e. SECTION 14)
*
	SECTION 15
	DC.B	'mcsq'     ;never referenced by any code!
	SECTION	9
	SECTION	14
	XREF	start
	XREF	main
        XREF    exit
        XREF    errno
* allocations for module
	SECTION	9
	END
