xglib:\
xglib.a(blankw.o)\
xglib.a(boxit.o)\
xglib.a(colrset.o)\
xglib.a(doaxis.o)\
xglib.a(dracir.o)\
xglib.a(getgco.o)\
xglib.a(getinteger.o)\
xglib.a(lablman.o)\
xglib.a(lablod.o)\
xglib.a(lablout.o)\
xglib.a(lablru.o)\
xglib.a(lacom.o)\
xglib.a(lacopy.o)\
xglib.a(lodnx.o)\
xglib.a(logtic.o)\
xglib.a(markit1.o)\
xglib.a(markit2.o)\
xglib.a(markits.o)\
xglib.a(markum1.o)\
xglib.a(ndigit.o)\
xglib.a(newfig.o)\
xglib.a(nucur.o)\
xglib.a(numac.o)\
xglib.a(numass.o)\
xglib.a(pixtoc.o)\
xglib.a(plotpix.o)\
xglib.a(plotxy.o)\
xglib.a(plotxyl.o)\
xglib.a(ploty.o)\
xglib.a(plotyy.o)\
xglib.a(revv.o)\
xglib.a(texout.o)\
xglib.a(tickl.o)\
xglib.a(xcomdef.o)\
xglib.a(xglibc.o)\
xglib.a(xyman.o)
	ranlib xglib.a
	@echo xglib.a is up-to-date
#
FFLAGS= -O0 -fno-automatic
WORDSIZE=64
#
.F.o:
	f77 $(FFLAGS) -c $<
.f.o:
	f77 $(FFLAGS) -c $<
.c.o:
	gcc   -c -I/usr/X11R6/include/ $<
