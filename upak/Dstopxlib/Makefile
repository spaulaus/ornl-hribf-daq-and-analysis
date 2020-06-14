stopxlib:\
stopxlib.a(absorber.o)\
stopxlib.a(blank.o)\
stopxlib.a(chara.o)\
stopxlib.a(cmpstopx.o)\
stopxlib.a(dodedx.o)\
stopxlib.a(doeloss.o)\
stopxlib.a(dorange.o)\
stopxlib.a(elementx.o)\
stopxlib.a(elspwr.o)\
stopxlib.a(fopen.o)\
stopxlib.a(gas.o)\
stopxlib.a(getabsorb.o)\
stopxlib.a(getenergy.o)\
stopxlib.a(getfile.o)\
stopxlib.a(getnucn.o)\
stopxlib.a(getproj.o)\
stopxlib.a(number.o)\
stopxlib.a(nuspwr.o)\
stopxlib.a(pack.o)\
stopxlib.a(rangi.o)\
stopxlib.a(spwrhi.o)\
stopxlib.a(squezr.o)\
stopxlib.a(statman.o)\
stopxlib.a(stopp.o)\
stopxlib.a(stopxnit.o)\
stopxlib.a(thick.o)\
stopxlib.a(unpack.o)
	ranlib stopxlib.a
	@echo  stopxlib.a is up-to-date
#
FFLAGS= $(OPT) $(FARGS)
#
.f.o:
	$(F77) $(FFLAGS) -c $<
#
#	ar rv stopxlib.a $*.o
