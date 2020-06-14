txxlib:\
txxlib.a(adjust.o)\
txxlib.a(blankc.o)\
txxlib.a(blankit.o)\
txxlib.a(caseup4.o)\
txxlib.a(cleanup.o)\
txxlib.a(compoz.o)\
txxlib.a(fontmo.o)\
txxlib.a(getiwd.o)\
txxlib.a(kmdtst.o)\
txxlib.a(lastc.o)\
txxlib.a(linout.o)\
txxlib.a(pac120.o)\
txxlib.a(pager.o)\
txxlib.a(stripath.o)\
txxlib.a(trimout.o)\
txxlib.a(unpac120.o)\
txxlib.a(unpac240.o)
	ranlib txxlib.a
	@echo  txxlib.a is up-to-date
#
FFLAGS= $(OPT) $(FARGS)
#
.f.o:
	$(F77) -c $(FFLAGS) $<
#
#	ar rv txxlib.a $*.o
