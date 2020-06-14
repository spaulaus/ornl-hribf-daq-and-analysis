fitxlib:\
fitxlib.a(cmpfit.o)\
fitxlib.a(cmpfitu.o)\
fitxlib.a(cmpgen.o)\
fitxlib.a(dmainv.o)\
fitxlib.a(errcomp.o)\
fitxlib.a(funky.o)\
fitxlib.a(fitxnit.o)\
fitxlib.a(fox.o)\
fitxlib.a(getdatx.o)\
fitxlib.a(gran12.o)\
fitxlib.a(helpnit.o)\
fitxlib.a(nudaf.o)\
fitxlib.a(pl.o)\
fitxlib.a(smin.o)\
fitxlib.a(statman.o)\
fitxlib.a(tablog.o)\
fitxlib.a(usend.o)\
fitxlib.a(usercmp.o)\
fitxlib.a(userfox.o)\
fitxlib.a(wtcomp.o)\
fitxlib.a(yinmod.o)
	ranlib fitxlib.a
	@echo fitxlib.a is up-to-date
#
FFLAGS= $(OPT) $(FARGS)
#
.f.o:
	$(F77) -c $(FFLAGS)  $<
#
#	ar rv fitxlib.a  $*.o
