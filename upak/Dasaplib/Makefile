asaplib:\
asaplib.a(asapr.o)\
asaplib.a(backer.o)\
asaplib.a(calbak.o)\
asaplib.a(caller.o)\
asaplib.a(diddlw.o)\
asaplib.a(filopen.o)\
asaplib.a(fox.o)\
asaplib.a(oneby1.o)\
asaplib.a(pfind.o)\
asaplib.a(pkfit.o)\
asaplib.a(sapnit.o)\
asaplib.a(selpks.o)\
asaplib.a(setter.o)\
asaplib.a(smin.o)\
asaplib.a(spkin.o)\
asaplib.a(summer.o)
	ranlib asaplib.a
	@echo asaplib.a is up-to-date
#
FFLAGS= $(OPT) $(FARGS)
#
.f.o:
	$(F77) -c $(FFLAGS)  $<
#
install: asaplib
	install -d $(INSTALLDIR)
	install -p asaplib.a $(INSTALLDIR)

clean: 
	rm -f *.exe  *.o asaplib.a
