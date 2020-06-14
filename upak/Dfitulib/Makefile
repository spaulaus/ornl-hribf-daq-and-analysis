fitulib:\
fitulib.a(cmpfit.o)\
fitulib.a(cmpfitu.o)\
fitulib.a(cmpgen.o)\
fitulib.a(cmpsplin.o)\
fitulib.a(curv2.o)\
fitulib.a(curvs.o)\
fitulib.a(curvss.o)\
fitulib.a(dmainv.o)\
fitulib.a(errcomp.o)\
fitulib.a(funky.o)\
fitulib.a(fitunit.o)\
fitulib.a(fox.o)\
fitulib.a(getdatx.o)\
fitulib.a(gran12.o)\
fitulib.a(helpnit.o)\
fitulib.a(intrvl.o)\
fitulib.a(nudaf.o)\
fitulib.a(pl.o)\
fitulib.a(plotfit.o)\
fitulib.a(plotxylin.o)\
fitulib.a(plotxysym.o)\
fitulib.a(smin.o)\
fitulib.a(snhcsh.o)\
fitulib.a(splinerr.o)\
fitulib.a(statman.o)\
fitulib.a(tablog.o)\
fitulib.a(terms.o)\
fitulib.a(usend.o)\
fitulib.a(usercmp.o)\
fitulib.a(userfox.o)\
fitulib.a(wtcomp.o)\
fitulib.a(yinmod.o)
	ranlib fitulib.a
	@echo fitulib.a is up-to-date
#
FFLAGS= $(OPT) -DPTRSIZE=$(PTRSIZE) $(FARGS)
#
.F.o:
	$(F77) -c $(FFLAGS)  $<

.f.o:
	$(F77) -c $(FFLAGS)  $<
#
