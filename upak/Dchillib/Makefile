chillib:\
chillib.a(assym.o)\
chillib.a(baffer.o)\
chillib.a(banloc.o)\
chillib.a(cline.o)\
chillib.a(dimen.o)\
chillib.a(dirco.o)\
chillib.a(equate.o)\
chillib.a(freg1.o)\
chillib.a(gaffer.o)\
chillib.a(gasort.o)\
chillib.a(gatad.o)\
chillib.a(gatin.o)\
chillib.a(gatloc.o)\
chillib.a(gspan.o)\
chillib.a(hislog.o)\
chillib.a(hismil.o)\
chillib.a(hispro.o)\
chillib.a(idvav.o)\
chillib.a(ifpro.o)\
chillib.a(ioerr.o)\
chillib.a(isvav.o)\
chillib.a(iterv.o)\
chillib.a(kinda.o)\
chillib.a(kvalu.o)\
chillib.a(kvalue.o)\
chillib.a(lablst.o)\
chillib.a(lasav.o)\
chillib.a(lasub.o)\
chillib.a(lismil.o)\
chillib.a(locsym.o)\
chillib.a(lodjwd.o)\
chillib.a(lograt2.o)\
chillib.a(looper.o)\
chillib.a(lterms.o)\
chillib.a(mapper.o)\
chillib.a(milout.o)\
chillib.a(nuban.o)\
chillib.a(nugat.o)\
chillib.a(nvalu.o)\
chillib.a(nxlabl.o)\
chillib.a(nxop.o)\
chillib.a(operr.o)\
chillib.a(parco.o)\
chillib.a(parlin.o)\
chillib.a(pass1.o)\
chillib.a(pass2.o)\
chillib.a(plist.o)\
chillib.a(qq.o)\
chillib.a(refor.o)\
chillib.a(savlin.o)\
chillib.a(subsort.o)
	ranlib chillib.a
	@echo chillib.a is up-to-date
#
FFLAGS= $(OPT) $(FARGS)
#
.f.o:
	$(F77) -c $(FFLAGS) $<
#
install: chillib
	install -d $(INSTALLDIR)
	install -p chillib.a $(INSTALLDIR)

clean: 
	rm -f *.exe *.o chillib.a
