all: ipclib.a acqlib.a install

ipclib.a: \
ipclib.a(fortipc.o)
#	ar rv ipclib.a $^
	ranlib ipclib.a
	rm -f *.exe  fortipc.o

acqlib.a:\
acqlib.a(nullacq.o)
#	ar rv acqlib.a $^
	ranlib acqlib.a
	rm -f nullacq.o

install: ipclib.a acqlib.a
	install -d $(INSTALLDIR)
	install ipclib.a $(INSTALLDIR)
	install acqlib.a $(INSTALLDIR)
