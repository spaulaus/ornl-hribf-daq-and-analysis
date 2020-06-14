# Makefile 
# Make all the programs in Dacq, including pacor.
#
# RLV 28 Dec 2017
#
#
BINS= install-acq_run \
      install-caenr \
      install-caenw \
      install-cnaf \
      install-docnaf \
      install-femsg \
      install-femsg-stdout \
      install-fifo \
      install-ipcmon \
      install-logger \
      install-m_setup \
      install-pacor \
      install-udpmon \
      install-vmehardware

CFLAGS = $(OPTS) $(CARGS)
FFLAGS = $(OPTS) $(FARGS)

INCS= $(includedir)/pkt_io_udp.h  $(includedir)/orph_udp.h $(includedir)/acq_ctl.h

# This is the master install target.  
install: $(BINS)

######acq_run	
acq_run: acq_run.c  $(INCS)
	$(CC) $(CFLAGS) -o acq_run acq_run.c $(OBJECTS) $(libdir)/vmelib.a

install-acq_run: acq_run
	install acq_run $(bindir)
	install startvme $(bindir)
	install stopvme $(bindir)
	install statvme $(bindir)
	install initvme $(bindir)
	install pacfile $(bindir)
	install zeroclk $(bindir)
	install zerosis $(bindir)

#
######caenr	
caenr: caenr.f $(INCS)
	$(FORT) $(FFLAGS) caenr.f $(hhirfdir)/orphlib.a $(libdir)/vmexxlib.a $(libdir)/vmelib.a -o caenr

install-caenr: caenr
	install caenr $(bindir)

#
#
######caenw	
caenw: caenw.f $(INCS)
	$(FORT) $(FFLAGS) caenw.f $(hhirfdir)/orphlib.a $(libdir)/vmexxlib.a $(libdir)/vmelib.a -o caenw

install-caenw: caenw
	install caenw $(bindir)
#
#
######cnaf
cnaf: cnaf.c  $(INCS)
	$(CC) $(CFLAGS) cnaf.c $(hhirfdir)/orphlib.a $(libdir)/vmelib.a -o cnaf

install-cnaf: cnaf
	install cnaf $(bindir)

#
#
######docnaf
docnaf: docnaf.c  docnaf.h
	$(CC) $(CFLAGS) -o docnaf docnaf.c $(hhirfdir)/orphlib.a $(libdir)/vmelib.a

install-docnaf: docnaf
	install -p docnaf $(bindir)

#
#
######femsg-stdout
femsg-stdout: femsg-stdout.c 
	$(CC) $(CFLAGS) -o femsg-stdout femsg-stdout.c $(libdir)/vmelib.a $(libdir)/acqlib.a $(libdir)/ipclib.a

install-femsg-stdout: femsg-stdout
	install femsg-stdout $(bindir)

#
#
######femsg
femsg: femsg.c 
	$(CC) $(CFLAGS) -o femsg femsg.c $(libdir)/vmelib.a $(libdir)/acqlib.a $(libdir)/ipclib.a

install-femsg: femsg
	install femsg $(bindir)

#
#
######fifo
fifo: fifo.o  
	cc $(CFLAGS) fifo.c $(libdir)/vmelib.a -o fifo

install-fifo: fifo
	install $^ $(bindir)

#
#
######ipcmon
ipcmon: ipcmon.c $(includedir)/ipcdefs.h $(includedir)/acqlib.h
	$(CC) $(CFLAGS) -o ipcmon ipcmon.c $(libdir)/vmelib.a $(libdir)/acqlib.a $(libdir)/ipclib.a

install-ipcmon: ipcmon
	install ipcmon $(bindir)

#
#
######logger
logger: logger.c $(includedir)/orphmsg.h $(includedir)/ipcdefs.h $(includedir)/acqlib.h
	cc $(CFLAGS) -o logger logger.c $(libdir)/acqlib.a $(libdir)/ipclib.a

install-logger: logger
	install logger $(bindir)
#
#
######m_setup
m_setup: m_setup.c 
	gcc $(CFLAGS) m_setup.c -o m_setup $(hhirfdir)/orphlib.a $(libdir)/vmelib.a $(libdir)/vmexxlib.a
install-m_setup: m_setup
	install m_setup $(bindir)

#
#
######pacor
install-pacor:
	$(MAKE) -C pacor install
#
#
######udpmon
udpmon: udpmon.o $(includedir)/pkt_io_udp.h  $(includedir)/orph_udp.h
	$(CC) $(CFLAGS) -o udpmon udpmon.c  $(libdir)/vmelib.a

install-udpmon: udpmon
	install udpmon $(bindir)

#
#
######vmehardware
LIBS= $(libdir)/vmelib.a
INCS= $(includedir)/pkt_io_udp.h $(includedir)/mem_mgr.h

vmehardware: vmehardware.c $(includedir)/pkt_io_udp.h $(includedir)/mem_mgr.h
	$(CC) $(CFLAGS) -o vmehardware vmehardware.c $(libdir)/vmelib.a

install-vmehardware: vmehardware
	install vmehardware $(bindir)

#########Clean up targets

clean: 
	rm -f *.o
	rm -f pacor/pacor pacor/lib/pacorlib.a pacor/*.o
	rm -f acq_run 
	rm -f caenr 
	rm -f caenw 
	rm -f cnaf 
	rm -f docnaf 
	rm -f femsg 
	rm -f femsg-stdout 
	rm -f fifo 
	rm -f ipcmon 
	rm -f logger 
	rm -f m_setup 
	rm -f udpmon 
	rm -f vmehardware

.PHONY: clean
