FFLAGS = $(OPT) -DPTRSIZE=$(PTRSIZE) $(FARGS)
CFLAGS = $(OPT) $(CARGS) -I $(includedir)

OBJS = acqipc.o\
       get_acq_key.o\
       fortipc.o\
       acqlog.o\
       acq_tape_ctrl.o\
       acq_log.o\
       messlog.o

acqlib.a: acqlib.a($(OBJS))
	ranlib acqlib.a

acqipc.o: $(includedir)/ipcdefs.h orphmsg.h acqshm.h acqlib.h
get_acq_key.o:  $(includedir)/ipckeys.h
fortipc.o:  $(includedir)/ipcdefs.h acqshm.h acqlib.h
acq_tape_ctrl.o: orphmsg.h
acq_log.o: orphmsg.h
acqlog.o:
messlog.o:

install: acqlib.a
	echo $(libdir)
	install acqlib.a $(libdir)

.PHONY:  clean ran

clean:
	rm -f acqlib.a $(OBJS)

ran:
	ranlib acqlib.a

.f.o:
	$(FORT) $(FFLAGS) -c $<
