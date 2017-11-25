BASE = ../
INCLDIR = $(BASE)/include
LIBDIR = $(BASE)/lib
BINDIR = $(BASE)/bin

#FORT = /usr/bin/gfortran
FORT = /sw/bin/gfortran
FFLAGS = -O0 -I./ -I$(INCLDIR) -Wall -fno-automatic -fsecond-underscore -fno-range-check
#FORT = /usr/bin/f77
#FFLAGS = -O0 -I./ -I$(INCLDIR) -fno-automatic -fsecond-underscore
CFLAGS = -O0 -I./ -I$(INCLDIR) -Wall

OBJS = acqipc.o\
       get_acq_key.o\
       fortipc.o\
       acqlog.o\
       acq_tape_ctrl.o\
       acq_log.o\
       messlog.o

acqlib.a: acqlib.a($(OBJS))
	ranlib acqlib.a

acqipc.o: $(INCLDIR)/ipcdefs.h orphmsg.h acqshm.h acqlib.h
get_acq_key.o:  $(INCLDIR)/ipckeys.h
fortipc.o:  $(INCLDIR)/ipcdefs.h acqshm.h acqlib.h
acq_tape_ctrl.o: orphmsg.h
acq_log.o: orphmsg.h
acqlog.o:
messlog.o:

install: acqlib.a
	install acqlib.a $(BASE)/lib 

.PHONY:  clean ran

clean:
	rm -f acqlib.a $(OBJS)

ran:
	ranlib acqlib.a

.f.o:
	$(FORT) $(FFLAGS) -c $<
