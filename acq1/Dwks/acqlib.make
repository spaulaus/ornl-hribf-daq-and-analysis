CFLAGS= -g -O
FFLAGS= -g -O -fno-automatic -fsecond-underscore -fno-range-check

OBJS = acqipc.o\
       get_acq_key.o\
       fortipc.o\
       acqlog.o\
       acq_tape_ctrl.o\
       acq_log.o\
       messlog.o

acqlib.a: acqlib.a($(OBJS))
	ranlib acqlib.a

acqipc.o: ../Dshm/ipcdefs.h orphmsg.h acqshm.h acqlib.h
get_acq_key.o: ../Dshm/ipckeys.h
fortipc.o: ../Dshm/ipcdefs.h acqshm.h acqlib.h
acq_tape_ctrl.o: orphmsg.h
acq_log.o: orphmsg.h

.PHONY:  cleanobjs cleanall ran

cleanobjs:
	rm -f $(OBJS)

cleanall:
	rm -f acqlib.a $(OBJS)

ran:
	ranlib acqlib.a

.f.o:
	gfortran $(FFLAGS) -c $<

.c.o:
	gcc $(CFLAGS) -c $<
