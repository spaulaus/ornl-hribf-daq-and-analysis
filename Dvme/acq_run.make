LIBS= ../Dacq/vmelib.a
INCS= ../Dacq/pkt_io.h  ../Dlan/orph_pf.h acq_ctl.h
CFLAGS = -O2
OBJECTS =

acq_run: acq_run.c  $(OBJECTS) $(INCS) $(LIBS)
	gcc $(CFLAGS) -o acq_run acq_run.c $(OBJECTS) $(LIBS)

