INCS=../Dlan/orph_pf.h ../Dvme/mem_mgr.h
CFLAGS = -O2

load_acq_params.o: load_acq_params.c  $(INCS)
	$(CC) $(CFLAGS) -c load_acq_params.c

