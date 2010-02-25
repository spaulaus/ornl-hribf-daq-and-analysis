#
FFLAGS= -O0
#
OBJS= scadr.o
#
LIBS= /tera/milner/DDgnu/Dscadrlib/scadrlib.a\
      /tera/milner/DDgnu/Dorphlib/orphlib.a\
      /usr/acq/vme/vmelib.a
#
scadr: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o scadr
.f.o:
	f77 $(FFLAGS) -c $<
