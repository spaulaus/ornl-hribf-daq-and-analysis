#
FFLAGS= -O0 -fno-automatic
#
OBJS= focomp.o 
#
LIBS= /tera/milner/DDgnu/Dorphlib/orphlib.a
#
focomp: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o focomp
.f.o:
	f77 $(FFLAGS) -c $<
