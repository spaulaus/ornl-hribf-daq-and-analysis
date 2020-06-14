#
FFLAGS= -O0 -fno-automatic
#
OBJS= amass.o  
#
LIBS= /tera/milner/DDgnu/Dorphlib/orphlib.a
#
amass: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o amass
.f.o:
	f77 $(FFLAGS) -c $<
