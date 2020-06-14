#
# Use this Makefile to compile a scan for online data analysis
# This version is for gfortran compilers
# The -L/sw/lib is special for Mac OS X FINK users.  Please modify
# needed.
# Using this requires that the data acquisition system be installed.
# If you are not monitoring a data acquisition, use SCANOF instead.
#
FFLAGS= -O0 -fno-automatic -fsecond-underscore -fno-range-check
#
OBJS= /usr/hhirf/scanor.o\
      scanusubs.o
#
LIBS= /usr/hhirf/scanorlib.a\
      /usr/hhirf/orphlib.a\
      /usr/acq2/lib/acqlib.a\
      /usr/acq2/lib/ipclib.a

#
scanor: $(OBJS) $(LIBS)
#
	gfortran $(FFLAGS) $(OBJS) $(LIBS)  -o scanor
.f.o:
	gfortran $(FFLAGS) -c $<
