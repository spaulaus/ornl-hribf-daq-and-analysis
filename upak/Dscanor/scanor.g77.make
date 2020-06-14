#
# Use this Makefile to compile a scan for online data analysis
# This version is for g77 compilers
# Using this requires that the data acquisition system be installed.
# If you are not monitoring a data acquisition, use SCANOF instead.
FFLAGS= -O -fno-automatic
#
OBJS= scanor.o\
      scanusubs.o
#
LIBS= /usr/hhirf/scanorlib.a\
      /usr/hhirf/orphlib.a\
      /usr/acq/wks/acqlib.a\
      /usr/acq/wks/ipclib.a
#
scanor: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o scanor
.f.o:
	f77 $(FFLAGS) -c $<
