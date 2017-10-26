#
#  Makefile
#

#
#  RTEMS_MAKEFILE_PATH is typically set in an environment variable
#

EXEC=vmetest.exe
PGM=${ARCH}/$(EXEC)

# optional managers required
MANAGERS=io event semaphore message

# C source names
CSRCS = init.c cnafxx.c vmexx.c resetxx.c devchk.c testvme.c VMEtest.c \
         mem_mgr.c data_proc.c clk100.c vmemon.c debug.c swaps.c
COBJS_ = $(CSRCS:.c=.o)
COBJS = $(COBJS_:%=${ARCH}/%)

# C++ source names
CXXSRCS =
CXXOBJS_ = $(CXXSRCS:.cc=.o)
CXXOBJS = $(CXXOBJS_:%=${ARCH}/%)

# AS source names
ASSRCS =
ASOBJS_ = $(ASSRCS:.s=.o)
ASOBJS = $(ASOBJS_:%=${ARCH}/%)

# Libraries
LIBS = -lrtemsall -lc

include $(RTEMS_MAKEFILE_PATH)/Makefile.inc
include $(RTEMS_CUSTOM)
include $(PROJECT_ROOT)/make/leaf.cfg

OBJS= $(COBJS) $(CXXOBJS) $(ASOBJS)
#
# (OPTIONAL) Add local stuff here using +=
#

DEFINES  +=
CPPFLAGS +=
CFLAGS   +=  -I../../include 
CFLAGS_LD += -Wl,--defsym -Wl,HeapSize=0xF0000 
CFLAGS_OPTIMIZE_V   +=
CFLAGS_DEBUG_V   += -v -qrtems_debug

LD_PATHS  +=
LD_LIBS   +=



all:    ${ARCH} $(PGM)

$(PGM): $(OBJS)
	$(make-exe)

j:
	@echo $(CFLAGS_DEBUG_V)
	@echo $(RTEMS_USE_GCC272)
