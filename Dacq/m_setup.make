#
DIRA= /usr/hhirf/
#
CFLAGS= -Wall
LIBS= $(DIRA)orphlib.a ../lib/vmelib.a ../lib/vmexxlib.a
OBJS=
m_setup: m_setup.c $(OBJS) $(LIBS) $(INCS)
	gcc $(CFLAGS) m_setup.c $(OBJS) $(LIBS) -o m_setup
install: m_setup
	install m_setup ../bin
