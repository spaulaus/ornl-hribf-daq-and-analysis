#
CFLAGS= -g
DIRA= /usr/hhirf/
#
LIBS= $(DIRA)orphlib.a vmelib.a ../Dvme/vmexxlib.a
#LIBS= $(DIRA)orphlib.a ../Dvme/vmexxlib.a
OBJS =
tesla: m_setup.c $(OBJS) $(LIBS) $(INCS)
	gcc $(CFLAGS) m_setup.c $(OBJS) $(LIBS) -o m_setup
