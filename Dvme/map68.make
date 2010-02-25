LIBS= /usr/hhirf/orphlib.a
INCS=
OBJECTS =  map68.f

map68f:  $(OBJECTS) $(INCS) $(LIBS)
	lf95 -o map68f $(OBJECTS) $(LIBS)

