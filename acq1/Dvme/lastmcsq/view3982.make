LIBS=../Dacq/vmelib.a /usr/hhirf/orphlib.a

view3982: view3982.f  $(LIBS)
	f77 -O4 -o view3982 view3982.f $(OBJECTS) $(LIBS)

