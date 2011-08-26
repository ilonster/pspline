#GNU Makefile to build Library libcdf_dummy.a 
#
#02/01/06 C. Ludescher-Furth
#
ifneq ($(MAKELEVEL),0)
# if OBJ was defined for main Makefile one level down
ifeq ("${OBJ}",".")
	override OBJ=..
endif
endif

# define system dependent flags, etc.
-include ../share/Make.local		
include ../share/Make.flags


ifdef DEBUG
	FFLAGS = $(DFFLAGS)
endif


ARC = $(OBJ)/lib/libcdf_dummy.a
OBJDIR = $(OBJ)/obj/cdf_dummy

#=============================================================
 
# fortran archive members
F = $(subst .f,.o, $(wildcard *.f)) $(subst .F,.o, $(wildcard *.F)) $(subst .f90,.o, $(wildcard *.f90))
C =  $(subst .c,.o,$(wildcard *.c))
MEMS = $(C) $(F)
MEM = $(foreach file,$(MEMS),$(ARC)($(file)))


.PHONY: clean realclean checkdir all exec

libs: FORCE $(ARC)
 
all:  libs 
	@echo done  

$(OLDLIB): timestamp.inf
	@echo "--- DETECTED cdf_dummy.a source update"
	@echo "--- Re-Making $(ARC)"
	@$(MAKE) libs

FORCE:
	@test -d $(OBJ)/lib || mkdir -p $(OBJ)/lib
	@test -d $(OBJ)/obj/cdf_dummy || mkdir -p $(OBJ)/obj/cdf_dummy

 
# compile c
$(ARC)(%.o): %.c
	$(c_proc)
	$(make_ar)

# compile fixed fortran 
$(ARC)(%.o): %.f
	$(FC) $(FFLAGS) -I./ $(INCFLAGS) $(OUT)  $(OBJDIR)/$*.o $<
	$(make_ar)

# compile free fortran 
$(ARC)(%.o): %.f90
	$(FC) $(FFLAGS) -I./ $(INCFLAGS) $(OUT)  $(OBJDIR)/$*.o $<
	$(make_ar)

# pre-process/compile fixed fortran code
$(ARC)(%.o): %.F
	$(F_proc)
	$(make_ar)

$(ARC): $(MEM)
	@$(RANLIB) $@ > /dev/null
#
#---------------------------------------------------
exec:
	@echo 

clean: 
	@rm -f $(OBJDIR)/*

realclean: clean
	@rm -f $(ARC)

install:
	@test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	@if test -f $(ARC); then \
	  umask 133; cp $(ARC) $(LIBDIR)/; \
	else test -f $(LIBDIR)/libcdf_dummy.a || echo ">>> can not find $(ARC)";fi

uninstall:
	rm -f $(LIBDIR)/libcdf_dummy.a
