#
# $Id: Makefile,v 1.1.1.1 2010/10/08 07:12:32 kunze Exp $
#
PROG     = lib$(PROGN).a
INCL     = ../include

include $(INCL)/makeobjs.incl
include $(INCL)/make_$(SYS).incl

AR       = ar
ARFLAGS  = -crv

all: $(PROG)

$(PROG): $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

clean:
	rm -f $(OBJS) $(MODDIR)/*.mod 

realclean: clean
	rm -f $(PROG)

#- Secondary targets  ---------------------------------------------

.SUFFIXES: .c .cc .f .F .f90 .F90 .for

.c.o:
	$(CC) $(CFLAGS) -c $< \
	-o `dirname $<`/`basename $< .c`.o

.f.o:
	$(F77) $(FFLAGS) -c $< \
	-o `dirname $<`/`basename $< .f`.o

.for.o:
	$(F77) $(FFLAGS) -c $< \
	-o `dirname $<`/`basename $< .for`.o

.F.o:
	$(F77) $(FPPFLAGS) $(FFLAGS) -c $< -o `basename $< .F`.o 

.f90.o:
	$(FPP) $(FPPFLAGS) -c $< \
	-o `dirname $<`/`basename $< .f90`.o

.F90.o:
	$(FPP) $(FPPFLAGS) -c $< \
	-o `dirname $<`/`basename $< .F90`.o
#- Dependencies ---------------------------------------------------

include $(INCL)/make_dep.incl

