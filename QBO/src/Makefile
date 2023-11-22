#
#
FPP      = gfortran -cpp
#FPP      = ifort -cpp
#FPPFLAGS = -O0 -g
#FPPFLAGS = -O

INSTALL_DIR = ../bin

PROG1 = file_add
OBJS1 = $(PROG1).o
SRCS1 = $(PROG1).f90

PROG2 = gen_indata
OBJS2 = $(PROG2).o
SRCS2 = $(PROG2).f90

PROG3 = parse_wind
OBJS3 = $(PROG3).o
SRCS3 = $(PROG3).f90

PROG4 = interpolate_qbo
OBJS4 = $(PROG4).o
SRCS4 = $(PROG4).f90


all: $(PROG1) $(PROG2) $(PROG3) $(PROG4)

$(PROG1): $(OBJS1)
	$(FPP) -o $@ $(OBJS1)
$(PROG2): $(OBJS2)
	$(FPP) -o $@ $(OBJS2)
$(PROG3): $(OBJS3)
	$(FPP) -o $@ $(OBJS3)
$(PROG4): $(OBJS4)
	$(FPP) -o $@ $(OBJS4)

clean:
	rm $(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4)

install: all
	cp -p $(PROG1) $(PROG2) $(PROG3) $(PROG4) $(INSTALL_DIR)



#- Secondary targets  ---------------------------------------------

.SUFFIXES: .f90

.f90.o:
	$(FPP) $(FPPFLAGS) -c $< -o `dirname $<`/`basename $< .f90`.o
