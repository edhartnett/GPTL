include ../macros.make

FC      = pgfortran
FFLAGS  = -g -O0 -acc -Minfo=accel -Minfo -ta=tesla:cc60 -DENABLE_ACC
CU      = nvcc
CUFLAGS = -UDEBUG -DENABLE_ACC $(UNDERSCORING) -G -O0 -ftz=true -gencode arch=compute_60,code=compute_60 -rdc=true
ARFLAGS = ruv

OBJS = gptl.o util.o f_wrappers.o gptl_acc.o print_gpustats.o

%.o: %.cu
	$(CU) -c $< $(CUFLAGS)

%.o: %.F90
	$(FC) -c $< $(FFLAGS)

all: $(OBJS)
	$(AR) $(ARFLAGS) lib$(GPULIB).a $(OBJS)
	ranlib lib$(GPULIB).a

gptl.o: private.h gptl_acc.h
util.o: private.h
f_wrappers.o: private.h gptl_acc.h
gptl_acc.o: ../devicehost.h
print_gpustats.o: ../devicehost.h private.h

clean:
	$(RM) $(OBJS) *.mod lib$(GPULIB).a
