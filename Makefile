FC = gfortran

all:
	make lib
	make demo

lib:
	$(FC) -c libC81.f90

demo:
	make lib
	$(FC) demo.f90 libC81.o -o demo.out

clean:
	rm -f *.o *.mod *.out
