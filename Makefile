FC = gfortran

all:
	make lib
	make demo

lib:
	$(FC) -c libC81.f90

demo1:
	make lib
	$(FC) demo1.f90 libC81.o -o demo1.out

clean:
	rm -f *.o *.mod *.out
