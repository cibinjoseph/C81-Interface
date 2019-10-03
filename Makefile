FC = gfortran

all:
	make lib
	make demo

lib:
	$(FC) -c libC81.f90

demo1:
	make lib
	$(FC) demo1.f90 libC81.o -o demo1.out

demo2:
	make lib
	$(FC) demo2.f90 libC81.o -o demo2.out

demo3:
	make lib
	$(FC) demo3.f90 libC81.o -o demo3.out

change:
	@make -s demo2
	@yes y | ./demo2.out
	@diff -q prevCopy Samples/naca6403_Re20k.C81 || exit 0

clean:
	rm -f *.o *.mod *.out
