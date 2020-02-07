FC = gfortran

all: libC81.o demo1 demo2 demo3

libC81.o: libC81.f90
	$(FC) -c libC81.f90

demo1.out: demo1.f90 libC81.o
	$(FC) demo1.f90 libC81.o -o demo1.out

demo2.out: demo2.f90 libC81.o
	$(FC) demo2.f90 libC81.o -o demo2.out

demo3.out: demo3.f90 libC81.o
	$(FC) demo3.f90 libC81.o -o demo3.out

changes:
	@make -s demo2
	@yes y | ./demo2.out
	@diff -q prevCopy Samples/naca6403_Re20k.C81 || exit 0

docum: ford_input.md
	rm -rf docs/ford_ouput/src
	ford -p docs/ford_input ford_input.md

clean:
	rm -f *.o *.mod *.out
