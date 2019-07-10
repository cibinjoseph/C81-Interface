! This demo program reads the naca6403_Re20k.csv and writes it to
! an output file naca6403_Re20k.C81. Both files are in the Samples/ directory.

program demo2

  use libC81
  implicit none

  integer, parameter :: rows = 28
  integer, parameter :: cols = 2
  integer, parameter :: nMach = 3
  integer :: i

  type(C81_class) :: C81
  real, dimension(rows,cols) :: A


  ! Read airfoil data from CSV file
  A=getTable('Samples/naca6403_Re20k.csv',rows,cols)

  ! Allocate arrays
  allocate(C81%MaL(nMach))
  allocate(C81%MaD(nMach))
  allocate(C81%MaM(nMach))
  allocate(C81%AL(rows))
  allocate(C81%AD(rows))
  allocate(C81%AM(rows))
  allocate(C81%CL(rows,nMach))
  allocate(C81%CD(rows,nMach))
  allocate(C81%CM(rows,nMach))

  ! Specify airfoil name
  C81%airfoilName = 'NACA6403_Re20k'

  ! Copy values from read array to variables
  C81%MaL = (/0.0, 0.5, 0.9/)
  C81%MaD = C81%MaL
  C81%MaM = C81%MaL

  C81%AL = A(:,1)
  C81%AD = C81%AL
  C81%AM = C81%AL

  do i=1,nMach
    C81%CL(:,i) = A(:,2)
  enddo

  C81%CD = C81%CL
  C81%CM = C81%CL

  ! Write airfoil data to C81 file
  call c81%writefile('Samples/naca6403_Re20k.C81')

end program demo2
