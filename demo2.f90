! This demo program reads the naca6403_Re20k.csv and writes it to
! an output file naca6403_Re20k.C81. Both files are in the Samples/ directory.

program demo2

  use libC81
  implicit none

  integer, parameter :: rows = 25
  integer, parameter :: cols = 4
  integer, parameter :: nMach = 3
  integer :: i,j

  type(C81_class) :: C81
  real, dimension(rows,cols) :: A


  ! Read airfoil data from CSV file
  A=getTable('Samples/GAW-2.csv',rows,cols)

  ! Allocate arrays
  allocate(C81%MaL(nMach))
  allocate(C81%MaD(nMach))
  allocate(C81%MaM(nMach))
  allocate(C81%AL(rows-1))
  allocate(C81%AD(rows-1))
  allocate(C81%AM(rows-1))
  allocate(C81%CL(rows-1,nMach))
  allocate(C81%CD(rows-1,nMach))
  allocate(C81%CM(rows-1,nMach))

  ! Specify airfoil name
  C81%airfoilName = 'GA(W)-2'

  ! Copy values from read array to variables
  C81%MaL = A(1,2:)
  C81%MaD = C81%MaL
  C81%MaM = C81%MaL

  C81%AL = A(2:,1)
  C81%AD = C81%AL
  C81%AM = C81%AL

  do j=2,cols
    do i=2,rows
      C81%CL(i-1,j-1) = A(i,j)
    enddo
  enddo

  C81%CD = C81%CL
  C81%CM = C81%CL

  ! Write airfoil data to C81 file
  call c81%writefile('Samples/GAW-2.C81')

end program demo2
