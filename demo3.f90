! This demo program reads the lift.csv and drag.csv files and writes it to
! an output file output.C81. Both files are in the Samples/ directory.

program demo3

  use libC81
  implicit none

  integer, parameter :: rowsL = 25
  integer, parameter :: colsL = 4
  integer, parameter :: nMachL = 3

  integer, parameter :: rowsD = 25
  integer, parameter :: colsD = 4
  integer, parameter :: nMachD = 3

  integer :: i,j

  type(C81_class) :: C81
  real, dimension(rowsL,colsL) :: LMat
  real, dimension(rowsD,colsD) :: DMat


  ! Read airfoil lift data from CSV file
  LMat=getTable('Samples/lift.csv',rowsL,colsL)

  ! Read airfoil drag data from CSV file
  DMat=getTable('Samples/drag.csv',rowsD,colsD)

  ! Allocate arrays
  allocate(C81%MaL(nMachL))
  allocate(C81%MaD(nMachD))
  allocate(C81%MaM(nMachL))
  allocate(C81%AL(rowsL-1))
  allocate(C81%AD(rowsD-1))
  allocate(C81%AM(rowsL-1))
  allocate(C81%CL(rowsL-1,nMachL))
  allocate(C81%CD(rowsD-1,nMachD))
  allocate(C81%CM(rowsL-1,nMachL))

  ! Specify airfoil name
  C81%airfoilName = 'GA(W)-2'

  ! Copy values from read array to variables
  C81%MaL = LMat(1,2:)
  C81%MaD = DMat(1,2:)
  C81%MaM = C81%MaL

  C81%AL = LMat(2:,1)
  C81%AD = DMat(2:,1)
  C81%AM = C81%AL

  do j=2,colsL
    do i=2,rowsL
      C81%CL(i-1,j-1) = LMat(i,j)
    enddo
  enddo

  do j=2,colsD
    do i=2,rowsD
      C81%CD(i-1,j-1) = DMat(i,j)
    enddo
  enddo

  C81%CM = C81%CL

  ! Write airfoil data to C81 file
  call c81%writefile('Samples/output.C81')

end program demo3
