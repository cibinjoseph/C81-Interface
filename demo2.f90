! This demo program reads the naca6403_Re20k.csv and writes it to
! an output file naca6403_Re20k.C81. Both files are in the Samples/ directory.

program demo2

  use libC81
  implicit none

  integer, parameter :: rows = 28
  integer, parameter :: cols = 2
  integer, parameter :: nMach = 3
  integer :: i

  character(len=30) :: airfoil_name
  real, allocatable, dimension(:) :: MaL, MaD, MaM
  real, allocatable, dimension(:) :: AL, AD, AM
  real, allocatable, dimension(:,:) :: CL, CD, CM
  real, dimension(rows,cols) :: A


  ! Read airfoil data from CSV file
  A=getTable('Samples/naca6403_Re20k.csv',rows,cols)

  ! Allocate arrays
  allocate(MaL(nMach))
  allocate(MaD(nMach))
  allocate(MaM(nMach))
  allocate(AL(rows))
  allocate(AD(rows))
  allocate(AM(rows))
  allocate(CL(rows,nMach))
  allocate(CD(rows,nMach))
  allocate(CM(rows,nMach))

  ! Specify airfoil name
  airfoil_name = 'NACA6403_Re20k'

  ! Copy values from read array to variables
  MaL = (/0.0, 0.5, 0.9/)
  MaD = MaL
  MaM = MaL

  AL = A(:,1)
  AD = AL
  AM = AL

  do i=1,nMach
    CL(:,i) = A(:,2)
  enddo

  CD = CL
  CM = CL

  ! Write airfoil data to C81 file
  call writeC81('Samples/naca6403_Re20k.C81',airfoil_name,MaL,AL,CL,MaD,AD,CD,MaM,AM,CM)

end program demo2
