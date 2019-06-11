! This demo program reads the C81 file sample1.C81 and writes the values read to
! an output file sampleOutput.C81. Both files are in the Samples/ directory.
! The working demo for the getTable() function, similar to dlmread() in Matlab, is also provided

program demo1

  use libC81
  implicit none

  integer, parameter :: rows = 4
  integer, parameter :: cols = 4

  character(len=30) :: airfoil_name
  real, allocatable, dimension(:) :: MaL, MaD, MaM
  real, allocatable, dimension(:) :: AL, AD, AM
  real, allocatable, dimension(:,:) :: CL, CD, CM
  real, dimension(4,4) :: A


  ! Read airfoil data from C81 file
  call readC81('Samples/sample1.C81',airfoil_name,MaL,AL,CL,MaD,AD,CD,MaM,AM,CM)

  ! Write airfoil data to C81 file
  call writeC81('Samples/sampleOutput.C81',airfoil_name,MaL,AL,CL,MaD,AD,CD,MaM,AM,CM)

  ! Demo of subroutine to read tabular data from csv file to Fortran array
  ! Works similar to dlmread() from Matlab
  ! Useful for creating arrays from airfoil data in CSV format
  A=getTable('Samples/sample1.csv',rows,cols)
  print*,A

end program demo1
