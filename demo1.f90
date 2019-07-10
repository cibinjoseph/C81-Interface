! This demo program reads the C81 file sample1.C81 and writes the values read to
! an output file sampleOutput.C81. Both files are in the Samples/ directory.
! The working demo for the getTable() function, similar to dlmread() in Matlab, is also provided

program demo1

  use libC81
  implicit none

  integer, parameter :: rows = 4
  integer, parameter :: cols = 4

  type(C81_class) :: C81
  real, dimension(4,4) :: A
  integer :: i,j


  ! Read airfoil data from C81 file
  call C81%readfile('Samples/sample1.C81')
  print*, 'Airfoil data read SUCCESSFUL'
  print*

  ! Write airfoil data to C81 file
  call C81%writefile('Samples/sampleOutput.C81')
  print*, 'Airfoil data write SUCCESSFUL'
  print*

  ! Read tabular data from csv file to Fortran array
  ! Works similar to dlmread() from Matlab
  ! Useful for creating arrays from airfoil data in CSV format
  A = getTable('Samples/sample1.csv',rows,cols)
  print*, 'Tabular data read SUCCESSFUL'
  print*

  do i=1,size(A,1)
    print*,(A(i,j),j=1,size(A,2))
  enddo
  print*

  ! Find left and right indices from 1-d sorted array in which
  ! a queried value lies
  print*, A(1,:)
  print*, getInterval(A(1,:),0.15)
  print*, 'Binary search SUCCESSFUL'

  ! Return 2-d interpolated value from 2-d array
  print*
  print*, C81%getCL(-14.5,0.425)
  print*, '2-d interpolation SUCCESSFUL'

end program demo1

