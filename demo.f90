program demo
  use libC81
  implicit none

  character(len=30) :: airfoil_name
  real, allocatable, dimension(:) :: MaL, MaD, MaM
  real, allocatable, dimension(:) :: AL, AD, AM
  real, allocatable, dimension(:,:) :: CL, CD, CM

  call readC81('sample1.C81',airfoil_name,MaL,AL,CL,MaD,AD,CD,MaM,AM,CM)
  call writeC81('sample_out.C81',airfoil_name,MaL,AL,CL,MaD,AD,CD,MaM,AM,CM)


end program demo
