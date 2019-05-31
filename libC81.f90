module libC81
  implicit none
contains

  subroutine readC81(C81filename,airfoil_name,MaL,AL,CL,MaD,AD,CD,MaM,AM,CM)
    character(len=*), intent(in) :: C81filename
    character(len=30), intent(out) :: airfoil_name
    real, allocatable, intent(out), dimension(:) :: MaL, MaD, MaM
    real, allocatable, intent(out), dimension(:) :: AL, AD, AM
    real, allocatable, intent(out), dimension(:,:) :: CL, CD, CM
    integer :: ML, NL, MD, ND, MM, NM
    integer :: i, j
    character(len=10) :: formatChar

    open(unit=10,file=C81filename)
    read(10,100) airfoil_name,ML,NL,MD,ND,MM,NM
    allocate(MaL(ML))
    allocate(MaD(MD))
    allocate(MaM(MM))
    allocate(AL(NL))
    allocate(AD(ND))
    allocate(AM(NM))
    allocate(CL(NL,ML))
    allocate(CD(ND,MD))
    allocate(CM(NM,MM))

    ! Lift
    read(10,101) (MaL(i),i=1,min(9,ML))
    if (ML>9) then 
      write(formatChar,'(A4,I1,A5)') '(7X,',ML-9,'F7.0)'
      read(10,formatChar) (MaL(i),i=10,ML)
    endif
    do i=1,NL
      read(10,102) AL(i), (CL(i,j),j=1,min(9,ML))
      if (ML>9)  read(10,formatChar) (CL(i,j),j=10,ML)
    enddo

    ! Drag
    read(10,101) (MaD(i),i=1,min(9,MD))
    if (MD>9) then 
      write(formatChar,'(A4,I1,A5)') '(7X,',MD-9,'F7.0)'
      read(10,formatChar) (MaD(i),i=10,MD)
    endif
    do i=1,ND
      read(10,102) AD(i), (CD(i,j),j=1,min(9,MD))
      if (MD>9)  read(10,formatChar) (CD(i,j),j=10,MD)
    enddo

    ! Moment
    read(10,101) (MaM(i),i=1,min(9,MM))
    if (MM>9) then 
      write(formatChar,'(A4,I1,A5)') '(7X,',MM-9,'F7.0)'
      read(10,formatChar) (MaM(i),i=10,MM)
    endif
    do i=1,NM
      read(10,102) AM(i), (CM(i,j),j=1,min(9,MM))
      if (MM>9)  read(10,formatChar) (CM(i,j),j=10,MM)
    enddo

    close(10)

    100 format (A30,6I2)
    101 format (7X,9F7.0)
    102 format (10F7.0)
  end subroutine readC81

  subroutine writeC81(C81filename,airfoil_name,MaL,AL,CL,MaD,AD,CD,MaM,AM,CM)
    character(len=*), intent(in) :: C81filename
    character(len=30), intent(in) :: airfoil_name
    real, intent(in), dimension(:) :: MaL, MaD, MaM
    real, intent(in), dimension(:) :: AL, AD, AM
    real, intent(in), dimension(:,:) :: CL, CD, CM
    integer :: ML, NL, MD, ND, MM, NM
    integer :: i, j
    character(len=10) :: formatChar

    open(unit=10,file=C81filename)
    ML = size(MaL,1)
    MD = size(MaD,1)
    MM = size(MaM,1)
    NL = size(AL,1)
    ND = size(AD,1)
    NM = size(AM,1)

    write(10,100) airfoil_name,ML,NL,MD,ND,MM,NM
    ! Lift
    write(10,101) (MaL(i),i=1,min(9,ML))
    if (ML>9) then 
      write(formatChar,'(A4,I1,A5)') '(7X,',ML-9,'F7.3)'
      write(10,formatChar) (MaL(i),i=10,ML)
    endif
    do i=1,NL
      write(10,102) AL(i), (CL(i,j),j=1,min(9,ML))
      if (ML>9)  write(10,formatChar) (CL(i,j),j=10,ML)
    enddo

    ! Drag
    write(10,101) (MaD(i),i=1,min(9,MD))
    if (MD>9) then 
      write(formatChar,'(A4,I1,A5)') '(7X,',MD-9,'F7.3)'
      write(10,formatChar) (MaD(i),i=10,MD)
    endif
    do i=1,ND
      write(10,102) AD(i), (CD(i,j),j=1,min(9,MD))
      if (MD>9)  write(10,formatChar) (CD(i,j),j=10,MD)
    enddo

    ! Moment
    write(10,101) (MaM(i),i=1,min(9,MM))
    if (MM>9) then 
      write(formatChar,'(A4,I1,A5)') '(7X,',MM-9,'F7.3)'
      write(10,formatChar) (MaM(i),i=10,MM)
    endif
    do i=1,NM
      write(10,102) AM(i), (CM(i,j),j=1,min(9,MM))
      if (MM>9)  write(10,formatChar) (CM(i,j),j=10,MM)
    enddo

    close(10)

    100 format (A30,6I2)
    101 format (7X,9F7.3)
    102 format (F7.2,9F7.3)
  end subroutine writeC81


end module libC81
