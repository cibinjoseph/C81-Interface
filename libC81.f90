module libC81
  implicit none
contains

  ! Reads from C81 file to allocatable arrays
  subroutine readC81(C81filename,airfoil_name,MaL,AL,CL,MaD,AD,CD,MaM,AM,CM)
    character(len=*), intent(in) :: C81filename
    character(len=30), intent(out) :: airfoil_name
    real, allocatable, intent(out), dimension(:) :: MaL, MaD, MaM
    real, allocatable, intent(out), dimension(:) :: AL, AD, AM
    real, allocatable, intent(out), dimension(:,:) :: CL, CD, CM
    integer :: ML, NL, MD, ND, MM, NM
    integer :: i, j
    integer :: stat
    character(len=10) :: formatChar

    open(unit=10, file=C81filename, status='old', action='read', iostat=stat)
    if (stat>0) error stop 'ERROR: File not found'

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

  ! Writes data arrays to C81 file
  subroutine writeC81(C81filename,airfoil_name,MaL,AL,CL,MaD,AD,CD,MaM,AM,CM)
    character(len=*), intent(in) :: C81filename
    character(len=30), intent(in) :: airfoil_name
    real, intent(in), dimension(:) :: MaL, MaD, MaM
    real, intent(in), dimension(:) :: AL, AD, AM
    real, intent(in), dimension(:,:) :: CL, CD, CM
    integer :: ML, NL, MD, ND, MM, NM
    integer :: i, j
    integer :: stat
    character(len=10) :: formatChar

    open(unit=10,file=C81filename, status='new', action='write', iostat=stat)
    if (stat>0) error stop 'ERROR: File already exists'

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

  ! Gets data from csv formatted file
  function getTable(filename,rows,cols)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: rows, cols
    integer :: i, j
    integer :: stat
    real, dimension(rows,cols) :: getTable

    open(unit=10, file=filename, status='old', action='read', iostat=stat)
    if (stat>0) error stop 'ERROR: File not found'
    do i=1,rows
      read(10,*) (getTable(i,j),j=1,cols)
    enddo
    close(10)
  end function getTable

  ! Returns upper and lower indices of a 1-d sorted array 
  ! using binary search in which a search value lies 
  function getInterval(A,x) result(indx)
    real, intent(in), dimension(:) :: A
    real, intent(in) :: x
    integer, dimension(2) :: indx  ! Left and right indices 
    integer :: n, i

    n = size(A,1)
    indx(1) = 1
    indx(2) = n

    ! Binary search algorithm
    do while ((indx(1) .ne. indx(2)) .and. (indx(2) .ne. (indx(1)+1)))
      i = floor((indx(1)+indx(2))*0.5)
      if (x < A(i)) then
        indx(2) = i
      elseif (x > A(i)) then
        indx(1) = i
      else
        indx(1) = i
        indx(2) = i
      endif
    enddo

    ! Check end cases
    if (abs(A(indx(1))-x) .le. epsilon(1.)) then
      indx(2) = indx(1)
    elseif (abs(A(indx(2))-x) .le. epsilon(1.)) then
      indx(1) = indx(2)
    endif
  end function getInterval

end module libC81
