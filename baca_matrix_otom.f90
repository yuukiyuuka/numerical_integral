program data_display
  implicit none
  
  integer :: n   ! Size of the array A
  real, allocatable :: A(:,:)  ! Original array
  character(len=50) :: filename  ! File name
  
  integer :: i, j
  integer :: file_unit, status
  
  ! Specify the file name containing the input matrix A
  filename = "input_matrix.txt"
  
  ! Open the input file
  open(unit=file_unit, file=filename, status="old", action="read", iostat=status)
  if (status /= 0) then
    print *, "Error opening the input file"
    stop
  end if
  
  ! Determine the size of the input matrix A
  n = 0
  do
    read(file_unit, *, iostat=status)
    if (status /= 0) exit
    n = n + 1
  end do
  
write(*,*) 'jumlah data =', n

  ! Allocate memory for the input matrix A
  allocate(A(n, 1))
  
  ! Rewind the input file
  rewind(file_unit)
  
  ! Read the input matrix A from the file
  do i = 1, n
    read(file_unit, *) A(i, 1)
  end do
  
  ! Close the input file
  close(unit=file_unit)
  
  ! Display the input matrix A on the terminal
  print *, "Input matrix A:"
  do i = 1, n
    print *, A(i, 1)
  end do
  
  ! Deallocate memory
  deallocate(A)
  
end program data_display

