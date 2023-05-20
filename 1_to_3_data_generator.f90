subroutine transform_matrix(A, n, B)
  implicit none
  
  integer, intent(in) :: n       ! Size of the input matrix A
  real, dimension(n, 1), intent(in) :: A   ! Input matrix A
  real, dimension(n, 6), intent(out) :: B  ! Output matrix B
  integer :: i, j, k
  
  ! Transform the input matrix A to output matrix B
  do i = 1, n
    k = int(A(i, 1))             ! Integer value of the array element
    
    ! Assign three integer values around k to the output matrix B
    do j = 1, 6
      B(i, j) = real(k + j - 3)
    end do
  end do
  
end subroutine transform_matrix

program data_generator
  implicit none
  
  integer :: n   ! Size of the array A
  real, allocatable :: A(:,:)  ! Original array
  real, allocatable :: B(:,:)  ! Transformed array
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
  
  ! Call the subprogram to transform matrix A to B
  allocate(B(n, 6))
  call transform_matrix(A, n, B)
  
  ! Save the output matrix B to an external file
  open(unit=file_unit, file="output.txt", status="replace")
  do i = 1, n
    write(file_unit, *) B(i, :)
  end do
  close(unit=file_unit)
  
  print *, "Data transformation complete."
  
  ! Deallocate memory
  deallocate(A)
  deallocate(B)
  
end program data_generator
