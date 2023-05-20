!██████╗░██████╗░░█████╗░░██████╗░██████╗░░█████╗░███╗░░░███╗  ██╗███╗░░██╗██╗
!██╔══██╗██╔══██╗██╔══██╗██╔════╝░██╔══██╗██╔══██╗████╗░████║  ██║████╗░██║██║
!██████╔╝██████╔╝██║░░██║██║░░██╗░██████╔╝███████║██╔████╔██║  ██║██╔██╗██║██║
!██╔═══╝░██╔══██╗██║░░██║██║░░╚██╗██╔══██╗██╔══██║██║╚██╔╝██║  ██║██║╚████║██║
!██║░░░░░██║░░██║╚█████╔╝╚██████╔╝██║░░██║██║░░██║██║░╚═╝░██║  ██║██║░╚███║██║
!╚═╝░░░░░╚═╝░░╚═╝░╚════╝░░╚═════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚═╝░░░░░╚═╝  ╚═╝╚═╝░░╚══╝╚═╝

!███╗░░░███╗██╗██╗░░░░░██╗██╗░░██╗
!████╗░████║██║██║░░░░░██║██║░██╔╝
!██╔████╔██║██║██║░░░░░██║█████═╝░
!██║╚██╔╝██║██║██║░░░░░██║██╔═██╗░
!██║░╚═╝░██║██║███████╗██║██║░╚██╗
!╚═╝░░░░░╚═╝╚═╝╚══════╝╚═╝╚═╝░░╚═╝

!░█████╗░██╗░░██╗███╗░░░███╗░█████╗░██████╗░  ██████╗░░█████╗░░██████╗██╗░░░██╗██╗██████╗░
!██╔══██╗██║░░██║████╗░████║██╔══██╗██╔══██╗  ██╔══██╗██╔══██╗██╔════╝╚██╗░██╔╝██║██╔══██╗
!███████║███████║██╔████╔██║███████║██║░░██║  ██████╦╝███████║╚█████╗░░╚████╔╝░██║██████╔╝
!██╔══██║██╔══██║██║╚██╔╝██║██╔══██║██║░░██║  ██╔══██╗██╔══██║░╚═══██╗░░╚██╔╝░░██║██╔══██╗
!██║░░██║██║░░██║██║░╚═╝░██║██║░░██║██████╔╝  ██████╦╝██║░░██║██████╔╝░░░██║░░░██║██║░░██║
!╚═╝░░╚═╝╚═╝░░╚═╝╚═╝░░░░░╚═╝╚═╝░░╚═╝╚═════╝░  ╚═════╝░╚═╝░░╚═╝╚═════╝░░░░╚═╝░░░╚═╝╚═╝░░╚═╝

!███╗░░██╗░█████╗░░░░░░██╗░██╗░░░░░░░██╗░█████╗░███╗░░██╗
!████╗░██║██╔══██╗░░░░░██║░██║░░██╗░░██║██╔══██╗████╗░██║
!██╔██╗██║███████║░░░░░██║░╚██╗████╗██╔╝███████║██╔██╗██║
!██║╚████║██╔══██║██╗░░██║░░████╔═████║░██╔══██║██║╚████║
!██║░╚███║██║░░██║╚█████╔╝░░╚██╔╝░╚██╔╝░██║░░██║██║░╚███║
!╚═╝░░╚══╝╚═╝░░╚═╝░╚════╝░░░░╚═╝░░░╚═╝░░╚═╝░░╚═╝╚═╝░░╚══╝

program integral_tugas_07
implicit none
  integer, parameter :: dpr=kind(1.0D0)
  integer :: i, k, metode, status, size
  real(dpr), allocatable :: D(:,:), Y(:), X(:)
  real(dpr) :: batas_atas, batas_bawah, I_numerik 
  character(len=20), dimension(2) :: list_nama_metode 
  character(len=20) :: output_integral, filename

  list_nama_metode(1) = 'trapezoid'
  list_nama_metode(2) = 'simpson'

! nama file input
filename = "input_matrix.txt"

! membuka dan menghitung ukuran file input
open(unit=1, file=filename, status="old", action="read", iostat=status)
if (status /= 0) then
    write(*,*) "Error opening the input file"
    stop
end if

! ukuran file input
size = 0
do
    read(1, *, iostat=status)
    if (status /= 0) exit
    size = size + 1
end do
write(*,*) 'jumlah data input =', size

allocate(D(1:size,1:2),X(1:size),Y(1:size))
write(*,*) ' '

rewind(1)

! membaca data input 
do i=1,size
    read(1,*,end=2) D(i,:)
    X(i)=D(i,1) !data X
    Y(i)=D(i,2) !data Y
2 end do
close(1)

!menghitung nilai integral
 write(*,*) 'masukkan batas bawah dan batas atas integrasi'
 write(*,*) 'batas atas ='
 read(*,*) batas_atas
 write(*,*) 'batas bawah ='
 read(*,*) batas_bawah

20 write(*,*) 'masukkan jumlah titik integrasi'
 read(*,*) k

 !memilih metode integrasi
 write(*,*) 'pilih metode integrasi'
 write(*,*) 'ketik 1 untuk menggunakan metode komposit trapezoid'
 write(*,*) 'ketik 2 untuk menggunakan metode komposit simpson'
10 read(*,*) metode

 if (metode==1) then 
    I_numerik = integral_trapezoid(batas_bawah,batas_atas,k,X,Y,size)
    output_integral = list_nama_metode(1)
 else if (metode==2) then
         if (mod(k, 2) == 1) then
         write(*,*) 'Untuk metode integrasi simpson, jumlah titik integrasi harus genap!' 
         goto 20
         else 
         goto 30
         end if
    30 I_numerik = integral_simpson(batas_bawah,batas_atas,k,X,Y,size)
    output_integral = list_nama_metode(2)
 else 
    write(*,*) 'metode tidak valid, ulangi' 
    goto 10
end if

open(unit=2, file='output_integral_tugas_07.txt', status='replace', action='write')
write(2,*) 'metode integrasi =','  ', output_integral
write(2,*) 'jumlah titik integrasi =', k
write(2,*) 'hasil numerik =', I_numerik
close(2)

write(*,*) 'perhitungan berhasil, cek hasil di file output_integral_tugas_07.txt'

deallocate(D,X,Y)

stop

contains

function interpolasi_hermite_kubik(XX,YY,x0,n,size) result(yhk)
integer :: i, j, k, ll, in_min
integer, intent(in) :: n, size
real(dpr), intent(in) :: XX(size), YY(size), x0(n)
real(dpr) :: b, m
real(dpr) :: h1(n,2), h2(n,2), h(n,4), yhk(n), X(size+2), Y(size+2)

! melakukan interpolasi lagrange kubik
! inisiasi nilai
do i = 1,size+2
   X(i) = 0.0
   Y(i) = 0.0
end do 

do i = 1,size
   X(i+1) = XX(i)
   Y(i+1) = YY(i)
end do

do k = 1,n
   ! mengecek posisi x0(k)
   do ll = 2, size+1
      if (x0(k) >= X(ll) .and. x0(k) <= X(ll+1)) then
         in_min = ll-1
         exit
      end if
   end do
   ! didapat indeks minimum in_min
   
   ! menghitung h1(k,i) dan h2(k,i)
   h1(k,1) = (1-2*(x0(k)-X(in_min+1))/(X(in_min+1)-X(in_min+2)))*((x0(k)-X(in_min+2))/(X(in_min+1)-X(in_min+2)))**2 
   h1(k,2) = (1-2*(x0(k)-X(in_min+2))/(X(in_min+2)-X(in_min+1)))*((x0(k)-X(in_min+1))/(X(in_min+2)-X(in_min+1)))**2
   h2(k,1) = (x0(k)-X(in_min+1))*((x0(k)-X(in_min+2))/(X(in_min+1)-X(in_min+2)))**2
   h2(k,2) = (x0(k)-X(in_min+2))*((x0(k)-X(in_min+1))/(X(in_min+2)-X(in_min+1)))**2
   
   ! menghitung h(k,i)
   h(k,1) = h2(k,1)*((X(in_min+1)-X(in_min+2))/(X(in_min+0)-X(in_min+2)))/(X(in_min+0)-X(in_min+1))
   h(k,2) = h1(k,1) + h2(k,1)*(1/(X(in_min+1)-X(in_min+2)) + 1/(X(in_min+1)-X(in_min+0))) &
          + h2(k,2)*((X(in_min+2)-X(in_min+3))/(X(in_min+1)-X(in_min+3)))/(X(in_min+1)-X(in_min+2))
   h(k,3) = h1(k,2) + h2(k,2)*(1/(X(in_min+2)-X(in_min+1)) + 1/(X(in_min+2)-X(in_min+3))) &
          + h2(k,1)*((X(in_min+1)-X(in_min+0))/(X(in_min+2)-X(in_min+0)))/(X(in_min+2)-X(in_min+1))
   h(k,4) = h2(k,2)*((X(in_min+2)-X(in_min+1))/(X(in_min+3)-X(in_min+1)))/(X(in_min+3)-X(in_min+2))

   ! menghitung pasangan titik dari x0 yaitu yhk
   yhk(k) = 0.0
   do i = in_min,in_min+3
      yhk(k) = yhk(k) + h(k,i+1-in_min) * Y(i)
   end do

end do

end function interpolasi_hermite_kubik

function integral_trapezoid(batas_bawah, batas_atas, k, X, Y, size) result(I_numerik)
    real(dpr), intent(in) :: batas_atas, batas_bawah
    real(dpr), dimension(:), allocatable :: f, x0
    real(dpr) :: I_numerik, h
    integer, intent(in) :: k, size
    integer :: i
    real(dpr), intent(in) :: X(size), Y(size)

    allocate(f(k+1),x0(k+1))

    h = (batas_atas - batas_bawah) / k
    do i = 1,k+1
       x0(i) = batas_bawah + (i - 1)*h
    end do
    
    f = interpolasi_hermite_kubik(X,Y,x0,k+1,size)

    I_numerik = (h / 2) * (f(1) + f(k + 1))

    do i = 1, k - 1
        I_numerik = I_numerik + h * f(i + 1)
    end do

    deallocate(f)

end function integral_trapezoid

  function integral_simpson(batas_bawah,batas_atas,k,X,Y,size) result(I_numerik)
    real(dpr), intent(in) :: batas_atas, batas_bawah
    real(dpr) :: I_numerik, h
    real(dpr) :: f(k+1),x0(k+1)
    integer, intent(in) :: k, size
    integer :: i,NN
    real(dpr), intent(in) :: X(size), Y(size)

    NN = k/2

    h = (batas_atas-batas_bawah)/k

    do i = 1,2*NN+1
       x0(i) = batas_bawah + (i - 1)*h
    end do

    f = interpolasi_hermite_kubik(X,Y,x0,k+1,size)

    I_numerik = (h/3)*(f(1)+f(2*NN+1))

    do i = 1,NN
    I_numerik = I_numerik + (4*h/3)*f(2*i)
    end do

    do i = 1,NN-1
    I_numerik = I_numerik + (2*h/3)*f(2*i+1)
    end do

  end function integral_simpson

end program integral_tugas_07