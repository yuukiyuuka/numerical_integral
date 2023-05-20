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

program tugas_06_integral
  implicit none
  integer, parameter :: dpr=kind(1.0D0)
  integer :: i, m, n, k, metode
  real(dpr), allocatable :: A(:)
  real(dpr) :: xmaks, xmin, p, b, konvergensi
  real(dpr) :: batas_atas, batas_bawah, I_analitik, I_numerik, h, kesrel
  character(len=20), dimension(2) :: list_nama_metode 
  character(len=20) :: output_integral

  list_nama_metode(1) = 'trapezoid'
  list_nama_metode(2) = 'simpson'
  
  m = 7
  n = m+1
  
  allocate(A(1:n))
  
  ! membaca data    
  open(unit=1,file='koefisien_pol_orde_7.txt',action='read')
  do i=1,n
      read(1,*) A(i)
  end do
  close(1)

 !menghitung nilai integral Analitik
 write(*,*) 'masukkan batas bawah dan batas atas integrasi'
 write(*,*) 'batas atas ='
 read(*,*) batas_atas
 write(*,*) 'batas bawah ='
 read(*,*) batas_bawah
 I_analitik = anti_pol_7(A,batas_atas,n)-anti_pol_7(A,batas_bawah,n)
 
 !menghitung nilai integral numerik 
20 write(*,*) 'masukkan jumlah titik integrasi'
 read(*,*) k

 !memilih metode integrasi
 write(*,*) 'pilih metode integrasi'
 write(*,*) 'ketik 1 untuk menggunakan metode komposit trapezoid'
 write(*,*) 'ketik 2 untuk menggunakan metode komposit simpson'
10 read(*,*) metode

 if (metode==1) then 
    I_numerik = integral_trapezoid(batas_bawah,batas_atas,k)
    output_integral = list_nama_metode(1)
 else if (metode==2) then
         if (mod(k, 2) == 1) then
         write(*,*) 'Untuk metode integrasi simpson, jumlah titik integrasi harus genap!' 
         goto 20
         else 
         goto 30
         end if
    30 I_numerik = integral_simpson(batas_bawah,batas_atas,k)
    output_integral = list_nama_metode(2)
 else 
    write(*,*) 'metode tidak valid, ulangi' 
    goto 10
end if

 !kesalahan relatif
 kesrel = abs(I_numerik-I_analitik)/I_analitik*100
 konvergensi = 100-kesrel

open(unit=2, file='output_integral_tugas_06.txt', status='replace', action='write')
write(2,*) 'metode integrasi =', output_integral
write(2,*) 'jumlah titik integrasi =',k
write(2,*) 'hasil analitik =', I_analitik
write(2,*) 'hasil numerik =', I_numerik
write(2,*) 'kesalahan relatif =', kesrel, '%'
write(2,*) 'tingkat konvergensi =', konvergensi, '%'
close(2)

write(*,*) 'perhitungan berhasil, cek hasil di file output_integral_tugas_06.txt'
  deallocate(A)

  stop

contains
  
  function pol_7(A, x, n) result(y) 
    integer, intent(in) :: n
    real(dpr), intent(in) :: A(n)
    real(dpr), intent(in) :: x
    real(dpr) :: y
    integer :: i
    
    !inisiasi nilai y
    y = 0.0
    do i=1,n 
       y = y + A(i)*x**(i-1)
    end do
    
  end function pol_7

  function anti_pol_7(A, x, n) result(y)
   integer, intent(in) :: n
   real(dpr), intent(in) :: A(n)
   real(dpr), intent(in) :: x
   real(dpr) :: y
   integer :: i

    !inisiasi nilai y
    y = 0.0
    do i=1,n
       y = y + A(i)*x**(i)/i
    end do

  end function anti_pol_7

  function integral_trapezoid(batas_bawah, batas_atas, k) result(I_numerik)
    real(dpr), intent(in) :: batas_atas, batas_bawah
    real(dpr), dimension(:), allocatable :: f
    real(dpr) :: I_numerik, h
    integer, intent(in) :: k
    integer :: i

    allocate(f(k+1))

    h = (batas_atas - batas_bawah) / k

    do i = 1, k + 1
        f(i) = pol_7(A, batas_bawah + (i - 1) * h, n)
    end do

    I_numerik = (h / 2) * (f(1) + f(k + 1))

    do i = 1, k - 1
        I_numerik = I_numerik + h * f(i + 1)
    end do

    deallocate(f)

end function integral_trapezoid

  function integral_simpson(batas_bawah,batas_atas,k) result(I_numerik)
    real(dpr), intent(in) :: batas_atas, batas_bawah
    real(dpr) :: I_numerik, h
    real(dpr) :: f(k+1)
    integer, intent(in) :: k
    integer :: i,NN
    
    NN = k/2

    h = (batas_atas-batas_bawah)/k

    do i = 1,2*NN+1
       f(i) = pol_7(A,batas_bawah+(i-1)*h,n)
    end do

    I_numerik = (h/3)*(f(1)+f(2*NN+1))

    do i = 1,NN
    I_numerik = I_numerik + (4*h/3)*f(2*i)
    end do

    do i = 1,NN-1
    I_numerik = I_numerik + (2*h/3)*f(2*i+1)
    end do

  end function integral_simpson
  
end program tugas_06_integral
