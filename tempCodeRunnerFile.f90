open(unit=2, file='output_integral_tugas_06.txt', status='replace', action='write')
write(2,*) 'metode integrasi =', output_integral
write(2,*) 'jumlah titik integrasi =',k
write(2,*) 'hasil analitik =', I_analitik
write(2,*) 'hasil numerik =', I_numerik
write(2,*) 'kesalahan relatif =', kesrel, '%'
write(2,*) 'tingkat konvergensi =', konvergensi, '%'
close(2)

write(*,*) 'perhitungan berhasil, cek hasil di file output_integral_tugas_06.txt'