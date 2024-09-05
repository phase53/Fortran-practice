program ondo
    implicit none
    real(8) :: Ti, k, L, pi, x, dx, lam
    integer :: t, n, m, i, fo = 11

    open(fo, file = 'output.d')
    write(*,*) 'input m'
    read(*,*) m
    if(m < 2) then
        write(*,*) 'input m >= 2'
        stop
    endif

    L = 1.0_8
    k = 0.01_8
    pi = 2.0_8 * acos(0.0_8)
    dx = L/real(m, kind = 8)
    
    do t = 0, 10
        write(fo,'(e12.4)', advance = 'no') real(t, kind = 8)
        do i = 0, m
            if(i < real(m, kind = 8)/2.0_8) then
                Ti = real(i, kind = 8) * dx
            else
                Ti = L - real(i, kind = 8) * dx
            endif
            x = real(i, kind = 8) * dx
            do n = 1, 50
                lam = k * (((2.0_8 * real(n, kind = 8) - 1)/L) ** 2)
                Ti = Ti + (exp(-lam * real(t, kind = 8)) * sin((((2.0_8 * real(n, kind = 8)) - 1) * pi * x)/L))/((2.0_8 * real(n, kind = 8) - 1) ** 2)
            enddo
            Ti = (4.0_8 * L * Ti)/(pi ** 2)
            if(i == m) then
                write(fo,'(e12.4)', advance = 'yes') Ti
            else
                write(fo,'(e12.4)', advance = 'no') Ti
            endif
        enddo
    enddo
    close(fo)
!11行(m＋2)列のデータが出力され，例えばi行目には，秒数，(m+1点分の温度データ)が出力されています

end program ondo
