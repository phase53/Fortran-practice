program newton_sort2
    implicit none
    real(8) :: L1, L2, a, g = 9.8d0, h, pi, T, er, er0 = 1.0d-6
    integer :: k, km = 100
    write(*,*) 'input a:'
    read(*,*) a
    if(a <= 0.0d0) then 
        write(*,*) 'a <= 0.0d0'
        stop
    endif
    write(*,*) 'input h[m]'
    read(*,*) h
    if(h <= 0.0d0) then 
        write(*,*) 'h <= 0.0d0'
        stop
    endif
    write(*,*) 'input T[s]:'
    read(*,*) T
    if(T <= 0.0d0) then 
        write(*,*) 'T <= 0.0d0'
        stop
    endif
    pi = 2.0d0 * acos(0.0d0)

    L1 = a
    do k = 1, km
        L2 = T * sqrt((g * L1 * tanh((2.0d0 * pi * h)/L1))/(2.0d0 * pi))
        er = abs(L2 - L1)
        if(er < er0) then
            exit
        endif
        L1 = L2
    enddo
    write(*,*) 'L =' , L2, '[m]'
    write(*,*) 'h =' , h, '[m]'
    write(*,*) 'T =' , T, '[m]'

end program newton_sort2
