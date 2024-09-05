program newton_sort2
    implicit none
    real(8) :: x1, x2, a, er, er0 = 1.0d-6
    integer :: k, i, im = 100
    write(*,*) 'input a:'
    read(*,*) a
    if(a <= 0.0d0) then 
        write(*,*) 'a <= 0.0d0'
        stop
    endif
    write(*,*) 'input k:'
    read(*,*) k
    if(k <= 0.0d0) then 
        write(*,*) 'k <= 0.0d0'
        stop
    endif
    
    x1 = a
    do i = 1, im
        x2 = x1 - ((x1 ** k) - a)/(dble(k) * (x1 ** (k - 1)))
        er = abs(x2 - x1)
        if(er < er0) then
            exit
        endif
        x1 = x2
    enddo
    write(*,*) 'kai, k, er =', x2, k, er
end program newton_sort2
