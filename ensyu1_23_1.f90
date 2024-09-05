program daensekibun
    implicit none
    real(8) a, b, c, R, k, pi
    integer :: i, j, n
    
    write(*,*) 'input n'
    read(*,*) n
    if(n < 2) then
        write(*,*) 'input n >= 2'
        stop
    endif
    write(*,*) 'input k'
    read(*,*) k
    if(k >= 1.or.k <= -1) then
        write(*,*) 'input n (0 <= k < 1)'
        stop
    endif

    pi = 2.0d0 * acos(0.0d0)
    c = 1.0d0
    do i = 2, n
        a = 1.0d0
        b = 1.0d0
        do j = 2, i
            a = a * (((2.0d0 * dble(j - 1)) - 1.0d0) ** 2)
            b = b * ((2.0d0 * dble(j - 1)) ** 2)
        enddo
        c = c + (a/b) * (k ** (2.0d0 * dble(i - 1)))
    enddo
    R = (pi/2.0d0) * c
    write(*,*) 'k =', k, 'K(k) =', R
end program daensekibun
