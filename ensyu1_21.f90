program menseki
    implicit none
    real(8) e, s1, s2, dth, r, pi
    integer :: k, n, fo = 11

    open(fo, file = 'output.d')
    write(*,*) 'input k'
    read(*,*) k
    if(k < 3) then
        write(*,*) 'input k >= 3'
        stop
    endif
    write(*,*) 'input r'
    read(*,*) r
    if(r <= 0) then
        write(*,*) 'input r > 0'
        stop
    endif

    pi = 2.0d0 * acos(0.0d0)
    s1 = pi * (r ** 2)

    do n = 3, k
        dth = (2.0d0 * pi)/dble(n)
        s2 =(dble(n) * (r ** 2) * sin(dth))/2.0d0
        e = abs(s1 - s2)
        write(fo, '(3e12.4)') dble(n), e
    enddo
    close(fo)

end program menseki
