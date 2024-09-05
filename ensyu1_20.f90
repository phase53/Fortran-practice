program enn
    implicit none
    real(8) dth, th, x, y, r, pi
    integer :: n, i, fo = 11

    open(fo, file = 'output.d')
    write(*,*) 'input n'
    read(*,*) n
    if(n < 3) then
        write(*,*) 'input n >= 3'
        stop
    endif
    write(*,*) 'input r'
    read(*,*) r
    if(r <= 0) then
        write(*,*) 'input r > 0'
        stop
    endif

    
    th = 0.0d0
    pi = 2.0d0 * acos(0.0d0)
    dth = (2.0d0 * pi)/dble(n)
    do i = 1, n
        th = th + dth
        x = r * cos(th)
        y = r * sin(th)
        write(fo, '(4e12.4)') x, y
    enddo
    close(fo) 

end program enn
