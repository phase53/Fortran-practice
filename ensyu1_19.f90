program hyper
    implicit none
    real(8)  y1, y2, y3, x, dx
    integer :: i, n, fo = 11

    open(fo, file = 'output.d')
    write(*,*) 'input n'    
    read(*,*) n
    if(n < 2) then
        write(*,*) 'input n >= 2'
        stop
    endif
    
    x = -1.0d0
    dx = 2.0d0/dble(n)
    do i =  1, (n - 1)
        x = x + dx
        y1 = sinh(x)
        y2 = cosh(x)
        y3 = tanh(x)
        write(fo,'(4e12.4)') x, y1, y2, y3
    enddo
    close(fo)

end program hyper

