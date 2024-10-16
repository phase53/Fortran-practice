program ichi
    implicit none
    real(8),allocatable :: x(:), y(:), r(:)
    integer :: n, i, fo = 11

    open(fo, file = 'output.d')
    write(*,*) 'input n (n >= 2) :'
    read(*,*) n
    if(n < 2) then
        write(*,*) 'stop n < 2'
        stop
    endif

    allocate(x(n), y(n), r(n))
    
    x(:) = 0
    do i = 1, n - 1
        x(i + 1) = x(i) + 10._8/dble(n - 1)
    enddo

    call random_number(r)
    r = -1._8 + 2._8 * r

    y = 2._8 * x + 1 + r
    
    do i = 1, n
        write(fo,*) x(i), y(i)
    enddo

end program ichi
