program touhi1
    implicit none
    real(8) a, r
    integer :: n, i, fo = 11
    
    open(fo, file = 'output.d')
    a = 16.0d0
    r = 0.8d0
    do n = 1, 10
        write(fo,*) n, a * (r ** dble(n - 1))    
        write(fo,*) ''
    enddo
    close(fo)
    
end program touhi1
