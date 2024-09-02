program touhi1
    implicit none
    real(8) a, r
    integer n, i
    
    a = 16.0d0
    r = 0.8d0
    do n = 1, 10
        write(*,*) 'n =', n, 'a_n =', a * (r ** dble(n - 1))    
    enddo
    
end program touhi1
