program touhi2
    implicit none
    real(8) a, r, tmp1, tmp2, s1, s2
    integer n, i
    
    write(*,*) 'input n:'
    read(*,*) n
    
    a = 16.0d0
    r = 0.8d0
    s1 = 0d0

    do i = 1, n
        tmp1 = a * (r ** dble(i - 1))
        s1 = s1 + tmp1
    enddo
     
    s2 = (a * (1 - (r ** dble(n))))/(dble(1 - r))
    
    write(*,*) 's1 :', s1
    write(*,*) 's2 :', s2
end program touhi2
