program loopcheck
    implicit none
    integer n, k, s1, s2
    write(*,*) 'input n :'
    read(*,*) n
    s1 = 0
    s2 = 0
    do k = 1, n
        s1 = s1 + k
    enddo
    write(*,*) '左辺 =', s1
    s2 = ishft(n*(n+1),-1)
    write(*,*) '右辺 =', s2
end program loopcheck
