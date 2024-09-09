program naiseki
    implicit none
    real(8) :: k, u(1:3), v(1:3)
    integer :: n, i, j, fi1 = 10, fi2 = 20

    open(fi1, file = 'mat.d')
    open(fi2, file = 'mat.d')
    
    
    read(fi1, '(3e12.4)') (u(n), n = 1, 3)
    read(fi2, '(3e12.4)') (v(n), n = 1, 3)
    

    do j = 1, 3
        write(*,*) 'u(', j, ') =', u(j), 'v(', j, ') =', v(j)
    enddo

    k = 0.0_8
    do i = 1, 3
        k =  k + u(i) * v(i)
    enddo
    write(*,*) 'naiseki =', k
end program naiseki
