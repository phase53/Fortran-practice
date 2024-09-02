program seikibunpu
    implicit none
    real(8) x, y, dx, pi, a, b, s
    integer n, i

    do
        write(*,*) 'input n'
        read(*,*) n
        if(n <= 0) then
            write(*,*) 'input posotive n...'
            cycle
        endif
        exit    
    enddo
    do
        write(*,*) 'input a'
        read(*,*) a
        write(*,*) 'input b'
        read(*,*) b
        if(b < a) then
            write(*,*) 'b must be bigger than a'
            cycle
        endif
        exit
    enddo    

    s = 0.0d0
    pi = 2.0d0 * acos(0.0d0)
    dx = dble(b - a)/dble(n)
    do i = 0, n
        x = dble(i) * dx + a
        y = exp(-0.5d0 * (x ** 2))/dble(sqrt(2.0d0 * pi))
        if(i == 1.or.i == n) then
            y = y * 0.5d0
        endif
        s = s + y
    enddo
    
    s = s * dx
    write(*,*) 'ans :', s
end program seikibunpu
