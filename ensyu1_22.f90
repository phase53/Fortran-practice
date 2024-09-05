program niji
    implicit none
    real(8) re, im, a, b, c, x, x1, x2, D

    write(*,*) 'input a'
    read(*,*) a
    if(a == 0) then
        write(*,*) 'input a /= 0'
        stop
    endif
    write(*,*) 'input b'
    read(*,*) b
    write(*,*) 'input c'
    read(*,*) c
    
    D = (b ** 2) - (4.0d0 * a * c)
    if(D > 0) then
        x1 = (-b + sqrt(D))/(2.0d0 * a)
        x2 = (-b - sqrt(D))/(2.0d0 * a)
        write(*,*) 'x1 =', x1
        write(*,*) 'x2 =', x2
    else if(D == 0) then
        x1 = (-b)/(2.0d0 * a)
        write(*,*) 'x =', x1
    else
        re = (-b)/(2.0d0 * a)
        im = sqrt(-D)/(2.0d0 * a)
        write(*,*) 'x1 Re =', re, 'Im =', im
        write(*,*) 'x2 Re =', re, 'Im =', -im
    endif
end program niji

