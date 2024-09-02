program taylor
    implicit none
    real(8) g, x, f, er
    integer i, n

    f = 0.0d0
    g = 1.0d0
    x = 1.0d0
    do n = 0, 10
        do i = 0, n
            if(i > 0) then
                g = g * dble(i)
            endif
        enddo
        f = f + (x ** n)/g
        er = abs(f - exp(1.0d0)) 
        write(*,*) 'n :', n, 'er', er
    enddo
end program taylor
    
       
    
