program niko
    implicit none
    integer :: n, r, i1, i2, i3, A, B, C

    do
        write(*,*) 'input n(n >= 0, when you want to stop, input 999)'
        read(*,*) n
        if(n < 0) then
            write(*,*) 'input positive n...'
            cycle
        else if(n == 999) then
            write(*,*) 'good bye...'
            exit
        endif
        write(*,*) 'input n(n >= r >= 0)'
        read(*,*) r
        if(r < 0) then
            write(*,*) 'input positive r...'
            cycle
        else if(r > n) then 
            write(*,*)'r should be smaller than n...'
            cycle
        endif
        A = 1
        B = 1
        C = 1
        if(n == 0) then
            A = 1
        else
            do i1 = 1, n
                A = A * i1
            enddo
        endif
        if(n-r == 0) then
            B = 1
        else
            do i2 = 1, n-r
                B = B * i2
            enddo
        endif
        if(r == 0) then
            C = 1
        else
            do i3 = 1, r
                C = C * i3
            enddo
        endif
        write(*,*) 'nPr =', A/B, 'nCr =', A/(B*C)
    enddo
end program niko
