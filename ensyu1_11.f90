program amari
    implicit none
    integer a, p, i
    
    do
        write(*,*) 'input a'
        read(*,*) a
        write(*,*) 'input p'
        read(*,*) p
        if(p == 0) then
            write(*,*) 'CANNOT input  0'
            cycle
        endif
        exit
    enddo
    
    if(a >= 0.and.p > 0) then
        do 
            if(a < p) then
                write(*,*) 'amari is', a
                stop
            endif
            a = a - p
        enddo
    endif
    
    if(a < 0.and.p > 0) then
        do 
            if(a >= 0) then
                write(*,*) 'amari is', a
                stop
            endif
            a = a + p
        enddo
    endif

    if(a >= 0.and.p < 0) then
        do 
            if(a < -p) then
                write(*,*) 'amari is', a
                stop
            endif
            a = a + p
        enddo
    endif

    if(a < 0.and.p < 0) then
        do 
            if(a > 0) then
                write(*,*) 'amari is', a
                stop
            endif
            a = a - p
        enddo
    endif
    
end program amari    
