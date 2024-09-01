program prime
    implicit none
    integer :: n, i
    
    1 continue
    write(*,*) 'input n you want to judge (input 0 if you want to cancel judging)'
    read(*,*) n
    
    if(n < 0) then
        write(*,*) 'input positive n...'
        goto 1
    else if (n == 0) then 
        write(*,*) 'good bye...'
        stop
    endif
    do i = 2, int(sqrt(dble(n)))
        if(mod(n, i) == 0) then
            write(*,*) 'n is NOT prime number.'
            goto 2
        endif
    enddo
    write(*,*) 'n is prime number' 
    2 continue
end program prime
    
       
    
    
        
    
