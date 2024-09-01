program gcd
    implicit none 
    integer :: i, n, m
    
    1 continue
    write(*,*) 'input n'
    read(*,*) n
    if(n <= 0) then
        write(*,*) 'input positive n...'
        goto 1
    endif
    
    write(*,*) 'input m'
    read(*,*) m
    if(m <= 0) then
        write(*,*) 'input positive m...'
        goto 1
    endif
    
    if(n == m) then
        write(*,*) 'gcd is ', n
        goto 2
    else if(n > m) then
        do i = m, 1, -1
            if((mod(n, i) == 0).and.(mod(m, i) == 0)) then
                write(*,*) 'gcd is ', i
                goto 2
            endif
        enddo
    else if(n < m) then
        do i = n, 1, -1
            if((mod(n, i) == 0).and.(mod(m, i) == 0)) then
                write(*,*) 'gcd is ', i
                goto 2
            endif
        enddo
    endif
    2 continue
end program gcd 
        
       
    
 
