program bunkai
    implicit none
    integer :: n, i, j

    1 continue
    write(*,*) 'input n '
    read(*,*) n
    if(n <= 1) then
        write(*,*) 'input n >= 2...'
        goto 1
    endif
    
    3 continue
    if(n == 1) then
        goto 4
    endif
    do i = 2, n
        if(i == 2) then
            if(mod(n,i) == 0) then
                write(*,*) i
                n = int(n/i)
                goto 3
            endif
        else if(i == 3) then
            if(mod(n,i) == 0) then
                write(*,*) i
                n = int(n/i)
                goto 3
            endif
        else
            do j = 2, int(sqrt(dble(i)))
                if(mod(i, j) == 0) then
                    goto 2
                endif
            enddo
            if(mod(n, i) == 0) then
                write(*,*) i
                n = int(n/i)
                goto 3
            endif
        endif
        2 continue
    enddo
    4 continue
end program bunkai
