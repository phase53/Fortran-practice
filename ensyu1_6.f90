program loop_inf
    implicit none
    integer wa, n, m, i
    do
        write(*,*) 'input m(if m <= 0, stop) :'
        read(*,*) m
        if(m <= 0) stop 'good bye...'

        write(*,*) 'input n(if n <= 0, stop) :'
        read(*,*) n
        if(n <= 0) stop 'good bye...'

        wa = 0
        if(m <= n) then
            do i = m, n
                wa = wa + i
            enddo
        else
            do i = n, m
                wa = wa + i
            enddo
        endif
        write(*,*) 'wa =', wa
    enddo
end program loop_inf
