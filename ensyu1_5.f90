program loop_inf
    implicit none
    integer wa, n, m, i
    do
        write(*,*) 'input m(if m <= 0, stop) :'
        read(*,*) m
        write(*,*) 'input n(if n <= m, stop) :'
        if(m <= 0) stop 'good bye...'
        read(*,*) n
        if(n < m) stop 'good bye...'
        wa = 0
        do i = m, n
            wa = wa + i
        enddo
        write(*,*) 'wa =', wa
    enddo
end program loop_inf
