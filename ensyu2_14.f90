program mat3d
    implicit none
    integer :: i, j, k, is, n
    real(8) :: t1, t2
    integer,allocatable :: a(:, :, :)
    
    write(*,*) 'input n :'
    read(*,*) n
    allocate(a(n, n, n), stat = is)
    if(is /= 0) then
        write(*,*) 'cannot allocate (n is too large)'
    endif
    
    call cpu_time(t1)
    
    do k = 1, n
        do j = 1, n
            do i = 1, n
                a(i, j, k) = 0
            enddo
        enddo
    enddo

    call cpu_time(t2)

    write(*,*) 'CPU time1 =', t2 - t1
    
    call cpu_time(t1)

    do i = 1, n
        do j = 1, n
            do k = 1, n
                a(i, j, k) = 0
            enddo
        enddo
    enddo

    call cpu_time(t2)

    write(*,*) 'CPU time2 =', t2 - t1

    call cpu_time(t1)

    a(1:n, 1:n, 1:n) = 0
    
    call cpu_time(t2)

    write(*,*) 'CPU time3 =', t2 - t1

    deallocate(a)

end program mat3d
