program matrix
    implicit none
    real(8),allocatable :: m(:,:)
    integer :: n, i

    write(*,*) 'input n (>= 2) :'
    read(*,*) n
    if(n < 2) then
        write(*,*) 'stop n < 2'
        stop
    endif

    allocate(m(n,n))

    call random_seed
    do i = 1, n
        call random_number(m(i, 1:n))
    enddo

    do i = 1, n
        write(*,'(100e12.4)') m(i, 1:n)
    enddo

    deallocate(m)
end program matrix
