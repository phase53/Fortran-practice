program rnum
    implicit none
    real(8), allocatable :: r(:)
    real(8) :: mean, std
    integer :: n, i, j
    
    write(*,*) 'input n (>= 1)'
    read(*,*) n
    
    if(n < 1) then
        write(*,*) 'stop n < 1'
        stop
    endif

    allocate(r(n))

    call random_seed
    call random_number(r(1:n))

    r(1:n) = 2._8 * r(1:n) - 1._8
    write(*,*) 'r(n):', r(1:n)

    mean = 0._8
    do i = 1, n
        mean = mean + r(i)
    enddo
    mean = mean/dble(n)
    write(*,*) 'mean:', mean

    std = 0._8
    do j = 1, n
        std = std + (r(j) - mean) ** 2
    enddo
    std = std/dble(n)
    std = sqrt(std)
    write(*,*) 'std:', std

end program rnum
