program ransuu
    implicit none
    real(8),allocatable :: ran(:)
    integer :: n, i

    write(*,*) 'input n (>=1)'
    read(*,*) n
    if(n < 1) then
        write(*,*) 'stop n < 1'
        stop
    endif
    allocate(ran(n))

    call random_seed
    call random_number(ran)
    ran = 10._8 * ran

    do i = 1, n
        ran(i) = int(ran(i))
    enddo

    write(*,*) ran(:)

end program ransuu
