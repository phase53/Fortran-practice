program norm241
    implicit none
    real(8) :: u(3), l
    integer :: i

    call random_number(u)
    l = 0

    do i = 1, 3
        l = l + u(i) * u(i)
    enddo

    l = sqrt(l)

    write(*,*) u
    write(*,*) l

end program norm241
