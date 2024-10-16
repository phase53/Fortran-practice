program norms
    implicit none
    integer :: i
    integer,parameter :: n = 5
    real(8) :: a(1:n), b(1:n), c(1:n)

    call random_number(a)
    call random_number(b)

    do i = 1, n
        c(i) = (a(i) - b(i)) ** 2
    enddo

    write(*,*) c

end program norms

