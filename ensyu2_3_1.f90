program norm
    implicit none
    integer,parameter :: n = 5
    real(8) :: a(1:n), b(1:n), c(1:n)

    call random_number(a)
    call random_number(b)

    c = (a(:) - b(:)) ** 2

    write(*,*) c

end program norm

