program gaiseki
    implicit none
    real(8) :: seibun, c(1:2,1:3), a(1:3), b(1:3) 
    integer :: i

    call random_seed
    call random_number(a(1:3))
    call random_number(b(1:3))

    c(1,1:3) = a(1:3)
    c(2,1:3) = b(1:3)

    write(*,*) 'c =', c
    do i = 1, 3
        c = cshift(c, 1, 2)
        seibun = c(1,1) * c(2,2) - c(1,2) * c(2,1)
	write(*,*) '(a \times b)_', i, '=', seibun
    enddo

end program gaiseki
