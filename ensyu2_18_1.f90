program mattimesvec
    implicit none
    integer,parameter :: n = 3
    real(8) :: t1, t2, a(1:n,1:n), x(1:n), y(1:n)
    integer :: i, j

    call random_seed
    call random_number(a(:,:))
    call random_number(x(:))

    call cpu_time(t1)
    do i = 1, n
        y(i) = 0._8
	do j = 1, n
	    y(i) = y(i) + a(i,j) * x(j)
	enddo
    enddo

    call cpu_time(t2)

    write(*,*) 'A =', a(:,:)
    write(*,*) 'x =', x(:)
    write(*,*) 'y =', y(:)
    write(*,*) 'Time =', t2 - t1

end program mattimesvec
