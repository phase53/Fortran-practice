program mattimesmat
    implicit none
    integer,parameter :: n = 3
    real(8) :: t1, t2, a(1:n,1:n), b(1:n,1:n), c(1:n,1:n)
    integer :: i, j, k

    call random_seed
    call random_number(a(:,:))
    call random_number(b(:,:))

    call cpu_time(t1)
    do j = 1, n
        do i = 1 ,n
            c(i, j) = 0._8
	    do k = 1, n
	        c(i,j) = c(i,j) + a(i,k) * b(k,j)
	    enddo
        enddo
    enddo

    call cpu_time(t2)

    write(*,*) 'A =', a(:,:)
    write(*,*) 'B =', b(:,:)
    write(*,*) 'C =', c(:,:)
    write(*,*) 'Time =', t2 - t1

end program mattimesmat
