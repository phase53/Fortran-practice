program mat
    implicit none
    integer,parameter :: n = 3
    real(8) :: a(1:n,1:n), b(1:n,1:n), c(1:n,1:n), d(1:n,1:n)
    integer :: i, j, k

    call random_seed
    call random_number(a(:,:))
    call random_number(b(:,:))

    do j = 1, n
        do i = 1 ,n
            c(i, j) = 0._8
	    do k = 1, n
	        c(i,j) = c(i,j) + a(i,k) * b(k,j)
	    enddo
        enddo
    enddo

    d(1:n,1:n) = matmul(a(1:n,1:n),b(1:n,1:n))

    write(*,*) 'A =', a(:,:)
    write(*,*) 'B =', b(:,:)
    write(*,*) 'C =', c(:,:)
    write(*,*) 'D =', d(:,:)

end program mat
