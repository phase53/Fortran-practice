program tenchi1
    implicit none
    integer,parameter :: n = 3
    real(8) :: a(1:n,1:n), b(1:n,1:n), c(1:n,1:n), d(1:n,1:n), e(1:n,1:n), f(1:n,1:n), g(1:n,1:n)
    integer :: i, j, k

    call random_seed
    call random_number(a(:,:))
    call random_number(b(:,:))

    !(AB)^Tの計算

    do j = 1, n
        do i = 1 ,n
            c(i, j) = 0._8
	    do k = 1, n
	        c(i,j) = c(i,j) + a(i,k) * b(k,j)
	    enddo
        enddo
    enddo

    do j = 1, n
        do i = 1, n
	    d(i,j) = c(j,i)
        enddo
    enddo

    !(B^T)(A^T)の計算

    do j = 1, n
        do i = 1, n
	    e(i,j) = a(j,i)
        enddo
    enddo
    
    do j = 1, n
        do i = 1, n
	    f(i,j) = b(j,i)
        enddo
    enddo

    do j = 1, n
        do i = 1 ,n
            g(i, j) = 0._8
	    do k = 1, n
	        g(i,j) = g(i,j) + f(i,k) * e(k,j)
	    enddo
        enddo
    enddo

    write(*,*) 'A =', a(:,:)
    write(*,*) 'B =', b(:,:)
    write(*,*) '(AB)^T =', d(:,:)
    write(*,*) '(B^T)(A^T) =', g(:,:)

end program tenchi1
