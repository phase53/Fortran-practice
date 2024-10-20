program syou
    real(8),allocatable :: a(:,:), b(:,:)
    integer :: i, j, n

    write(*,*) 'input n (>= 2) :'
    read(*,*) n
    if(n < 2) then
        write(*,*) 'stop n < 2'
	stop
    endif

    allocate(a(n,n),b(n-1,n-1))

    call random_seed
    call random_number(a(1:n,1:n))

    write(*,*) 'A =', a

    do j = 1, n
        do i = 1, n
	    b(1:i-1,1:j-1) = a(1:i-1,1:j-1)
	    b(1:i-1,j:n-1) = a(1:i-1,j+1:n)
            b(i:n-1,1:j-1) = a(i+1:n,1:j-1)
            b(i:n-1,j:n-1) = a(i+1:n,j+1:n)
            write(*,*) 'B = A~_(', i, j, ') =', B
        enddo
    enddo

end program syou
