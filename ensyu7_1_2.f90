program parallel_dotp
    !$ use omp_lib
    implicit none
    integer :: i, n = 1000
    real(8) :: dp = 0._8
    real(8),allocatable :: x(:), y(:)
    
    allocate(x(n),y(n))
    call random_number(x(1:n))
    call random_number(y(1:n))

    !$ call omp_set_num_threads(2)
    !$OMP parallel private(i)
        !$OMP do
	do i = 1, n
	    dp = dp + x(i) * y(i)
	enddo
	!$OMP enddo
    !$OMP end parallel
    write(*,*) 'parallel dp =', dp
    write(*,*) 'serial dp =', dot_product(x,y)
end program parallel_dotp
