program main
    !$ use omp_lib
    implicit none
    integer,parameter :: n = 100
    integer :: i, j
    real(8) :: x(1:n), y(1:n), a(1:n,1:n), py(1:n), t1, t2, t3, t4, t5, t6

    call random_seed
    call random_number(a)
    call random_number(x)

    !$ call omp_set_num_threads(8)

    !$ t1 = omp_get_wtime()
    !$OMP parallel private(i,j)
        !$OMP do
	do i = 1, n
	    y(i) = 0._8
	enddo
	!$OMP enddo
	
	!$OMP do
	do i = 1, n
	    do j = 1, n
	        y(i) = y(i) + a(i,j) * x(j)
	    enddo
	enddo
	!$OMP enddo
    !$OMP end parallel
    !$ t2 = omp_get_wtime()

    !$ t3 = omp_get_wtime()
    !$OMP parallel private(i,j)
        !$OMP do
	do i = 1, n
	    y(i) = 0._8
	enddo
	!$OMP enddo
	
	do j = 1, n
	    !$OMP do
	    do i = 1, n
	        y(i) = y(i) + a(i,j) * x(j)
	    enddo
	    !$OMP enddo
	enddo
    !$OMP end parallel
    !$ t4 = omp_get_wtime()

    !$ t5 = omp_get_wtime()
    !$OMP parallel private(i,j,py)
        !$OMP do
	do i = 1, n
	    y(i) = 0._8
	enddo
	!$OMP enddo
	
	!$OMP do
	do j = 1, n
	    do i = 1, n
	        py(i) = py(i) + a(i,j) * x(j)
	    enddo
	enddo
	!$OMP enddo
	!$OMP critical
	do i = 1, n
	    y(i) = y(i) + py(i)
	enddo
	!$OMP end critical
    !$OMP end parallel
    !$ t6 = omp_get_wtime()

    write(*,*) 'row/core =', t2 - t1
    write(*,*) 'fixed row/core =', t4 - t3
    write(*,*) 'col/core =', t6 - t5
end program main

