program main
    !$ use omp_lib
    implicit none
    integer :: i, j, k
    integer,parameter :: n1 = 100, n2 = 200, n3 = 50
    real(8) :: a3(1:n1,1:n2,1:n3), t1

    !$ call omp_set_num_threads(10)

    !$ t1 = omp_get_wtime()
    !$OMP parallel private(i,j,k)
        !$OMP do collapse(3)
        do k = 1, n3
	    do j = 1, n2
	        do i = 1, n1
		    a3(i,j,k) = 0._8
		enddo
	    enddo
	enddo
	!$OMP enddo
    !$OMP end parallel
    !$ write(*,*) 'elapsed time =', omp_get_wtime() - t1
end program main
