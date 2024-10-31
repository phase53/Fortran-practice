program parallel_critical
    !$ use omp_lib
    implicit none
    integer :: i, n = 1000, psum, wa = 0
    !$ call omp_set_num_threads(2)
    !$OMP parallel private(i,psum)
        psum = 0
	!$OMP do
	do i = 1, n
	    psum = psum + i
	enddo
	!$OMP enddo
	write(*,*) 'my thread no and psum =',omp_get_thread_num(),psum
	wa = wa + psum
    !$OMP end parallel 
    write(*,*) 'wa =', wa
end program parallel_critical
