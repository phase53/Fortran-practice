program simple_sort
    implicit none
    integer, parameter :: n = 10
    real(8) :: a(n), m1
    integer :: i, j, m, m2

    call random_seed
    call random_number(a(:))
    write(*,*) 'Original a is', a
    m = 1
    do i = 1 ,n - 1
        m1 = a(i)
        do j = i + 1, n
	    if(a(j) < m1) then
	        m1 = a(j)
	        m = j
            endif
	    if(i == 1) then
	        m2 = m
            endif
	enddo        
        if(a(i) > m1) then
	    a(m) = a(i)
	    a(i) = m1
	endif
    enddo

    write(*,*) 'Sorted a is', a(:)
    write(*,*) 'Min location is', m2

end program simple_sort

