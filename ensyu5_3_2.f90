module mat_subprogs
    implicit none
contains
    recursive function det_mat(a) result(det)
        real(8),intent(in) :: a(:,:)
	real(8) :: det, b(size(a,1) - 1,size(a,2) - 1)
	integer :: i
	if(size(a,1) > 1) then
	    det = 0._8
	    do i = 1, size(a,1)
	        b(1:i-1,1:size(a,1)-1) = a(1:i-1,2:size(a,1))
		b(i:size(a,1)-1,1:size(a,1)-1) = a(i+1:size(a,1),2:size(a,1))
		det = det + (-1._8) ** (i + 1) * a(i,1) * det_mat(b)
	    enddo
	else
	    det = a(1,1)
	endif
    end function det_mat
end module mat_subprogs

program main
    use mat_subprogs
    implicit none
    integer,parameter :: n = 4
    real(8) :: seki, a(1:n,1:n), b(1:n,1:n)
    integer :: j, k

    if(n > 10.or.n < 1) then
        write(*,*) 'stop n must be 1 <= n <= 10'
	stop
    endif
    
    a(:,:) = 0._8
    call random_seed

    do k = 1, n
        do j = 1, k
            call random_number(a(j,k))
	enddo
    enddo

    write(*,*) 'A ='
    do j = 1, n
        write(*,'(5e12.4)') a(j,1:n)
    enddo

    seki = 1._8
    do j = 1, n
        seki = seki * a(j,j)
    enddo

    write(*,*) '|A| =', det_mat(a)
    write(*,*) 'Πa(i,i) =', seki
end program main
