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

program cal_det
    use mat_subprogs
    implicit none
    integer,parameter :: n = 5
    real(8) :: a(n,n)

    call random_seed
    call random_number(a)

    write(*,*) 'det =', det_mat(a)
end program cal_det

