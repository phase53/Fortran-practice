module mat_subprogs
    implicit none
contains
    subroutine print_mat2(a)
        real(8),intent(in) :: a(:,:)
	integer :: i, n, m
        
	n = size(a,1)
	m = size(a,2)
	do i = 1, n
	    write(*,'(100e12.4)') a(i,1:m)
	enddo
    end subroutine print_mat2
end module mat_subprogs

program main
    use mat_subprogs
    implicit none
    real(8) :: a(-2:2,-3:3)

    call random_number(a(:,:))
    call print_mat2(a)
end program main


