module mat_subprogs
    implicit none
contains
    subroutine print_rmatc(a, mes)
        real(8),intent(in) :: a(:,:)
	integer :: i, n, m
	character(*),intent(in) :: mes
	n = size(a,1)
	m = size(a,2)
	write(*,*) mes
	do i = 1, n
	    write(*,'(100e12.4)') a(i,1:m)
	enddo
    end subroutine print_rmatc
end module mat_subprogs

program main
    use mat_subprogs
    implicit none
    integer,parameter :: n = 3, m = 3
    real(8) :: a(1:n,1:m)

    call random_seed
    call random_number(a)

    call print_rmatc(a,'matrix a')
end program main


