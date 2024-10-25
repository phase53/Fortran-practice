module vec_subprogs
    implicit none
contains
    subroutine print_lb(v,lb)
        integer,intent(in) :: lb
	integer,intent(in) :: v(lb:)

	write(*,*) v(0), lbound(v,1), ubound(v,1)
    end subroutine print_lb
end module vec_subprogs

program main
    use vec_subprogs
    implicit none
    integer,parameter :: n = 3
    integer :: i, x(-n:n) = (/(i, i = -n, n)/)

    write(*,*) x(0), -n, n
    call print_lb(x,-n)
    call print_lb(x,lbound(x,1))
end program main
