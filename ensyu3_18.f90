module subprogs
    implicit none
contains
    function norm(u,n) result(r)
        real(8),intent(in) :: u(n)
        real(8) :: r
	integer,intent(in) :: n

	r = sqrt(dot_product(u,u))
    end function norm
end module subprogs

program main
    use subprogs
    implicit none
    integer,parameter :: n = 3
    real(8) :: u(n)

    call random_seed
    call random_number(u)
    write(*,*) '|u| =', norm(u,n)
end program main
