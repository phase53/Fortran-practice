module subprogs
    implicit none
contains
    subroutine set_gridx(x,n1,n2)
        integer,intent(in) :: n1, n2
	real(8),intent(out) :: x(1:2,1:n1,1:n2)
        integer :: i

	do i = 1, n1
	    x(1,i,1:n2) = dble(i-1)/dble(n1-1)
	enddo

	do i = 1, n2
	    x(2,1:n1,i) = dble(i-1)/dble(n2-1)
	enddo
    end subroutine set_gridx

    function theory(x,i,j,n1,n2) result(phi)
        integer,intent(in) :: i, j, n1, n2
	real(8),intent(in) :: x(1:2,1:n1,1:n2)
        real(8) :: phi, pi

	pi = 2._8 * acos(0._8)

        phi = (sin(pi * x(1,i,j)) * sinh(pi * (1 - x(2,i,j))))/sinh(pi)
    end function theory
end module subprogs

program main
    use subprogs
    implicit none
    integer,parameter :: n1 = 101, n2 = 101
    real(8) :: x(1:2,1:n1,1:n2)
    integer :: i, j, fo = 11

    open(fo,file = 'mat3d2.d')

    call set_gridx(x,n1,n2)

    do j = 1, n2
        do i = 1, n1
	    write(fo,*) x(1:2,i,j), theory(x,i,j,n1,n2)
	    write(fo,*) ''
	enddo
    enddo

    close(fo)
end program main
