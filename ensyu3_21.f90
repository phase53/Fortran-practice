module subprogs
    implicit none
contains
    function siki(a) result(r)
        real(8),intent(in) :: a(2,2)
	real(8) :: r
	
	r = a(1,1) * a(2,2) - a(1,2) * a(2,1)
    end function siki
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a(1:3), b(1:3), c(1:2,1:3)
    integer :: i

    call random_seed
    call random_number(a(1:3))
    call random_number(b(1:3))

    c(1,1:3) = a(1:3)
    c(2,1:3) = b(1:3)

    write(*,*) 'a =', a
    write(*,*) 'b =', b

    do i = 1, 3
       c = cshift(c, 1, 2)
       write(*,*) 'x_', i, '=', siki(c(1:2,1:2))
    enddo
end program main
