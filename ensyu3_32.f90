module subprogs
    implicit none
contains
    function scalar(a,b,c) result(g)
        real(8),intent(in) :: a(1:3), b(1:3), c(1:3)
	real(8) :: p(1:2,1:3), d(1:3), g
        integer :: i

        p(1,1:3) = b(1:3)
	p(2,1:3) = c(1:3)

        do i = 1, 3
	    p = cshift(p,1,2)
	    d(i) = p(1,1) * p(2,2) - p(1,2) * p(2,1)
	enddo

        g = dot_product(a,d)

    end function scalar
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a(1:3), b(1:3), c(1:3)

    call random_seed
    call random_number(a)
    call random_number(b)
    call random_number(c)

    write(*,*) 'a ='
    write(*,'(5e12.4)') a
    write(*,*) 'b ='
    write(*,'(5e12.4)') b
    write(*,*) 'c ='
    write(*,'(5e12.4)') c

    write(*,*) 'a \dot (b \times c) ='
    write(*,'(5e12.4)') scalar(a,b,c)
end program main
