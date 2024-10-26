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
    
    function yoinshi(a, i, j) result(c)
        real(8),intent(in) :: a(1:3,1:3)
        real(8) :: c, b(1:2,1:2)
        integer,intent(in) :: i, j

        b(1:i-1,1:j-1) = a(1:i-1,1:j-1)
        b(1:i-1,j:2) = a(1:i-1,j+1:3)
        b(i:2,1:j-1) = a(i+1:3,1:j-1)
        b(i:2,j:2) = a(i+1:3,j+1:3)
        c = ((-1) ** (i+j)) * (b(1,1) * b(2,2) - b(1,2) * b(2,1))
    end function yoinshi

    function tenkai(a, i) result(r)
        real(8),intent(in) :: a(1:3,1:3)
	real(8) :: r
	integer,intent(in) :: i
        integer :: k
	
	r = 0._8
	do k = 1, 3
	    r = r + a(i,k) * yoinshi(a,i,k)
        enddo
    end function tenkai
end module subprogs

program main
    use subprogs
    implicit none
    integer,parameter :: i = 2
    real(8) :: m(1:3,1:3), a(1:3), b(1:3), c(1:3)

    call random_seed
    call random_number(a)
    call random_number(b)
    call random_number(c)

    m(1:3,1) = a(1:3)
    m(1:3,2) = b(1:3)
    m(1:3,3) = c(1:3)

    write(*,*) '|M| =', tenkai(m,i)
    write(*,*) 'a\dot(b\times c) =', scalar(a,b,c)
end program main
