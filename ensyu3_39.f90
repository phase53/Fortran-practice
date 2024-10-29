module subprogs
    implicit none
contains
    function normal_vec2(v,n) result(nv)
        integer,intent(in) :: n
	real(8),intent(in) :: v(n)
	real(8) :: nv(n), vl

        vl = sqrt(dot_product(v,v))
	if(vl == 0._8) then
	    nv(:) = 0._8
	else
	    nv(:) = v(:)/vl
	endif
    end function normal_vec2

    function gs(a,n) result(e)
        integer,intent(in) :: n
	real(8),intent(in) :: a(n,n)
	real(8) :: e(n,n), dotp

	integer :: k, j
	e(1:n,1) = normal_vec2(a(1:n,1:1),n)
	
	do k = 2, n
	    e(1:n,k) = a(1:n,k)
	    do j = 1, k - 1
	        dotp = dot_product(a(1:n,k),e(1:n,j))
		e(1:n,k) = e(1:n,k) - dotp * e(1:n,j)
	    enddo
	    e(1:n,k) = normal_vec2(e(1:n,k:k),n)
	enddo
    end function gs

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
    integer,parameter ::n = 3, j = 2
    real(8) :: a(n,n), b(n,n), c(n,n), t(n,n)
    
    call random_seed
    call random_number(a)
    call random_number(b)

    t = gs(b,n)
    c = matmul(transpose(t),a)
    c = matmul(c,t)

    write(*,*) '|A| =', tenkai(a,j)
    write(*,*) '|C| =', tenkai(c,j)

end program main
