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
end module subprogs

program main
    use subprogs
    implicit none
    integer,parameter :: n = 5
    real(8) :: e(1:n,1:n), t(1:n,1:n), tl(1:n,1:n)
    integer :: i

    call random_seed
    call random_number(t)

    tl = gs(t,n)
    e = matmul(tl,transpose(tl))

    write(*,*) 'A ='
    do i = 1, n
        write(*,'(5e12.4)') t(i,1:n)
    enddo
    write(*,*) 'T ='
    do i = 1, n
        write(*,'(5e12.4)') tl(i,1:n)
    enddo
    write(*,*) 'TT^{T} ='
    do i = 1, n
        write(*,'(5e12.4)') e(i,1:n)
    enddo

end program main
