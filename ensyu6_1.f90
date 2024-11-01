module subprogs
    implicit none
contains
    subroutine gauss(a0,x,b0,n)
        integer,intent(in) :: n
        real(8),intent(in) :: a0(n,n), b0(n)
        real(8),intent(out) :: x(n)
        integer :: i, k
        real(8) :: ar, s, a(n,n), b(n)

        a(:,:) = a0(:,:)
        b(:) = b0(:)
        do k = 1, n
            if(a(k,k) == 0._8) then
	        write(*,*) 'stop some tr = 0'
	        stop
	    endif
	    ar = 1._8/a(k,k)
	    a(k,k) = 1._8
	    a(k,k+1:n) = ar * a(k,k+1:n)
	    b(k) = ar * b(k)
            if(k < n) then
	        do i = k + 1, n
	            a(i,k+1:n) = a(i,k+1:n) - a(i,k) * a(k,k+1:n)
	            b(i) = b(i) - a(i,k) * b(k)
	            a(i,k) = 0._8
	        enddo
	    endif
        enddo
	do i = n, 1, -1
	    if(i + 1 <= n) then
	        s = 0._8
	        do k = n, i + 1, -1
	            s = s + a(i,k) * x(k)
                enddo
                x(i) = b(i) - s
	    else
	        x(i) = b(i)
            endif
	enddo
    end subroutine gauss

    subroutine set_random_ab(a,b,x,n)
        real(8),allocatable,intent(out) :: a(:,:), b(:), x(:)
	integer,intent(inout) :: n
	write(*,*) 'input n:'
	read(*,*) n
	if(n < 1.or.n > 100) then
	    write(*,*) 'stop n must be 1 <= n <= 100'
	    stop
	endif
	allocate(a(1:n,1:n),b(1:n),x(1:n))
        call random_seed
	call random_number(a)
        call random_number(b)

    end subroutine set_random_ab
end module subprogs

program main
    use subprogs
    implicit none
    real(8),allocatable :: a0(:,:), b0(:), x(:), r(:)
    integer :: n, i

    call set_random_ab(a0,b0,x,n)
    call gauss(a0,x,b0,n)

    write(*,*) 'b ='
    write(*,'(5e12.4)') b0
    write(*,*) 'A ='
    do i = 1, n
        write(*,'(5e12.4)') a0(i,1:n)
    enddo

    write(*,*) 'x ='
    write(*,'(5e12.4)') x

    allocate(r(n))
    r(:) = b0(:) - matmul(a0,x) 
    write(*,*) 'error =', dot_product(r,r)
end program main
