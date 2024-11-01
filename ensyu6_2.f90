module subprogs
    implicit none
contains
    subroutine gauss_jordan(a0,x,b,n)
        integer,intent(in) :: n
	real(8),intent(in) :: a0(n,n), b(n)
	real(8),intent(out) :: x(n)
	integer :: i, k
	real(8) ar, a(n,n)

	a(:,:) = a0(:,:)
	x(:) = b(:)
	do k = 1, n
	    if(a(k,k) == 0._8) then
	        write(*,*) 'stop pivot = 0'
	    endif
	    ar = 1._8/a(k,k)
	    a(k,k) = 1._8
	    a(k,k+1:n) = ar * a(k,k+1:n)
	    x(k) = ar * x(k)
	    
	    do i = 1, n
	        if(i /= k) then
		    a(i,k+1:n) = a(i,k+1:n) - a(i,k) * a(k,k+1:n)
		    x(i) = x(i) -a(i,k) *x(k)
		    a(i,k) = 0._8
		endif
	    enddo
        enddo
    end subroutine gauss_jordan

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
    real(8),allocatable :: a(:,:), b(:), x(:), r(:)
    real(8) :: t1, t2, t3, t4
    integer :: n, i

    call set_random_ab(a,b,x,n)
    call cpu_time(t1)
    call gauss(a,x,b,n)
    call cpu_time(t2)
    call cpu_time(t3)
    call gauss_jordan(a,x,b,n) 
    call cpu_time(t4)

    write(*,*) 'gauss time T1 =', t2 - t1
    write(*,*) 'gauss jordan time T2 =', t4 - t3
    write(*,*) 'T2/T1 =', (t4 - t3)/(t2 - t1)
end program main

