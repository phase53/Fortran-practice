module subprogs
    implicit none
contains
    subroutine toukei(x,n)
        real(8),intent(in) :: x(n)
	integer,intent(in) :: n
	real(8) :: s, b, v, std
        integer :: i

        s = sum(x(:))
	b = s/dble(n)
        v = 0._8
	do i = 1, n
            v = v + (x(i) - b) ** 2
	enddo
        v = v/dble(n)
	std = sqrt(v)
        
	write(*,*) 'original =', x
	write(*,*) 'sum =', s
	write(*,*) 'mean =', b
	write(*,*) 'variance =', v
	write(*,*) 'std =', std
    end subroutine toukei
end module subprogs

program main
    use subprogs
    implicit none
    real(8),allocatable :: x(:)
    integer :: n

    write(*,'(a)',advance = 'no') 'input n:'
    read(*,*) n
    if(n < 2) then
        write(*,*) 'stop n must be bigger than 2'
	stop
    endif
    allocate(x(n))
    call random_seed
    call random_number(x(:))

    call toukei(x,n)
end program main
