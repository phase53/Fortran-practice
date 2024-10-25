module vec_subprogs
    implicit none
contains
    subroutine allocate_rvec(a,n)
        real(8),allocatable,intent(out) :: a(:)
	integer :: n
	
	write(*,'(a)',advance = 'no') 'input n :'
	read(*,*) n
	if(n < 1.or.n > 100) then
	    write(*,*) 'stop n must be 0 < n < 101'
	    stop
	endif
	allocate(a(n))
	call random_number(a)
	a = -1._8 + 2._8 * a
    end subroutine allocate_rvec

    subroutine print_vec(a,n)
        real(8),intent(in) :: a(n)
	integer,intent(in) :: n
        
	write(*,'(100e12.4)') a
    end subroutine print_vec
end module vec_subprogs

program random_vec
    use vec_subprogs
    implicit none
    real(8),allocatable :: v(:)
    integer :: n

    call allocate_rvec(v,n)
    call print_vec(v,n)
end program random_vec
