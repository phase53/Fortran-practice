module subprogs
    implicit none
contains
    subroutine taisyo(a,n)
        real(8),intent(in) :: a(n,n)
	integer,intent(in) :: n
        real(8) :: b(n,n), c(n,n), d(n,n)
        integer :: i

	b = transpose(a)
	c = (a + b)/2._8
	d = (a - b)/2._8

        write(*,'(a)',advance = 'no') 'A ='
	do i = 1, n
	    write(*,*) a(i,1:n)
	enddo
        
        write(*,'(a)',advance = 'no') 'A^{T} ='
	do i = 1, n
	    write(*,*) b(i,1:n)
	enddo

	write(*,'(a)',advance = 'no') 'A_1 ='
	do i = 1, n
	    write(*,*) c(i,1:n)
	enddo

        write(*,'(a)',advance = 'no') 'A_2 ='
	do i = 1, n
	    write(*,*) d(i,1:n)
	enddo
    end subroutine taisyo
end module subprogs

program main
    use subprogs
    implicit none
    real(8),allocatable :: a(:,:)
    integer :: n

    write(*,*) 'input n >= 2:'
    read(*,*) n
    if(n < 2) then
        write(*,*) 'stop n < 2'
	stop
    endif
    allocate(a(n,n))

    call random_seed
    call random_number(a(:,:))
    call taisyo(a,n)
end program main
