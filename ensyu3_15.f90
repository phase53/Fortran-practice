module subprogs
    implicit none
contains
    subroutine gyo(a)
        real(8) :: a(2,2), b, c(2,2)
        integer :: i

	b = a(1,1) * a(2,2) - a(1,2) * a(2,1)
	if(b == 0) then
	    write(*,*) '|A| = 0'
	    stop
	endif
	c(1,1) = a(2,2)
	c(2,2) = a(1,1)
	c(1,2) = -a(1,2)
	c(2,1) = -a(2,1)
        write(*,*) '|A| =', b
	write(*,'(a)',advance = 'no') 'A^{-1} ='
	do i = 1, 2
	    write(*,*) c(i,1:2)/b
	enddo
    end subroutine gyo
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a(2,2)

    call random_seed
    call random_number(a(:,:))

    call gyo(a)
end program main



