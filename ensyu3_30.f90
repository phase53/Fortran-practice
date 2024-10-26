module subprogs
    implicit none
contains
    function tenchi(a,n) result(at)
        real(8),intent(in) :: a(1:n,1:n)
	integer,intent(in) :: n
	real(8) :: at(1:n,1:n)
        integer :: i, j

	do j = 1, n
	    do i = 1, n
	        at(j,i) = a(i,j)
	    enddo
	enddo

    end function tenchi
end module subprogs

program main
    use subprogs
    implicit none
    integer,parameter :: n = 3
    real(8) :: a(1:n,1:n), at(1:n,1:n), at2(1:n,1:n)
    integer :: i

    call random_seed
    call random_number(a)

    at(1:n,1:n) = tenchi(a,n)
    at2(1:n,1:n) = transpose(a)

    write(*,*) 'original A ='
    do i = 1, n
        write(*,'(5e12.4)') a(i,1:n)
    enddo

    write(*,*) 'A^T ='
    do i = 1, n
        write(*,'(5e12.4)') at(i,1:n)
    enddo

    write(*,*) 'A^T2 ='
    do i = 1, n
        write(*,'(5e12.4)') at2(i,1:n)
    enddo

end program main
