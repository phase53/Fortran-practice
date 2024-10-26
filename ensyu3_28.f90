module subprogs
    implicit none
contains
    function tani(a, n) result(e)
        real(8),intent(in) :: a(1:n,1:n)
	integer,intent(in) :: n
	real(8) :: e(1:n,1:n)
        integer :: i

        e(1:n,1:n) = 0._8
	do i = 1, n
	    e(i,i) = 1._8
	enddo

    end function tani
end module subprogs

program main
    use subprogs
    implicit none
    integer,parameter :: n = 5
    real(8) :: a(1:n,1:n)
    integer :: i

    call random_seed
    call random_number(a)

    a = tani(a,n)
    do i = 1, n
        write(*,'(5e12.4)') a(i,1:n)
    enddo
end program main
