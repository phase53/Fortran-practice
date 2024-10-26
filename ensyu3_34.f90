module subprogs
    implicit none
contains
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
    real(8) :: r, a(1:3,1:3), b(1:3,1:3), c(1:3,1:3)
    integer :: i, j
    integer,parameter :: k = 2

    call random_seed
    call random_number(a)

    r = tenkai(a, k)
    if(r == 0._8) then
        write(*,*) 'stop |A| = 0'
	stop
    endif

    do j = 1, 3
        do i = 1, 3
	    b(j,i) = yoinshi(a,i,j)/r
	enddo
    enddo

    write(*,*) 'A ='
    do i = 1, 3
        write(*,'(5e12.4)') a(i,1:3)
    enddo

    write(*,*) 'A^{-1} ='
    do i = 1, 3
        write(*,'(5e12.4)') b(i,1:3)
    enddo

    c = matmul(a,b)
    write(*,*) 'AA^{-1} ='
    do i = 1, 3
        write(*,'(5e12.4)') c(i,1:3)
    enddo
end program main
