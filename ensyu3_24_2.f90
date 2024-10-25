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
    real(8) :: seki, a(1:3,1:3), b(1:3,1:3)
    integer,parameter :: i = 2
    integer :: j, k

    a(:,:) = 0._8
    call random_seed

    do k = 1, 3
        do j = 1, k
            call random_number(a(j,k))
	enddo
    enddo

    write(*,*) 'A ='
    do j = 1, 3
        write(*,*) a(j,1:3)
    enddo

    seki = 1._8
    do j = 1, 3
        seki = seki * a(j,j)
    enddo

    write(*,*) '|A| =', tenkai(a,i)
    write(*,*) 'Πa(i,i) =', seki

end program main
