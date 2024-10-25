module subprogs
    implicit none
contains
    function tr(a,n) result(r)
        real(8),intent(in) :: a(n,n)
	integer,intent(in) :: n
        real(8) :: r
	integer :: i

        r = 0._8
        do i = 1, n
	    r = r + a(i,i)
	enddo
    end function tr
end module subprogs

program main
    use subprogs
    implicit none
    integer,parameter :: n = 3
    real(8) :: a(n,n), b(n,n), ab(n,n), ba(n,n)
    integer :: i

    call random_seed
    call random_number(a)
    call random_number(b)

    ab = matmul(a,b)
    ba = matmul(b,a)

    write(*,*) 'A ='
    do i = 1, n
        write(*,*) a(i,1:n)
    enddo
    write(*,*) 'B ='
    do i = 1, n
        write(*,*) b(i,1:n)
    enddo
    write(*,*) 'tr(AB) =', tr(ab,n)
    write(*,*) 'tr(BA) =', tr(ba,n)

end program main
