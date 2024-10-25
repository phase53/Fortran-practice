module subprogs
    implicit none
contains
    function daen(k,n) result(R)
        real(8),intent(in) :: k
	real(8) a, b, c, R, pi
	integer,intent(in) :: n
        integer :: i, j
    
        pi = 2.0d0 * acos(0.0d0)
        c = 1.0d0
        do i = 2, n
            a = 1.0d0
            b = 1.0d0
            do j = 2, i
                a = a * (((2.0d0 * dble(j - 1)) - 1.0d0) ** 2)
                b = b * ((2.0d0 * dble(j - 1)) ** 2)
            enddo
            c = c + (a/b) * (k ** (2.0d0 * dble(i - 1)))
        enddo
        R = (pi/2.0d0) * c
    end function daen
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: k
    integer :: n

    write(*,*) 'input n'
    read(*,*) n
    if(n < 2) then
        write(*,*) 'input n >= 2'
        stop
    endif
    write(*,*) 'input k'
    read(*,*) k
    if(k >= 1.or.k <= -1) then
        write(*,*) 'input n (0 <= k^2 < 1)'
        stop
    endif

    write(*,*) 'k =', k, 'K(k) =', daen(k,n)
end program main 
