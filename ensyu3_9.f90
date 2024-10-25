module subprogs
    implicit none
contains
    function seiki(a,b,n) result(s)
        real(8),intent(in) :: a, b
        real(8) :: x, y, dx, pi, s
        integer,intent(in) :: n
	integer :: i

        s = 0.0d0
        pi = 2.0d0 * acos(0.0d0)
        dx = dble(b - a)/dble(n)
        do i = 0, n
            x = dble(i) * dx + a
            y = exp(-0.5d0 * (x ** 2))/dble(sqrt(2.0d0 * pi))
            if(i == 1.or.i == n) then
                y = y * 0.5d0
            endif
            s = s + y
        enddo
    
        s = s * dx
    end function seiki
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a, b
    integer :: n

    write(*,*) 'input n'
    read(*,*) n
    if(n <= 0) then
        write(*,*) 'stop input posotive n'
        stop
    endif

    write(*,*) 'input a'
    read(*,*) a
    write(*,*) 'input b'
    read(*,*) b
    if(b < a) then
        write(*,*) 'stop b must be bigger than a'
        stop
    endif

    write(*,*) 's =', seiki(a,b,n)
end program main
