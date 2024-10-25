module subprogs
    implicit none
contains
    function touhi(a,r,n) result(s)
        real(8),intent(in) :: a, r
	integer :: n
        real(8) :: s
    
        s = (a * (1._8 - (r ** dble(n))))/(1._8 - r)
    end function touhi
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a, r
    integer :: n

    write(*,*) 'input a:'
    read(*,*) a
    write(*,*) 'input r:'
    read(*,*) r
    write(*,*) 'input n:'
    read(*,*) n
    if(n < 1) then
        write(*,*) 'stop n < 1'
	stop
    endif

    write(*,*) 'S =', touhi(a,r,n)
end program main
