module subprogs
    implicit none
contains
    function gaiseki(u,v) result(w)
        real(8),intent(in) :: u(1:3), v(1:3)
	real(8) :: w(1:3)

        w(1) = u(2) * v(3) - u(3) * v(2)
        w(2) = u(3) * v(1) - u(1) * v(3)
        w(3) = u(1) * v(2) - u(2) * v(1)

    end function gaiseki

    function gaiseki2(u,v) result(w)
        real(8),intent(in) :: u(1:3), v(1:3)
	real(8) :: c(1:2,1:3), w(1:3)
        integer :: i

        c(1,1:3) = u(1:3)
	c(2,1:3) = v(1:3)

        do i = 1, 3
	    c = cshift(c,1,2)
	    w(i) = c(1,1) * c(2,2) - c(1,2) * c(2,1)
	enddo

    end function gaiseki2
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: u(1:3), v(1:3)

    call random_seed
    call random_number(u)
    call random_number(v)

    write(*,*) 'u ='
    write(*,'(5e12.4)') u

    write(*,*) 'v ='
    write(*,'(5e12.4)') v

    write(*,*) 'u \times v ='
    write(*,'(5e12.4)') gaiseki(u,v)

    write(*,*) 'u \times v2 ='
    write(*,'(5e12.4)') gaiseki2(u,v)
end program main
