module subprogs
    implicit none
contains
    subroutine kaiten(x,th)
        real(8),intent(in) :: x(2), th
	real(8) :: y(2), zero(2)
	integer :: fo = 11

        open(fo,file = 'output.d')
	y(1) = cos(th) * x(1) - sin(th) * x(2)
        y(2) = sin(th) * x(1) + cos(th) * x(2)
        zero(1:2) = (/0._8,0._8/)

        write(*,*) 'x(x_1,x_2) =', x
	write(*,*) 'y(y_1,y_2) =', y

	!描画用
	write(fo,*) x
	write(fo,*) y
	write(fo,*) ''
	
	write(fo,*) x
	write(fo,*) zero
	write(fo,*) ''
	
	write(fo,*) y
	write(fo,*) zero
	write(fo,*) ''
	close(fo)
    end subroutine kaiten
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: x(2), pi = 2._8 * acos(0._8)

    call random_seed
    call random_number(x)

    x = -2._8 + 4._8 * x

    call kaiten(x,pi/6._8)
end program main
