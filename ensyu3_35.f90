module subprogs
    implicit none
contains
    function eval2x2mat(a) result(eval)
        real(8),intent(in) :: a(:,:)
	complex(8) :: eval(2)
	real(8) :: b, c, d, e
	if(size(a,1) /= size(a,2)) then
	    write(*,*) 'stop not square'
	    stop
	endif
	if(size(a,1) /= 2) then
	    write(*,*) 'stop not 2x2 matrix'
	    stop
	endif

	b = -0.5d0 * (a(1,1) + a(2,2))
	c = a(1,1) * a(2,2) - a(1,2) * a(2,1)
	d = b ** 2 - c

	if(d < 0._8) then
	    eval(1) = cmplx(-b, sqrt(-d), kind = 8)
	    eval(2) = conjg(eval(1))
	else if(d > 0._8) then
	    e = -b + sign(sqrt(d), -b)
	    eval(1) = cmplx(e, 0._8, kind = 8)
	    eval(2) = cmplx(c/e, 0._8, kind = 8)
	else
	    eval(1) = cmplx(-b, 0._8, kind = 8)
	    eval(2) = eval(1)
	endif
    end function eval2x2mat
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a(1:2,1:2)
    complex(8) :: e(1:2)
    integer :: i

    call random_seed
    call random_number(a)
    a(1:2,1:2) = -5._8 * 10._8 * a(1:2,1:2)

    e(1:2) = eval2x2mat(a)
    do i = 1, 2
        write(*,*) '\lambda_', i, '=', e(i)
    enddo
end program main


