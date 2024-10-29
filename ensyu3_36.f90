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

    function vecter(a,p) result(v)
        real(8),intent(in) :: a(:,:)
        complex(8),intent(in) :: p
	complex(8) :: v(1:2)
        complex(8) :: al(size(a,1),size(a,2))

        al
	!すみません，未完成です．．．
end module subprogs
