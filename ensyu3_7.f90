module subprogs
    implicit none
contains
    subroutine cub(x,y,z,v,s)
        real(8),intent(in) :: x, y, z
        real(8),intent(out) :: v, s

        s = 2._8 * (x * y + y * z + z * x)
	v = x * y * z
    end subroutine cub
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: x, y, z, v, s

    write(*,*) 'input x'
    read(*,*) x
    if(x < 0) then
        write(*,*) 'stop x is negative'
	stop
    endif
    write(*,*) 'input y'
    read(*,*) y
    if(y < 0) then
        write(*,*) 'stop y is negative'
	stop
    endif
    write(*,*) 'input z'
    read(*,*) z
    if(z < 0) then
        write(*,*) 'stop z is negative'
	stop
    endif

    call cub(x,y,z,v,s)

    write(*,*) 'S =', s, '[L^2]'
    write(*,*) 'V =', v ,'[L^3]'
end program main
