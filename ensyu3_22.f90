module subprogs
    implicit none
contains
    function yoinshi(a, i, j) result(c)
        real(8) :: a(1:3,1:3)
        real(8) :: c, b(1:2,1:2)
        integer,intent(in) :: i, j

        b(1:i-1,1:j-1) = a(1:i-1,1:j-1)
        b(1:i-1,j:2) = a(1:i-1,j+1:3)
        b(i:2,1:j-1) = a(i+1:3,1:j-1)
        b(i:2,j:2) = a(i+1:3,j+1:3)
        c = ((-1) ** (i+j)) * (b(1,1) * b(2,2) - b(1,2) * b(2,1))

    end function yoinshi
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a(1:3,1:3)
    integer :: i, j
    call random_seed
    call random_number(a(1:3,1:3))

    write(*,*) 'A =', a
    
    write(*,*) 'input i (1 <= i <= 3)'
    read(*,*) i
    if(i < 1.or.3 < i) then
        write(*,*) 'stop i < i or 3 < i'
	stop
    endif

    write(*,*) 'input j (1 <= i <= 3)'
    read(*,*) j
    if(j < 1.or.3 < j) then
        write(*,*) 'stop j < i or 3 < j'
	stop
    endif

    write(*,*) 'a~ =', yoinshi(a,i,j)
end program main
